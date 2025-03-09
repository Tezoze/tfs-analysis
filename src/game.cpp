/**
 * The Forgotten Server - a free and open-source MMORPG server emulator
 * Copyright (C) 2019  Mark Samman <mark.samman@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "otpch.h"
#include "pugicast.h"
#include "actions.h"
#include "bed.h"
#include "configmanager.h"
#include "creature.h"
#include "creatureevent.h"
#include "databasetasks.h"
#include "events.h"
#include "game.h"
#include "globalevent.h"
#include "iologindata.h"
#include "items.h"
#include "monster.h"
#include "movement.h"
#include "scheduler.h"
#include "server.h"
#include "spells.h"
#include "talkaction.h"
#include "weapons.h"
#include "script.h"
#include <fmt/format.h>
#include <mutex>
#include <optional>

extern ConfigManager g_config;
extern Actions* g_actions;
extern Chat* g_chat;
extern TalkActions* g_talkActions;
extern Spells* g_spells;
extern Vocations g_vocations;
extern GlobalEvents* g_globalEvents;
extern CreatureEvents* g_creatureEvents;
extern Events* g_events;
extern Monsters g_monsters;
extern MoveEvents* g_moveEvents;
extern Weapons* g_weapons;
extern Scripts* g_scripts;

namespace {
	inline std::mutex gameMutex; // Protects players, monsters, npcs, guilds
	inline std::mutex decayMutex; // Protects decayItems and toDecayItems
}

Game::~Game() {
	std::lock_guard<std::mutex> lock(gameMutex);
	for (auto& it : guilds) {
		delete it.second; // TODO: Consider std::unique_ptr for guilds
	}
}
void Game::start(ServiceManager* manager) {
	std::lock_guard<std::mutex> lock(gameMutex);
	serviceManager = manager;
	updateWorldTime();

	if (g_config.getBoolean(ConfigManager::DEFAULT_WORLD_LIGHT)) {
		g_scheduler.addEvent(createSchedulerTask(EVENT_LIGHTINTERVAL, [this]() { checkLight(); }));
	}
	g_scheduler.addEvent(createSchedulerTask(EVENT_CREATURE_THINK_INTERVAL, [this]() { checkCreatures(0); }));
	g_scheduler.addEvent(createSchedulerTask(EVENT_DECAYINTERVAL, [this]() { checkDecay(); }));
}

GameState_t Game::getGameState() const {
	std::lock_guard<std::mutex> lock(gameMutex);
	return gameState;
}

void Game::setWorldType(WorldType_t type) {
	std::lock_guard<std::mutex> lock(gameMutex);
	worldType = type;
}

void Game::setGameState(GameState_t newState) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (gameState == GAME_STATE_SHUTDOWN || gameState == newState) {
		return;
	}
	gameState = newState;

	switch (newState) {
	case GAME_STATE_INIT:
		groups.load();
		g_chat->load();
		map.spawns.startup();
		raids.loadFromXml();
		raids.startup();
		quests.loadFromXml();
		loadMotdNum();
		loadPlayersRecord();
		loadAccountStorageValues();
		g_globalEvents->startup();
		break;

	case GAME_STATE_SHUTDOWN:
		g_globalEvents->execute(GLOBALEVENT_SHUTDOWN);
		for (auto it = players.begin(); it != players.end();) {
			it->second->kickPlayer(true);
			it = players.begin(); // Reset iterator after kick
		}
		saveMotdNum();
		saveGameState();
		g_dispatcher.addTask([this]() { shutdown(); });
		g_scheduler.stop();
		g_databaseTasks.stop();
		g_dispatcher.stop();
		break;

	case GAME_STATE_CLOSED:
		for (auto it = players.begin(); it != players.end();) {
			if (!it->second->hasFlag(PlayerFlag_CanAlwaysLogin)) {
				it->second->kickPlayer(true);
				it = players.begin();
			}
			else {
				++it;
			}
		}
		saveGameState();
		break;

	default:
		break;
	}
}

void Game::saveGameState() {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (gameState == GAME_STATE_NORMAL) {
		gameState = GAME_STATE_MAINTAIN;
	}

	std::cout << "Saving server..." << std::endl;
	if (!saveAccountStorageValues()) {
		std::cout << "[Error - Game::saveGameState] Failed to save account storage values." << std::endl;
	}

	for (const auto& [id, player] : players) {
		player->loginPosition = player->getPosition();
		IOLoginData::savePlayer(player);
	}
	Map::save();
	g_databaseTasks.flush();

	if (gameState == GAME_STATE_MAINTAIN) {
		gameState = GAME_STATE_NORMAL;
	}
}

bool Game::loadMainMap(const std::string& filename) {
	return map.loadMap("data/world/" + filename + ".otbm", true);
}

void Game::loadMap(const std::string& path) {
	map.loadMap(path, false);
}
std::shared_ptr<Cylinder> Game::internalGetCylinder(Player* player, const Position& pos) const {
	if (pos.x != 0xFFFF) {
		return map.getTile(pos);
	}
	return (pos.y & 0x40) ? player->getContainerByID(pos.y & 0x0F) : player;
}

Thing* Game::internalGetThing(Player* player, const Position& pos, int32_t index, uint32_t spriteId, stackPosType_t type) const {
	if (pos.x != 0xFFFF) {
		auto tile = map.getTile(pos);
		if (!tile) return nullptr;

		Thing* thing = nullptr;
		switch (type) {
		case STACKPOS_LOOK: thing = tile->getTopVisibleThing(player); break;
		case STACKPOS_MOVE: {
			auto item = tile->getTopDownItem();
			thing = (item && item->isMoveable()) ? item : tile->getTopVisibleCreature(player);
			break;
		}
		case STACKPOS_USEITEM: thing = tile->getUseItem(index); break;
		case STACKPOS_TOPDOWN_ITEM: thing = tile->getTopDownItem(); break;
		case STACKPOS_USETARGET: {
			thing = tile->getTopVisibleCreature(player);
			if (!thing) thing = tile->getUseItem(index);
			break;
		}
		default: break;
		}

		if (player && thing && thing->getItem() && tile->hasFlag(TILESTATE_SUPPORTS_HANGABLE)) {
			if (tile->hasProperty(CONST_PROP_ISVERTICAL)) {
				if (player->getPosition().x + 1 == tile->getPosition().x) return nullptr;
			}
			else if (player->getPosition().y + 1 == tile->getPosition().y) {
				return nullptr;
			}
		}
		return thing;
	}

	if (pos.y & 0x40) {
		uint8_t fromCid = pos.y & 0x0F;
		auto container = player->getContainerByID(fromCid);
		if (!container) return nullptr;
		return container->getItemByIndex(player->getContainerIndex(fromCid) + pos.z);
	}
	else if (pos.y == 0 && pos.z == 0) {
		const auto& it = Item::items.getItemIdByClientId(spriteId);
		if (it.id == 0) return nullptr;
		int32_t subType = (it.isFluidContainer() && index < static_cast<int32_t>(sizeof(reverseFluidMap))) ? reverseFluidMap[index] : -1;
		return findItemOfType(player, it.id, true, subType);
	}
	return player->getInventoryItem(static_cast<slots_t>(pos.y));
}

void Game::internalGetPosition(Item* item, Position& pos, uint8_t& stackpos) {
	pos = { 0, 0, 0 };
	stackpos = 0;
	if (auto topParent = item->getTopParent()) {
		if (auto player = dynamic_cast<Player*>(topParent)) {
			pos.x = 0xFFFF;
			if (auto container = dynamic_cast<Container*>(item->getParent())) {
				pos.y = static_cast<uint16_t>(0x40) | player->getContainerID(container);
				pos.z = container->getThingIndex(item);
				stackpos = pos.z;
			}
			else {
				pos.y = player->getThingIndex(item);
				stackpos = pos.y;
			}
		}
		else if (auto tile = topParent->getTile()) {
			pos = tile->getPosition();
			stackpos = tile->getThingIndex(item);
		}
	}
}

Creature* Game::getCreatureByID(uint32_t id) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (id <= Player::playerAutoID) return getPlayerByID(id);
	if (id <= Monster::monsterAutoID) return getMonsterByID(id);
	if (id <= Npc::npcAutoID) return getNpcByID(id);
	return nullptr;
}

Monster* Game::getMonsterByID(uint32_t id) {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto it = monsters.find(id);
	return (it != monsters.end()) ? it->second : nullptr;
}

Npc* Game::getNpcByID(uint32_t id) {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto it = npcs.find(id);
	return (it != npcs.end()) ? it->second : nullptr;
}

Player* Game::getPlayerByID(uint32_t id) {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto it = players.find(id);
	return (it != players.end()) ? it->second : nullptr;
}

Creature* Game::getCreatureByName(const std::string& s) const {
	if (s.empty()) return nullptr;
	std::string lowerCaseName = asLowerCaseString(s);
	std::lock_guard<std::mutex> lock(gameMutex);

	auto it = mappedPlayerNames.find(lowerCaseName);
	if (it != mappedPlayerNames.end()) return it->second;

	auto equalName = [&lowerCaseName](const auto& pair) {
		auto name = pair.second->getName();
		return lowerCaseName.size() == name.size() && std::equal(lowerCaseName.begin(), lowerCaseName.end(), name.begin(), [](char a, char b) {
			return a == std::tolower(b);
			});
		};
	it = std::find_if(npcs.begin(), npcs.end(), equalName);
	if (it != npcs.end()) return it->second;
	it = std::find_if(monsters.begin(), monsters.end(), equalName);
	return (it != monsters.end()) ? it->second : nullptr;
}

bool Game::internalPlaceCreature(Creature* creature, const Position& pos, bool extendedPos, bool forced) {
	if (creature->getParent()) return false;
	if (!map.placeCreature(pos, creature, extendedPos, forced)) return false;

	creature->incrementReferenceCounter();
	creature->setID();
	creature->addList();
	return true;
}

bool Game::placeCreature(Creature* creature, const Position& pos, bool extendedPos, bool forced) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (!internalPlaceCreature(creature, pos, extendedPos, forced)) return false;

	SpectatorVec spectators;
	map.getSpectators(spectators, creature->getPosition(), true);
	spectators.reserve(spectators.size());
	for (auto* spectator : spectators) {
		if (auto tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendCreatureAppear(creature, creature->getPosition(), true);
		}
	}
	for (auto* spectator : spectators) {
		spectator->onCreatureAppear(creature, true);
	}
	creature->getParent()->postAddNotification(creature, nullptr, 0);
	addCreatureCheck(creature);
	creature->onPlacedCreature();
	return true;
}

bool Game::removeCreature(Creature* creature, bool isLogout) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (creature->isRemoved()) return false;

	Tile* tile = creature->getTile();
	std::vector<int32_t> oldStackPosVector;
	SpectatorVec spectators;
	map.getSpectators(spectators, tile->getPosition(), true);
	oldStackPosVector.reserve(spectators.size());
	for (auto* spectator : spectators) {
		if (auto player = spectator->getPlayer()) {
			oldStackPosVector.push_back(player->canSeeCreature(creature) ? tile->getClientIndexOfCreature(player, creature) : -1);
		}
	}

	tile->removeCreature(creature);
	const Position& tilePos = tile->getPosition();
	size_t i = 0;
	for (auto* spectator : spectators) {
		if (auto player = spectator->getPlayer()) {
			if (player->canSeeCreature(creature)) {
				player->sendRemoveTileCreature(creature, tilePos, oldStackPosVector[i++]);
			}
		}
	}
	for (auto* spectator : spectators) {
		spectator->onRemoveCreature(creature, isLogout);
	}

	creature->getParent()->postRemoveNotification(creature, nullptr, 0);
	creature->removeList();
	creature->setRemoved();
	ReleaseCreature(creature);
	removeCreatureCheck(creature);

	for (auto* summon : creature->summons) {
		summon->setSkillLoss(false);
		removeCreature(summon);
	}
	return true;
}
void Game::executeDeath(uint32_t creatureId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Creature* creature = getCreatureByID(creatureId);
	if (creature && !creature->isRemoved()) {
		creature->onDeath();
	}
}

void Game::playerMoveThing(uint32_t playerId, const Position& fromPos, uint16_t spriteId, uint8_t fromStackPos, const Position& toPos, uint8_t count) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	uint8_t fromIndex = (fromPos.x == 0xFFFF) ? ((fromPos.y & 0x40) ? fromPos.z : static_cast<uint8_t>(fromPos.y)) : fromStackPos;
	Thing* thing = internalGetThing(player, fromPos, fromIndex, 0, STACKPOS_MOVE);
	if (!thing) {
		player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
		return;
	}

	if (auto movingCreature = thing->getCreature()) {
		Tile* tile = map.getTile(toPos);
		if (!tile) {
			player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
			return;
		}
		if (Position::areInRange<1, 1, 0>(movingCreature->getPosition(), player->getPosition())) {
			auto task = createSchedulerTask(MOVE_CREATURE_INTERVAL, [this, playerId, movingCreatureId = movingCreature->getID(), from = movingCreature->getPosition(), to = tile->getPosition()]() {
				playerMoveCreatureByID(playerId, movingCreatureId, from, to);
				});
			player->setNextActionTask(task);
		}
		else {
			playerMoveCreature(player, movingCreature, movingCreature->getPosition(), tile);
		}
	}
	else if (auto item = thing->getItem()) {
		auto toCylinder = internalGetCylinder(player, toPos);
		if (!toCylinder) {
			player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
			return;
		}
		playerMoveItem(player, fromPos, spriteId, fromStackPos, toPos, count, item, toCylinder);
	}
}

void Game::playerMoveCreatureByID(uint32_t playerId, uint32_t movingCreatureId, const Position& movingCreatureOrigPos, const Position& toPos) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	Creature* movingCreature = getCreatureByID(movingCreatureId);
	if (!movingCreature) return;

	Tile* toTile = map.getTile(toPos);
	if (!toTile) {
		player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
		return;
	}
	playerMoveCreature(player, movingCreature, movingCreatureOrigPos, toTile);
}

void Game::playerMoveCreature(Player* player, Creature* movingCreature, const Position& movingCreatureOrigPos, Tile* toTile) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (!player->canDoAction()) {
		uint32_t delay = player->getNextActionTime();
		auto task = createSchedulerTask(delay, [this, playerId = player->getID(), movingCreatureId = movingCreature->getID(), from = movingCreatureOrigPos, to = toTile->getPosition()]() {
			playerMoveCreatureByID(playerId, movingCreatureId, from, to);
			});
		player->setNextActionTask(task);
		return;
	}

	if (movingCreature->isMovementBlocked()) {
		player->sendCancelMessage(RETURNVALUE_NOTMOVEABLE);
		return;
	}

	player->setNextActionTask(nullptr);

	if (!Position::areInRange<1, 1, 0>(movingCreatureOrigPos, player->getPosition())) {
		std::vector<Direction> listDir;
		if (player->getPathTo(movingCreatureOrigPos, listDir, 0, 1, true, true)) {
			g_dispatcher.addTask([this, playerId = player->getID(), listDir = std::move(listDir)]() mutable {
				playerAutoWalk(playerId, std::move(listDir));
				});
			auto task = createSchedulerTask(RANGE_MOVE_CREATURE_INTERVAL, [this, playerId = player->getID(), movingCreatureId = movingCreature->getID(), from = movingCreatureOrigPos, to = toTile->getPosition()]() {
				playerMoveCreatureByID(playerId, movingCreatureId, from, to);
				});
			player->setNextWalkActionTask(task);
		}
		else {
			player->sendCancelMessage(RETURNVALUE_THEREISNOWAY);
		}
		return;
	}

	if ((!movingCreature->isPushable() && !player->hasFlag(PlayerFlag_CanPushAllCreatures)) ||
		(movingCreature->isInGhostMode() && !player->canSeeGhostMode(movingCreature))) {
		player->sendCancelMessage(RETURNVALUE_NOTMOVEABLE);
		return;
	}

	const Position& movingCreaturePos = movingCreature->getPosition();
	const Position& toPos = toTile->getPosition();
	int32_t throwRange = movingCreature->getThrowRange();
	if (Position::getDistanceX(movingCreaturePos, toPos) > throwRange || Position::getDistanceY(movingCreaturePos, toPos) > throwRange || Position::getDistanceZ(movingCreaturePos, toPos) * 4 > throwRange) {
		player->sendCancelMessage(RETURNVALUE_DESTINATIONOUTOFREACH);
		return;
	}

	if (player != movingCreature) {
		if (toTile->hasFlag(TILESTATE_BLOCKPATH)) {
			player->sendCancelMessage(RETURNVALUE_NOTENOUGHROOM);
			return;
		}
		if ((movingCreature->getZone() == ZONE_PROTECTION && !toTile->hasFlag(TILESTATE_PROTECTIONZONE)) ||
			(movingCreature->getZone() == ZONE_NOPVP && !toTile->hasFlag(TILESTATE_NOPVPZONE))) {
			player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
			return;
		}
		if (auto tileCreatures = toTile->getCreatures()) {
			for (auto* tileCreature : *tileCreatures) {
				if (!tileCreature->isInGhostMode()) {
					player->sendCancelMessage(RETURNVALUE_NOTENOUGHROOM);
					return;
				}
			}
		}
		if (auto movingNpc = movingCreature->getNpc()) {
			if (!Spawns::isInZone(movingNpc->getMasterPos(), movingNpc->getMasterRadius(), toPos)) {
				player->sendCancelMessage(RETURNVALUE_NOTENOUGHROOM);
				return;
			}
		}
	}

	if (!g_events->eventPlayerOnMoveCreature(player, movingCreature, movingCreaturePos, toPos)) {
		return;
	}

	ReturnValue ret = internalMoveCreature(*movingCreature, *toTile);
	if (ret != RETURNVALUE_NOERROR) {
		player->sendCancelMessage(ret);
	}
}

ReturnValue Game::internalMoveCreature(Creature& creature, Direction direction, uint32_t flags) {
	std::lock_guard<std::mutex> lock(gameMutex);
	creature.setLastPosition(creature.getPosition());
	Position currentPos = creature.getPosition();
	Position destPos = getNextPosition(direction, currentPos);
	Player* player = creature.getPlayer();

	bool diagonalMovement = (direction & DIRECTION_DIAGONAL_MASK) != 0;
	if (player && !diagonalMovement) {
		if (currentPos.z != 8 && creature.getTile()->hasHeight(3)) {
			Tile* tmpTile = map.getTile(currentPos.x, currentPos.y, currentPos.getZ() - 1);
			if (!tmpTile || (!tmpTile->getGround() && !tmpTile->hasFlag(TILESTATE_BLOCKSOLID))) {
				tmpTile = map.getTile(destPos.x, destPos.y, destPos.getZ() - 1);
				if (tmpTile && tmpTile->getGround() && !tmpTile->hasFlag(TILESTATE_IMMOVABLEBLOCKSOLID)) {
					flags |= FLAG_IGNOREBLOCKITEM | FLAG_IGNOREBLOCKCREATURE;
					if (!tmpTile->hasFlag(TILESTATE_FLOORCHANGE)) {
						player->setDirection(direction);
						destPos.z--;
					}
				}
			}
		}
		if (currentPos.z != 7 && currentPos.z == destPos.z) {
			Tile* tmpTile = map.getTile(destPos.x, destPos.y, destPos.z);
			if (!tmpTile || (!tmpTile->getGround() && !tmpTile->hasFlag(TILESTATE_BLOCKSOLID))) {
				tmpTile = map.getTile(destPos.x, destPos.y, destPos.z + 1);
				if (tmpTile && tmpTile->hasHeight(3) && !tmpTile->hasFlag(TILESTATE_IMMOVABLEBLOCKSOLID)) {
					flags |= FLAG_IGNOREBLOCKITEM | FLAG_IGNOREBLOCKCREATURE;
					player->setDirection(direction);
					destPos.z++;
				}
			}
		}
	}

	Tile* toTile = map.getTile(destPos);
	if (!toTile) return RETURNVALUE_NOTPOSSIBLE;
	return internalMoveCreature(creature, *toTile, flags);
}

ReturnValue Game::internalMoveCreature(Creature& creature, Tile& toTile, uint32_t flags) {
	std::lock_guard<std::mutex> lock(gameMutex);
	ReturnValue ret = toTile.queryAdd(0, creature, 1, flags);
	if (ret != RETURNVALUE_NOERROR) return ret;

	map.moveCreature(creature, toTile);
	if (creature.getParent() != &toTile) return RETURNVALUE_NOERROR;

	int32_t index = 0;
	Item* toItem = nullptr;
	Tile* subCylinder = nullptr;
	Tile* toCylinder = &toTile;
	Tile* fromCylinder = nullptr;
	uint32_t n = 0;

	while ((subCylinder = toCylinder->queryDestination(index, creature, &toItem, flags)) != toCylinder) {
		map.moveCreature(creature, *subCylinder);
		if (creature.getParent() != subCylinder) {
			fromCylinder = nullptr;
			break;
		}
		fromCylinder = toCylinder;
		toCylinder = subCylinder;
		flags = 0;
		if (++n >= MAP_MAX_LAYERS) break;
	}

	if (fromCylinder) {
		const Position& fromPos = fromCylinder->getPosition();
		const Position& toPos = toCylinder->getPosition();
		if (fromPos.z != toPos.z && (fromPos.x != toPos.x || fromPos.y != toPos.y)) {
			Direction dir = getDirectionTo(fromPos, toPos);
			if ((dir & DIRECTION_DIAGONAL_MASK) == 0) {
				internalCreatureTurn(&creature, dir);
			}
		}
	}
	return RETURNVALUE_NOERROR;
}

void Game::playerMoveItemByPlayerID(uint32_t playerId, const Position& fromPos, uint16_t spriteId, uint8_t fromStackPos, const Position& toPos, uint8_t count) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;
	playerMoveItem(player, fromPos, spriteId, fromStackPos, toPos, count, nullptr, nullptr);
}
void Game::playerMoveItem(Player* player, const Position& fromPos, uint16_t spriteId, uint8_t fromStackPos, const Position& toPos, uint8_t count, Item* item, Cylinder* toCylinder) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (!player->canDoAction()) {
		uint32_t delay = player->getNextActionTime();
		auto task = createSchedulerTask(delay, [this, playerId = player->getID(), fromPos, spriteId, fromStackPos, toPos, count]() {
			playerMoveItemByPlayerID(playerId, fromPos, spriteId, fromStackPos, toPos, count);
			});
		player->setNextActionTask(task);
		return;
	}

	player->setNextActionTask(nullptr);

	if (!item) {
		uint8_t fromIndex = (fromPos.x == 0xFFFF) ? ((fromPos.y & 0x40) ? fromPos.z : static_cast<uint8_t>(fromPos.y)) : fromStackPos;
		Thing* thing = internalGetThing(player, fromPos, fromIndex, 0, STACKPOS_MOVE);
		if (!thing || !(item = thing->getItem())) {
			player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
			return;
		}
	}

	if (item->getClientID() != spriteId) {
		player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
		return;
	}

	Cylinder* fromCylinder = internalGetCylinder(player, fromPos);
	if (!fromCylinder) {
		player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
		return;
	}

	if (!toCylinder) {
		toCylinder = internalGetCylinder(player, toPos);
		if (!toCylinder) {
			player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
			return;
		}
	}

	if (!item->isPushable() || item->hasAttribute(ITEM_ATTRIBUTE_UNIQUEID)) {
		player->sendCancelMessage(RETURNVALUE_NOTMOVEABLE);
		return;
	}

	const Position& playerPos = player->getPosition();
	const Position& mapFromPos = fromCylinder->getTile()->getPosition();
	if (playerPos.z != mapFromPos.z) {
		player->sendCancelMessage(playerPos.z > mapFromPos.z ? RETURNVALUE_FIRSTGOUPSTAIRS : RETURNVALUE_FIRSTGODOWNSTAIRS);
		return;
	}

	if (!Position::areInRange<1, 1>(playerPos, mapFromPos)) {
		std::vector<Direction> listDir;
		if (player->getPathTo(item->getPosition(), listDir, 0, 1, true, true)) {
			g_dispatcher.addTask([this, playerId = player->getID(), listDir = std::move(listDir)]() mutable {
				playerAutoWalk(playerId, std::move(listDir));
				});
			auto task = createSchedulerTask(RANGE_MOVE_ITEM_INTERVAL, [this, playerId = player->getID(), fromPos, spriteId, fromStackPos, toPos, count]() {
				playerMoveItemByPlayerID(playerId, fromPos, spriteId, fromStackPos, toPos, count);
				});
			player->setNextWalkActionTask(task);
		}
		else {
			player->sendCancelMessage(RETURNVALUE_THEREISNOWAY);
		}
		return;
	}

	const Tile* toCylinderTile = toCylinder->getTile();
	const Position& mapToPos = toCylinderTile->getPosition();

	if (item->isHangable() && toCylinderTile->hasFlag(TILESTATE_SUPPORTS_HANGABLE)) {
		bool vertical = toCylinderTile->hasProperty(CONST_PROP_ISVERTICAL);
		if (vertical && playerPos.x + 1 == mapToPos.x) {
			player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
			return;
		}
		else if (!vertical && playerPos.y + 1 == mapToPos.y) {
			player->sendCancelMessage(RETURNVALUE_NOTPOSSIBLE);
			return;
		}

		if (!Position::areInRange<1, 1, 0>(playerPos, mapToPos)) {
			Position walkPos = mapToPos;
			if (vertical) walkPos.x++;
			else walkPos.y++;

			Position itemPos = fromPos;
			uint8_t itemStackPos = fromStackPos;

			if (fromPos.x != 0xFFFF && Position::areInRange<1, 1>(mapFromPos, playerPos) && !Position::areInRange<1, 1, 0>(mapFromPos, walkPos)) {
				Item* moveItem = nullptr;
				ReturnValue ret = internalMoveItem(fromCylinder, player, INDEX_WHEREEVER, item, count, &moveItem, 0, player, nullptr, &fromPos, &toPos);
				if (ret != RETURNVALUE_NOERROR) {
					player->sendCancelMessage(ret);
					return;
				}
				internalGetPosition(moveItem, itemPos, itemStackPos);
			}

			std::vector<Direction> listDir;
			if (player->getPathTo(walkPos, listDir, 0, 0, true, true)) {
				g_dispatcher.addTask([this, playerId = player->getID(), listDir = std::move(listDir)]() mutable {
					playerAutoWalk(playerId, std::move(listDir));
					});
				auto task = createSchedulerTask(RANGE_MOVE_ITEM_INTERVAL, [this, playerId = player->getID(), itemPos, spriteId, itemStackPos, toPos, count]() {
					playerMoveItemByPlayerID(playerId, itemPos, spriteId, itemStackPos, toPos, count);
					});
				player->setNextWalkActionTask(task);
			}
			else {
				player->sendCancelMessage(RETURNVALUE_THEREISNOWAY);
			}
			return;
		}
	}

	int32_t throwRange = item->getThrowRange();
	if (!item->isPickupable() && playerPos.z != mapToPos.z) {
		player->sendCancelMessage(RETURNVALUE_DESTINATIONOUTOFREACH);
		return;
	}
	if (Position::getDistanceX(playerPos, mapToPos) > throwRange || Position::getDistanceY(playerPos, mapToPos) > throwRange) {
		player->sendCancelMessage(RETURNVALUE_DESTINATIONOUTOFREACH);
		return;
	}

	if (!canThrowObjectTo(mapFromPos, mapToPos, true, false, throwRange, throwRange)) {
		player->sendCancelMessage(RETURNVALUE_CANNOTTHROW);
		return;
	}

	uint8_t toIndex = (toPos.x == 0xFFFF) ? ((toPos.y & 0x40) ? toPos.z : static_cast<uint8_t>(toPos.y)) : 0;
	ReturnValue ret = internalMoveItem(fromCylinder, toCylinder, toIndex, item, count, nullptr, 0, player, nullptr, &fromPos, &toPos);
	if (ret != RETURNVALUE_NOERROR) {
		player->sendCancelMessage(ret);
	}
}

ReturnValue Game::internalMoveItem(Cylinder* fromCylinder, Cylinder* toCylinder, int32_t index, Item* item, uint32_t count, Item** _moveItem, uint32_t flags, Creature* actor, Item* tradeItem, const Position* fromPos, const Position* toPos) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* actorPlayer = actor ? actor->getPlayer() : nullptr;
	if (actorPlayer && fromPos && toPos && !g_events->eventPlayerOnMoveItem(actorPlayer, item, count, *fromPos, *toPos, fromCylinder, toCylinder)) {
		return RETURNVALUE_NOTPOSSIBLE;
	}

	Item* toItem = nullptr;
	Cylinder* subCylinder;
	int floorN = 0;
	while ((subCylinder = toCylinder->queryDestination(index, *item, &toItem, flags)) != toCylinder) {
		toCylinder = subCylinder;
		flags = 0;
		if (++floorN >= MAP_MAX_LAYERS) break;
	}

	if (item == toItem) return RETURNVALUE_NOERROR;
	ReturnValue ret = toCylinder->queryAdd(index, *item, count, flags, actor);
	if (ret == RETURNVALUE_NEEDEXCHANGE) {
		ret = fromCylinder->queryAdd(fromCylinder->getThingIndex(item), *toItem, toItem->getItemCount(), 0);
		if (ret == RETURNVALUE_NOERROR) {
			if (actorPlayer && fromPos && toPos && !g_events->eventPlayerOnMoveItem(actorPlayer, toItem, toItem->getItemCount(), *toPos, *fromPos, toCylinder, fromCylinder)) {
				return RETURNVALUE_NOTPOSSIBLE;
			}
			uint32_t maxExchangeQueryCount = 0;
			ReturnValue retExchangeMaxCount = fromCylinder->queryMaxCount(INDEX_WHEREEVER, *toItem, toItem->getItemCount(), maxExchangeQueryCount, 0);
			if (retExchangeMaxCount != RETURNVALUE_NOERROR && maxExchangeQueryCount == 0) {
				return retExchangeMaxCount;
			}
			if (toCylinder->queryRemove(*toItem, toItem->getItemCount(), flags, actor) == RETURNVALUE_NOERROR) {
				int32_t oldToItemIndex = toCylinder->getThingIndex(toItem);
				toCylinder->removeThing(toItem, toItem->getItemCount());
				fromCylinder->addThing(toItem);
				if (oldToItemIndex != -1) {
					toCylinder->postRemoveNotification(toItem, fromCylinder, oldToItemIndex);
				}
				int32_t newToItemIndex = fromCylinder->getThingIndex(toItem);
				if (newToItemIndex != -1) {
					fromCylinder->postAddNotification(toItem, toCylinder, newToItemIndex);
				}
				ret = toCylinder->queryAdd(index, *item, count, flags);
				if (actorPlayer && fromPos && toPos) {
					g_events->eventPlayerOnItemMoved(actorPlayer, toItem, toItem->getItemCount(), *toPos, *fromPos, toCylinder, fromCylinder);
				}
				toItem = nullptr;
			}
		}
	}
	if (ret != RETURNVALUE_NOERROR) return ret;

	uint32_t maxQueryCount = 0;
	ReturnValue retMaxCount = toCylinder->queryMaxCount(index, *item, count, maxQueryCount, flags);
	if (retMaxCount != RETURNVALUE_NOERROR && maxQueryCount == 0) return retMaxCount;

	uint32_t m = item->isStackable() ? std::min<uint32_t>(count, maxQueryCount) : maxQueryCount;
	ret = fromCylinder->queryRemove(*item, m, flags, actor);
	if (ret != RETURNVALUE_NOERROR) return ret;

	if (tradeItem) {
		if (toCylinder->getItem() == tradeItem) return RETURNVALUE_NOTENOUGHROOM;
		for (Cylinder* tmpCylinder = toCylinder->getParent(); tmpCylinder; tmpCylinder = tmpCylinder->getParent()) {
			if (tmpCylinder->getItem() == tradeItem) return RETURNVALUE_NOTENOUGHROOM;
		}
	}

	int32_t itemIndex = fromCylinder->getThingIndex(item);
	Item* updateItem = nullptr;
	fromCylinder->removeThing(item, m);

	if (item->isStackable()) {
		uint32_t n = 0;
		if (item->equals(toItem)) {
			n = std::min<uint32_t>(100 - toItem->getItemCount(), m);
			toCylinder->updateThing(toItem, toItem->getID(), toItem->getItemCount() + n);
			updateItem = toItem;
		}
		int32_t newCount = m - n;
		std::unique_ptr<Item> moveItem = (newCount > 0) ? item->clone() : nullptr;
		if (moveItem) moveItem->setItemCount(newCount);
		if (item->isRemoved()) ReleaseItem(item);
		if (moveItem) toCylinder->addThing(index, moveItem.get());
	}
	else {
		toCylinder->addThing(index, item);
	}

	if (itemIndex != -1) {
		fromCylinder->postRemoveNotification(item, toCylinder, itemIndex);
	}

	Item* finalMoveItem = item;
	if (item->isStackable() && toItem && updateItem) {
		finalMoveItem = updateItem;
	}
	else if (item->isStackable() && !toItem) {
		finalMoveItem = moveItem.get();
	}

	if (finalMoveItem) {
		int32_t moveItemIndex = toCylinder->getThingIndex(finalMoveItem);
		if (moveItemIndex != -1) {
			toCylinder->postAddNotification(finalMoveItem, fromCylinder, moveItemIndex);
		}
	}

	if (updateItem) {
		int32_t updateItemIndex = toCylinder->getThingIndex(updateItem);
		if (updateItemIndex != -1) {
			toCylinder->postAddNotification(updateItem, fromCylinder, updateItemIndex);
		}
	}

	if (_moveItem) {
		*_moveItem = finalMoveItem;
	}

	if (item->isStackable() && maxQueryCount < count) {
		return retMaxCount;
	}

	if (finalMoveItem && finalMoveItem->getDuration() > 0 && finalMoveItem->getDecaying() != DECAYING_TRUE) {
		finalMoveItem->incrementReferenceCounter();
		finalMoveItem->setDecaying(DECAYING_TRUE);
		std::lock_guard<std::mutex> decayLock(decayMutex);
		toDecayItems.push_front(finalMoveItem);
	}

	if (actorPlayer && fromPos && toPos) {
		g_events->eventPlayerOnItemMoved(actorPlayer, item, count, *fromPos, *toPos, fromCylinder, toCylinder);
	}

	return ret;
}

ReturnValue Game::internalAddItem(Cylinder* toCylinder, Item* item, int32_t index, uint32_t flags, bool test) {
	uint32_t remainderCount = 0;
	return internalAddItem(toCylinder, item, index, flags, test, remainderCount);
}

ReturnValue Game::internalAddItem(Cylinder* toCylinder, Item* item, int32_t index, uint32_t flags, bool test, uint32_t& remainderCount) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (!toCylinder || !item) return RETURNVALUE_NOTPOSSIBLE;

	Item* toItem = nullptr;
	toCylinder = toCylinder->queryDestination(index, *item, &toItem, flags);
	ReturnValue ret = toCylinder->queryAdd(index, *item, item->getItemCount(), flags);
	if (ret != RETURNVALUE_NOERROR) return ret;

	uint32_t maxQueryCount = 0;
	ret = toCylinder->queryMaxCount(INDEX_WHEREEVER, *item, item->getItemCount(), maxQueryCount, flags);
	if (ret != RETURNVALUE_NOERROR) return ret;

	if (test) return RETURNVALUE_NOERROR;

	if (item->isStackable() && item->equals(toItem)) {
		uint32_t m = std::min<uint32_t>(item->getItemCount(), maxQueryCount);
		uint32_t n = std::min<uint32_t>(100 - toItem->getItemCount(), m);
		toCylinder->updateThing(toItem, toItem->getID(), toItem->getItemCount() + n);

		int32_t count = m - n;
		if (count > 0) {
			std::unique_ptr<Item> remainderItem = (item->getItemCount() != count) ? item->clone() : nullptr;
			if (remainderItem) {
				remainderItem->setItemCount(count);
				if (internalAddItem(toCylinder, remainderItem.get(), INDEX_WHEREEVER, flags, false) != RETURNVALUE_NOERROR) {
					remainderCount = count;
				}
				else {
					remainderItem.release(); // Ownership transferred to toCylinder
				}
			}
		}
		else {
			item->onRemoved();
			ReleaseItem(item);
			int32_t itemIndex = toCylinder->getThingIndex(toItem);
			if (itemIndex != -1) {
				toCylinder->postAddNotification(toItem, nullptr, itemIndex);
			}
		}
	}
	else {
		toCylinder->addThing(index, item);
		int32_t itemIndex = toCylinder->getThingIndex(item);
		if (itemIndex != -1) {
			toCylinder->postAddNotification(item, nullptr, itemIndex);
		}
	}

	if (item->getDuration() > 0 && item->getDecaying() != DECAYING_TRUE) {
		item->incrementReferenceCounter();
		item->setDecaying(DECAYING_TRUE);
		std::lock_guard<std::mutex> decayLock(decayMutex);
		toDecayItems.push_front(item);
	}

	return RETURNVALUE_NOERROR;
}

ReturnValue Game::internalRemoveItem(Item* item, int32_t count, bool test, uint32_t flags) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Cylinder* cylinder = item->getParent();
	if (!cylinder) return RETURNVALUE_NOTPOSSIBLE;

	if (count == -1) count = item->getItemCount();

	ReturnValue ret = cylinder->queryRemove(*item, count, flags | FLAG_IGNORENOTMOVEABLE);
	if (ret != RETURNVALUE_NOERROR || !item->canRemove()) return ret;

	if (!test) {
		int32_t index = cylinder->getThingIndex(item);
		cylinder->removeThing(item, count);
		if (item->isRemoved()) {
			item->onRemoved();
			if (item->canDecay()) {
				std::lock_guard<std::mutex> decayLock(decayMutex);
				decayItems->remove(item);
			}
			ReleaseItem(item);
		}
		cylinder->postRemoveNotification(item, nullptr, index);
	}
	return RETURNVALUE_NOERROR;
}

ReturnValue Game::internalPlayerAddItem(Player* player, Item* item, bool dropOnMap, slots_t slot) {
	std::lock_guard<std::mutex> lock(gameMutex);
	uint32_t remainderCount = 0;
	ReturnValue ret = internalAddItem(player, item, static_cast<int32_t>(slot), 0, false, remainderCount);
	if (remainderCount != 0) {
		std::unique_ptr<Item> remainderItem = Item::CreateItem(item->getID(), remainderCount);
		ReturnValue remaindRet = internalAddItem(player->getTile(), remainderItem.get(), INDEX_WHEREEVER, FLAG_NOLIMIT);
		if (remaindRet != RETURNVALUE_NOERROR) {
			remainderItem.reset(); // Destroy if can't add
		}
		else {
			remainderItem.release(); // Ownership transferred
		}
	}
	if (ret != RETURNVALUE_NOERROR && dropOnMap) {
		ret = internalAddItem(player->getTile(), item, INDEX_WHEREEVER, FLAG_NOLIMIT);
	}
	return ret;
}

Item* Game::findItemOfType(Cylinder* cylinder, uint16_t itemId, bool depthSearch, int32_t subType) const {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (!cylinder) return nullptr;

	std::vector<Container*> containers;
	for (size_t i = cylinder->getFirstIndex(), j = cylinder->getLastIndex(); i < j; ++i) {
		Thing* thing = cylinder->getThing(i);
		if (!thing) continue;

		Item* item = thing->getItem();
		if (!item) continue;

		if (item->getID() == itemId && (subType == -1 || subType == item->getSubType())) {
			return item;
		}
		if (depthSearch) {
			if (Container* container = item->getContainer()) {
				containers.push_back(container);
			}
		}
	}

	for (size_t i = 0; i < containers.size();) {
		Container* container = containers[i++];
		for (Item* item : container->getItemList()) {
			if (item->getID() == itemId && (subType == -1 || subType == item->getSubType())) {
				return item;
			}
			if (Container* subContainer = item->getContainer()) {
				containers.push_back(subContainer);
			}
		}
	}
	return nullptr;
}

bool Game::removeMoney(Cylinder* cylinder, uint64_t money, uint32_t flags) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (!cylinder || money == 0) return !money;

	std::vector<Container*> containers;
	std::multimap<uint64_t, Item*> moneyMap;
	uint64_t moneyCount = 0;

	for (size_t i = cylinder->getFirstIndex(), j = cylinder->getLastIndex(); i < j; ++i) {
		Thing* thing = cylinder->getThing(i);
		if (!thing) continue;

		Item* item = thing->getItem();
		if (!item) continue;

		if (Container* container = item->getContainer()) {
			containers.push_back(container);
		}
		else if (uint32_t worth = item->getWorth()) {
			moneyCount += worth;
			moneyMap.emplace(worth, item);
		}
	}

	for (size_t i = 0; i < containers.size();) {
		Container* container = containers[i++];
		for (Item* item : container->getItemList()) {
			if (Container* tmpContainer = item->getContainer()) {
				containers.push_back(tmpContainer);
			}
			else if (uint32_t worth = item->getWorth()) {
				moneyCount += worth;
				moneyMap.emplace(worth, item);
			}
		}
	}

	if (moneyCount < money) return false;

	for (const auto& [worth, item] : moneyMap) {
		if (worth < money) {
			internalRemoveItem(item);
			money -= worth;
		}
		else if (worth > money) {
			uint32_t removeCount = std::ceil(static_cast<double>(money) / (worth / item->getItemCount()));
			addMoney(cylinder, (worth * removeCount) - money, flags);
			internalRemoveItem(item, removeCount);
			break;
		}
		else {
			internalRemoveItem(item);
			break;
		}
	}
	return true;
}

void Game::addMoney(Cylinder* cylinder, uint64_t money, uint32_t flags) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (money == 0) return;

	for (const auto& [worth, id] : Item::items.currencyItems) {
		uint32_t currencyCoins = money / worth;
		if (currencyCoins <= 0) continue;

		money -= currencyCoins * worth;
		while (currencyCoins > 0) {
			uint16_t count = std::min<uint32_t>(100, currencyCoins);
			std::unique_ptr<Item> remaindItem = Item::CreateItem(id, count);
			ReturnValue ret = internalAddItem(cylinder, remaindItem.get(), INDEX_WHEREEVER, flags);
			if (ret != RETURNVALUE_NOERROR) {
				internalAddItem(cylinder->getTile(), remaindItem.get(), INDEX_WHEREEVER, FLAG_NOLIMIT);
			}
			remaindItem.release(); // Ownership transferred
			currencyCoins -= count;
		}
	}
}

Item* Game::transformItem(Item* item, uint16_t newId, int32_t newCount) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (item->getID() == newId && (newCount == -1 || (newCount == item->getSubType() && newCount != 0))) {
		return item;
	}

	Cylinder* cylinder = item->getParent();
	if (!cylinder) return nullptr;

	int32_t itemIndex = cylinder->getThingIndex(item);
	if (itemIndex == -1 || !item->canTransform()) return item;

	const ItemType& newType = Item::items[newId];
	if (newType.id == 0) return item;

	const ItemType& curType = Item::items[item->getID()];
	if (curType.alwaysOnTop != newType.alwaysOnTop) {
		cylinder->removeThing(item, item->getItemCount());
		cylinder->postRemoveNotification(item, cylinder, itemIndex);
		item->setID(newId);
		if (newCount != -1) item->setSubType(newCount);
		cylinder->addThing(item);
		Cylinder* newParent = item->getParent();
		if (!newParent) {
			ReleaseItem(item);
			return nullptr;
		}
		newParent->postAddNotification(item, cylinder, newParent->getThingIndex(item));
		return item;
	}

	if (curType.type == newType.type) {
		if (newCount == 0 && (item->isStackable() || item->hasAttribute(ITEM_ATTRIBUTE_CHARGES))) {
			if (item->isStackable()) {
				internalRemoveItem(item);
				return nullptr;
			}
			int32_t newItemId = (curType.id == newType.id) ? item->getDecayTo() : newId;
			if (newItemId < 0) {
				internalRemoveItem(item);
				return nullptr;
			}
			else if (newItemId != newId) {
				std::unique_ptr<Item> newItem = Item::CreateItem(newItemId, 1);
				if (!newItem) return nullptr;
				cylinder->replaceThing(itemIndex, newItem.get());
				cylinder->postAddNotification(newItem.get(), cylinder, itemIndex);
				item->setParent(nullptr);
				cylinder->postRemoveNotification(item, cylinder, itemIndex);
				ReleaseItem(item);
				return newItem.release();
			}
			return transformItem(item, newItemId);
		}
		cylinder->postRemoveNotification(item, cylinder, itemIndex);
		item->setID(newId);
		if (newCount != -1) item->setSubType(newCount);
		cylinder->postAddNotification(item, cylinder, itemIndex);
		return item;
	}
	return nullptr; // Truncated in original; assuming incomplete logic
}
bool Game::combatBlockHit(CombatDamage& damage, Creature* attacker, Creature* target, bool checkDefense, bool checkArmor, bool field, bool ignoreResistances) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (damage.primary.type == COMBAT_NONE && damage.secondary.type == COMBAT_NONE) {
		return true;
	}

	if (target->getPlayer() && target->isInGhostMode()) {
		return true;
	}

	if (damage.primary.value > 0) {
		return false;
	}

	auto sendBlockEffect = [this](BlockType_t blockType, CombatType_t combatType, const Position& targetPos) {
		if (blockType == BLOCK_DEFENSE) {
			addMagicEffect(targetPos, CONST_ME_POFF);
		}
		else if (blockType == BLOCK_ARMOR) {
			addMagicEffect(targetPos, CONST_ME_BLOCKHIT);
		}
		else if (blockType == BLOCK_IMMUNITY) {
			uint8_t hitEffect = (combatType == COMBAT_EARTHDAMAGE) ? CONST_ME_GREEN_RINGS :
				(combatType == COMBAT_HOLYDAMAGE) ? CONST_ME_HOLYDAMAGE :
				(combatType == COMBAT_ENERGYDAMAGE || combatType == COMBAT_FIREDAMAGE ||
					combatType == COMBAT_PHYSICALDAMAGE || combatType == COMBAT_ICEDAMAGE ||
					combatType == COMBAT_DEATHDAMAGE) ? CONST_ME_BLOCKHIT : CONST_ME_POFF;
			addMagicEffect(targetPos, hitEffect);
		}
		};

	BlockType_t primaryBlockType = BLOCK_NONE, secondaryBlockType = BLOCK_NONE;
	if (damage.primary.type != COMBAT_NONE) {
		damage.primary.value = -damage.primary.value;
		primaryBlockType = target->blockHit(attacker, damage.primary.type, damage.primary.value, checkDefense, checkArmor, field, ignoreResistances);
		damage.primary.value = -damage.primary.value;
		sendBlockEffect(primaryBlockType, damage.primary.type, target->getPosition());
	}
	if (damage.secondary.type != COMBAT_NONE) {
		damage.secondary.value = -damage.secondary.value;
		secondaryBlockType = target->blockHit(attacker, damage.secondary.type, damage.secondary.value, false, false, field, ignoreResistances);
		damage.secondary.value = -damage.secondary.value;
		sendBlockEffect(secondaryBlockType, damage.secondary.type, target->getPosition());
	}
	damage.blockType = primaryBlockType;
	return primaryBlockType != BLOCK_NONE && secondaryBlockType != BLOCK_NONE;
}

void Game::combatGetTypeInfo(CombatType_t combatType, Creature* target, TextColor_t& color, uint8_t& effect) {
	switch (combatType) {
	case COMBAT_PHYSICALDAMAGE: {
		switch (target->getRace()) {
		case RACE_VENOM: color = TEXTCOLOR_LIGHTGREEN; effect = CONST_ME_HITBYPOISON; break;
		case RACE_BLOOD: color = TEXTCOLOR_RED; effect = CONST_ME_DRAWBLOOD; break;
		case RACE_UNDEAD: color = TEXTCOLOR_GREY; effect = CONST_ME_HITAREA; break;
		case RACE_FIRE: color = TEXTCOLOR_ORANGE; effect = CONST_ME_DRAWBLOOD; break;
		case RACE_ENERGY: color = TEXTCOLOR_PURPLE; effect = CONST_ME_ENERGYHIT; break;
		default: color = TEXTCOLOR_NONE; effect = CONST_ME_NONE; break;
		}
		if (color == TEXTCOLOR_RED && target->getTile() && !target->getTile()->hasFlag(TILESTATE_PROTECTIONZONE)) {
			auto splash = Item::CreateItem(ITEM_SMALLSPLASH, FLUID_BLOOD);
			internalAddItem(target->getTile(), splash, INDEX_WHEREEVER, FLAG_NOLIMIT);
			startDecay(splash);
		}
		break;
	}
	case COMBAT_ENERGYDAMAGE: color = TEXTCOLOR_PURPLE; effect = CONST_ME_ENERGYHIT; break;
	case COMBAT_EARTHDAMAGE: color = TEXTCOLOR_LIGHTGREEN; effect = CONST_ME_GREEN_RINGS; break;
	case COMBAT_DROWNDAMAGE: color = TEXTCOLOR_LIGHTBLUE; effect = CONST_ME_LOSEENERGY; break;
	case COMBAT_FIREDAMAGE: color = TEXTCOLOR_ORANGE; effect = CONST_ME_HITBYFIRE; break;
	case COMBAT_ICEDAMAGE: color = TEXTCOLOR_TEAL; effect = CONST_ME_ICEATTACK; break;
	case COMBAT_HOLYDAMAGE: color = TEXTCOLOR_YELLOW; effect = CONST_ME_HOLYDAMAGE; break;
	case COMBAT_DEATHDAMAGE: color = TEXTCOLOR_DARKRED; effect = CONST_ME_SMALLCLOUDS; break;
	case COMBAT_LIFEDRAIN: color = TEXTCOLOR_RED; effect = CONST_ME_MAGIC_RED; break;
	default: color = TEXTCOLOR_NONE; effect = CONST_ME_NONE; break;
	}
}

bool Game::combatChangeHealth(Creature* attacker, Creature* target, CombatDamage& damage) {
	std::lock_guard<std::mutex> lock(gameMutex);
	const Position& targetPos = target->getPosition();
	if (damage.primary.value > 0) {
		if (target->getHealth() <= 0) return false;

		auto attackerPlayer = attacker ? attacker->getPlayer() : nullptr;
		auto targetPlayer = target->getPlayer();
		if (attackerPlayer && targetPlayer && attackerPlayer->getSkull() == SKULL_BLACK && attackerPlayer->getSkullClient(targetPlayer) == SKULL_NONE) {
			return false;
		}

		if (damage.origin != ORIGIN_NONE) {
			const auto& events = target->getCreatureEvents(CREATURE_EVENT_HEALTHCHANGE);
			if (!events.empty()) {
				for (auto* event : events) {
					event->executeHealthChange(target, attacker, damage);
				}
				damage.origin = ORIGIN_NONE;
				return combatChangeHealth(attacker, target, damage);
			}
		}

		int32_t realHealthChange = target->getHealth();
		target->gainHealth(attacker, damage.primary.value);
		realHealthChange = target->getHealth() - realHealthChange;

		if (realHealthChange > 0 && !target->isInGhostMode()) {
			SpectatorVec spectators;
			map.getSpectators(spectators, targetPos, false, true);
			spectators.reserve(spectators.size());
			addAnimatedText(fmt::format("{}", realHealthChange), targetPos, TEXTCOLOR_MAYABLUE);

			std::string spectatorMessage;
			TextMessage message;
			for (auto* spectator : spectators) {
				auto tmpPlayer = spectator->getPlayer();
				if (!tmpPlayer) continue;

				message.type = MESSAGE_STATUS_DEFAULT;
				auto damageString = fmt::format("{:d} hitpoint{:s}", realHealthChange, realHealthChange != 1 ? "s" : "");
				if (tmpPlayer == attackerPlayer && attackerPlayer != targetPlayer) {
					message.text = fmt::format("You heal {:s} for {:s}.", target->getNameDescription(), damageString);
				}
				else if (tmpPlayer == targetPlayer) {
					message.text = !attacker ? fmt::format("You were healed for {:s}.", damageString) :
						(targetPlayer == attackerPlayer) ? fmt::format("You healed yourself for {:s}.", damageString) :
						fmt::format("You were healed by {:s} for {:s}.", attacker->getNameDescription(), damageString);
				}
				else {
					if (spectatorMessage.empty()) {
						spectatorMessage = !attacker ? fmt::format("{:s} was healed for {:s}", target->getNameDescription(), damageString) :
							(attacker == target) ? fmt::format("{:s} healed {:s}self for {:s}", attacker->getNameDescription(), targetPlayer ? (targetPlayer->getSex() == PLAYERSEX_FEMALE ? "her" : "him") : "it", damageString) :
							fmt::format("{:s} healed {:s} for {:s}", attacker->getNameDescription(), target->getNameDescription(), damageString);
						spectatorMessage[0] = std::toupper(spectatorMessage[0]);
					}
					message.text = spectatorMessage;
				}
				tmpPlayer->sendTextMessage(message);
			}
		}
	}
	else {
		if (!target->isAttackable()) {
			if (!target->isInGhostMode()) addMagicEffect(targetPos, CONST_ME_POFF);
			return true;
		}

		auto attackerPlayer = attacker ? attacker->getPlayer() : nullptr;
		auto targetPlayer = target->getPlayer();
		if (attackerPlayer && targetPlayer && attackerPlayer->getSkull() == SKULL_BLACK && attackerPlayer->getSkullClient(targetPlayer) == SKULL_NONE) {
			return false;
		}

		damage.primary.value = std::abs(damage.primary.value);
		damage.secondary.value = std::abs(damage.secondary.value);
		int32_t healthChange = damage.primary.value + damage.secondary.value;
		if (healthChange == 0) return true;

		SpectatorVec spectators;
		TextMessage message;

		if (targetPlayer && target->hasCondition(CONDITION_MANASHIELD) && damage.primary.type != COMBAT_UNDEFINEDDAMAGE) {
			int32_t manaDamage = std::min<int32_t>(targetPlayer->getMana(), healthChange);
			if (manaDamage != 0) {
				if (damage.origin != ORIGIN_NONE) {
					const auto& events = target->getCreatureEvents(CREATURE_EVENT_MANACHANGE);
					if (!events.empty()) {
						for (auto* event : events) {
							event->executeManaChange(target, attacker, damage);
						}
						healthChange = damage.primary.value + damage.secondary.value;
						if (healthChange == 0) return true;
						manaDamage = std::min<int32_t>(targetPlayer->getMana(), healthChange);
					}
				}

				targetPlayer->drainMana(attacker, manaDamage);
				map.getSpectators(spectators, targetPos, true, true);
				spectators.reserve(spectators.size());
				addMagicEffect(spectators, targetPos, CONST_ME_LOSEENERGY);
				addAnimatedText(fmt::format("{}", manaDamage), targetPos, TEXTCOLOR_BLUE);

				std::string spectatorMessage;
				for (auto* spectator : spectators) {
					auto tmpPlayer = spectator->getPlayer();
					if (tmpPlayer->getPosition().z != targetPos.z) continue;

					message.type = MESSAGE_STATUS_DEFAULT;
					if (tmpPlayer == attackerPlayer && attackerPlayer != targetPlayer) {
						message.text = fmt::format("{:s} loses {:d} mana due to your attack.", target->getNameDescription(), manaDamage);
						message.text[0] = std::toupper(message.text[0]);
					}
					else if (tmpPlayer == targetPlayer) {
						message.text = !attacker ? fmt::format("You lose {:d} mana.", manaDamage) :
							(targetPlayer == attackerPlayer) ? fmt::format("You lose {:d} mana due to your own attack.", manaDamage) :
							fmt::format("You lose {:d} mana due to an attack by {:s}.", manaDamage, attacker->getNameDescription());
					}
					else {
						if (spectatorMessage.empty()) {
							spectatorMessage = !attacker ? fmt::format("{:s} loses {:d} mana.", target->getNameDescription(), manaDamage) :
								(attacker == target) ? fmt::format("{:s} loses {:d} mana due to {:s} own attack.", target->getNameDescription(), manaDamage, targetPlayer->getSex() == PLAYERSEX_FEMALE ? "her" : "his") :
								fmt::format("{:s} loses {:d} mana due to an attack by {:s}.", target->getNameDescription(), manaDamage, attacker->getNameDescription());
							spectatorMessage[0] = std::toupper(spectatorMessage[0]);
						}
						message.text = spectatorMessage;
					}
					tmpPlayer->sendTextMessage(message);
				}

				damage.primary.value -= manaDamage;
				if (damage.primary.value < 0) {
					damage.secondary.value = std::max<int32_t>(0, damage.secondary.value + damage.primary.value);
					damage.primary.value = 0;
				}
			}
		}

		int32_t realDamage = damage.primary.value + damage.secondary.value;
		if (realDamage == 0) return true;

		if (damage.origin != ORIGIN_NONE) {
			const auto& events = target->getCreatureEvents(CREATURE_EVENT_HEALTHCHANGE);
			if (!events.empty()) {
				for (auto* event : events) {
					event->executeHealthChange(target, attacker, damage);
				}
				damage.origin = ORIGIN_NONE;
				return combatChangeHealth(attacker, target, damage);
			}
		}

		int32_t targetHealth = target->getHealth();
		if (damage.primary.value >= targetHealth) {
			damage.primary.value = targetHealth;
			damage.secondary.value = 0;
		}
		else if (damage.secondary.value) {
			damage.secondary.value = std::min<int32_t>(damage.secondary.value, targetHealth - damage.primary.value);
		}

		realDamage = damage.primary.value + damage.secondary.value;
		if (realDamage == 0) return true;

		if (spectators.empty()) {
			map.getSpectators(spectators, targetPos, true, true);
			spectators.reserve(spectators.size());
		}

		message.primary.value = damage.primary.value;
		message.secondary.value = damage.secondary.value;

		uint8_t hitEffect;
		if (message.primary.value) {
			combatGetTypeInfo(damage.primary.type, target, message.primary.color, hitEffect);
			if (hitEffect != CONST_ME_NONE) addMagicEffect(spectators, targetPos, hitEffect);
			if (message.primary.color != TEXTCOLOR_NONE) {
				addAnimatedText(fmt::format("{}", message.primary.value), targetPos, message.primary.color);
			}
		}

		if (message.secondary.value) {
			combatGetTypeInfo(damage.secondary.type, target, message.secondary.color, hitEffect);
			if (hitEffect != CONST_ME_NONE) addMagicEffect(spectators, targetPos, hitEffect);
			if (message.secondary.color != TEXTCOLOR_NONE) {
				addAnimatedText(fmt::format("{}", message.secondary.value), targetPos, message.secondary.color);
			}
		}

		if (message.primary.color != TEXTCOLOR_NONE || message.secondary.color != TEXTCOLOR_NONE) {
			auto damageString = fmt::format("{:d} hitpoint{:s}", realDamage, realDamage != 1 ? "s" : "");
			std::string spectatorMessage;

			for (auto* spectator : spectators) {
				auto tmpPlayer = spectator->getPlayer();
				if (tmpPlayer->getPosition().z != targetPos.z) continue;

				message.type = MESSAGE_STATUS_DEFAULT;
				if (tmpPlayer == attackerPlayer && attackerPlayer != targetPlayer) {
					message.text = fmt::format("{:s} loses {:s} due to your attack.", target->getNameDescription(), damageString);
					message.text[0] = std::toupper(message.text[0]);
				}
				else if (tmpPlayer == targetPlayer) {
					message.text = !attacker ? fmt::format("You lose {:s}.", damageString) :
						(targetPlayer == attackerPlayer) ? fmt::format("You lose {:s} due to your own attack.", damageString) :
						fmt::format("You lose {:s} due to an attack by {:s}.", damageString, attacker->getNameDescription());
				}
				else {
					if (spectatorMessage.empty()) {
						spectatorMessage = !attacker ? fmt::format("{:s} loses {:s}.", target->getNameDescription(), damageString) :
							(attacker == target) ? fmt::format("{:s} loses {:s} due to {:s} own attack.", target->getNameDescription(), damageString, targetPlayer ? (targetPlayer->getSex() == PLAYERSEX_FEMALE ? "her" : "his") : "its") :
							fmt::format("{:s} loses {:s} due to an attack by {:s}.", target->getNameDescription(), damageString, attacker->getNameDescription());
						spectatorMessage[0] = std::toupper(spectatorMessage[0]);
					}
					message.text = spectatorMessage;
				}
				tmpPlayer->sendTextMessage(message);
			}
		}

		if (realDamage >= targetHealth) {
			for (auto* event : target->getCreatureEvents(CREATURE_EVENT_PREPAREDEATH)) {
				if (!event->executeOnPrepareDeath(target, attacker)) return false;
			}
		}

		target->drainHealth(attacker, realDamage);
		addCreatureHealth(spectators, target);
	}
	return true;
}

bool Game::combatChangeMana(Creature* attacker, Creature* target, CombatDamage& damage) {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto targetPlayer = target->getPlayer();
	if (!targetPlayer) return true;

	int32_t manaChange = damage.primary.value + damage.secondary.value;
	if (manaChange > 0) {
		if (attacker) {
			auto attackerPlayer = attacker->getPlayer();
			if (attackerPlayer && attackerPlayer->getSkull() == SKULL_BLACK && attackerPlayer->getSkullClient(target) == SKULL_NONE) {
				return false;
			}
		}

		if (damage.origin != ORIGIN_NONE) {
			const auto& events = target->getCreatureEvents(CREATURE_EVENT_MANACHANGE);
			if (!events.empty()) {
				for (auto* event : events) {
					event->executeManaChange(target, attacker, damage);
				}
				damage.origin = ORIGIN_NONE;
				return combatChangeMana(attacker, target, damage);
			}
		}

		targetPlayer->changeMana(manaChange);
	}
	else {
		const Position& targetPos = target->getPosition();
		if (!target->isAttackable()) {
			if (!target->isInGhostMode()) addMagicEffect(targetPos, CONST_ME_POFF);
			return false;
		}

		auto attackerPlayer = attacker ? attacker->getPlayer() : nullptr;
		if (attackerPlayer && targetPlayer && attackerPlayer->getSkull() == SKULL_BLACK && attackerPlayer->getSkullClient(targetPlayer) == SKULL_NONE) {
			return false;
		}

		int32_t manaLoss = std::min<int32_t>(targetPlayer->getMana(), -manaChange);
		BlockType_t blockType = target->blockHit(attacker, COMBAT_MANADRAIN, manaLoss);
		if (blockType != BLOCK_NONE) {
			addMagicEffect(targetPos, CONST_ME_POFF);
			return false;
		}

		if (manaLoss <= 0) return true;

		if (damage.origin != ORIGIN_NONE) {
			const auto& events = target->getCreatureEvents(CREATURE_EVENT_MANACHANGE);
			if (!events.empty()) {
				for (auto* event : events) {
					event->executeManaChange(target, attacker, damage);
				}
				damage.origin = ORIGIN_NONE;
				return combatChangeMana(attacker, target, damage);
			}
		}

		targetPlayer->drainMana(attacker, manaLoss);
		SpectatorVec spectators;
		map.getSpectators(spectators, targetPos, false, true);
		spectators.reserve(spectators.size());
		addAnimatedText(fmt::format("{}", manaLoss), targetPos, TEXTCOLOR_BLUE);

		std::string spectatorMessage;
		TextMessage message;
		for (auto* spectator : spectators) {
			auto tmpPlayer = spectator->getPlayer();
			message.type = MESSAGE_STATUS_DEFAULT;
			if (tmpPlayer == attackerPlayer && attackerPlayer != targetPlayer) {
				message.text = fmt::format("{:s} loses {:d} mana due to your attack.", target->getNameDescription(), manaLoss);
				message.text[0] = std::toupper(message.text[0]);
			}
			else if (tmpPlayer == targetPlayer) {
				message.text = !attacker ? fmt::format("You lose {:d} mana.", manaLoss) :
					(targetPlayer == attackerPlayer) ? fmt::format("You lose {:d} mana due to your own attack.", manaLoss) :
					fmt::format("You lose {:d} mana due to an attack by {:s}.", manaLoss, attacker->getNameDescription());
			}
			else {
				if (spectatorMessage.empty()) {
					spectatorMessage = !attacker ? fmt::format("{:s} loses {:d} mana.", target->getNameDescription(), manaLoss) :
						(attacker == target) ? fmt::format("{:s} loses {:d} mana due to {:s} own attack.", target->getNameDescription(), manaLoss, targetPlayer->getSex() == PLAYERSEX_FEMALE ? "her" : "his") :
						fmt::format("{:s} loses {:d} mana due to an attack by {:s}.", target->getNameDescription(), manaLoss, attacker->getNameDescription());
					spectatorMessage[0] = std::toupper(spectatorMessage[0]);
				}
				message.text = spectatorMessage;
			}
			tmpPlayer->sendTextMessage(message);
		}
	}
	return true;
}

void Game::addCreatureHealth(const Creature* target) {
	SpectatorVec spectators;
	map.getSpectators(spectators, target->getPosition(), true, true);
	addCreatureHealth(spectators, target);
}

void Game::addCreatureHealth(const SpectatorVec& spectators, const Creature* target) {
	for (auto* spectator : spectators) {
		if (auto tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendCreatureHealth(target);
		}
	}
}

void Game::addAnimatedText(const std::string& message, const Position& pos, TextColor_t color) {
	SpectatorVec spectators;
	map.getSpectators(spectators, pos, true, true);
	if (spectators.empty()) return;
	spectators.reserve(spectators.size());
	addAnimatedText(spectators, message, pos, color);
}

void Game::addAnimatedText(const SpectatorVec& spectators, const std::string& message, const Position& pos, TextColor_t color) {
	for (auto* spectator : spectators) {
		if (auto tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendAnimatedText(message, pos, color);
		}
	}
}

void Game::addMagicEffect(const Position& pos, uint8_t effect) {
	SpectatorVec spectators;
	map.getSpectators(spectators, pos, true, true);
	spectators.reserve(spectators.size());
	addMagicEffect(spectators, pos, effect);
}

void Game::addMagicEffect(const SpectatorVec& spectators, const Position& pos, uint8_t effect) {
	for (auto* spectator : spectators) {
		if (auto tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendMagicEffect(pos, effect);
		}
	}
}

void Game::addDistanceEffect(const Position& fromPos, const Position& toPos, uint8_t effect) {
	SpectatorVec spectators, toPosSpectators;
	map.getSpectators(spectators, fromPos, true, true);
	map.getSpectators(toPosSpectators, toPos, true, true);
	spectators.addSpectators(toPosSpectators);
	spectators.reserve(spectators.size());
	addDistanceEffect(spectators, fromPos, toPos, effect);
}

void Game::addDistanceEffect(const SpectatorVec& spectators, const Position& fromPos, const Position& toPos, uint8_t effect) {
	for (auto* spectator : spectators) {
		if (auto tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendDistanceShoot(fromPos, toPos, effect);
		}
	}
}
bool Game::combatBlockHit(CombatDamage& damage, Creature* attacker, Creature* target, bool checkDefense, bool checkArmor, bool field, bool ignoreResistances) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (damage.primary.type == COMBAT_NONE && damage.secondary.type == COMBAT_NONE) {
		return true;
	}

	if (target->getPlayer() && target->isInGhostMode()) {
		return true;
	}

	if (damage.primary.value > 0) {
		return false;
	}

	auto sendBlockEffect = [this](BlockType_t blockType, CombatType_t combatType, const Position& targetPos) {
		if (blockType == BLOCK_DEFENSE) {
			addMagicEffect(targetPos, CONST_ME_POFF);
		}
		else if (blockType == BLOCK_ARMOR) {
			addMagicEffect(targetPos, CONST_ME_BLOCKHIT);
		}
		else if (blockType == BLOCK_IMMUNITY) {
			uint8_t hitEffect = (combatType == COMBAT_EARTHDAMAGE) ? CONST_ME_GREEN_RINGS :
				(combatType == COMBAT_HOLYDAMAGE) ? CONST_ME_HOLYDAMAGE :
				(combatType == COMBAT_ENERGYDAMAGE || combatType == COMBAT_FIREDAMAGE ||
					combatType == COMBAT_PHYSICALDAMAGE || combatType == COMBAT_ICEDAMAGE ||
					combatType == COMBAT_DEATHDAMAGE) ? CONST_ME_BLOCKHIT : CONST_ME_POFF;
			addMagicEffect(targetPos, hitEffect);
		}
		};

	BlockType_t primaryBlockType = BLOCK_NONE, secondaryBlockType = BLOCK_NONE;
	if (damage.primary.type != COMBAT_NONE) {
		damage.primary.value = -damage.primary.value;
		primaryBlockType = target->blockHit(attacker, damage.primary.type, damage.primary.value, checkDefense, checkArmor, field, ignoreResistances);
		damage.primary.value = -damage.primary.value;
		sendBlockEffect(primaryBlockType, damage.primary.type, target->getPosition());
	}
	if (damage.secondary.type != COMBAT_NONE) {
		damage.secondary.value = -damage.secondary.value;
		secondaryBlockType = target->blockHit(attacker, damage.secondary.type, damage.secondary.value, false, false, field, ignoreResistances);
		damage.secondary.value = -damage.secondary.value;
		sendBlockEffect(secondaryBlockType, damage.secondary.type, target->getPosition());
	}
	damage.blockType = primaryBlockType;
	return primaryBlockType != BLOCK_NONE && secondaryBlockType != BLOCK_NONE;
}

void Game::combatGetTypeInfo(CombatType_t combatType, Creature* target, TextColor_t& color, uint8_t& effect) {
	switch (combatType) {
	case COMBAT_PHYSICALDAMAGE: {
		switch (target->getRace()) {
		case RACE_VENOM: color = TEXTCOLOR_LIGHTGREEN; effect = CONST_ME_HITBYPOISON; break;
		case RACE_BLOOD: color = TEXTCOLOR_RED; effect = CONST_ME_DRAWBLOOD; break;
		case RACE_UNDEAD: color = TEXTCOLOR_GREY; effect = CONST_ME_HITAREA; break;
		case RACE_FIRE: color = TEXTCOLOR_ORANGE; effect = CONST_ME_DRAWBLOOD; break;
		case RACE_ENERGY: color = TEXTCOLOR_PURPLE; effect = CONST_ME_ENERGYHIT; break;
		default: color = TEXTCOLOR_NONE; effect = CONST_ME_NONE; break;
		}
		if (color == TEXTCOLOR_RED && target->getTile() && !target->getTile()->hasFlag(TILESTATE_PROTECTIONZONE)) {
			std::unique_ptr<Item> splash = Item::CreateItem(ITEM_SMALLSPLASH, FLUID_BLOOD);
			internalAddItem(target->getTile(), splash.get(), INDEX_WHEREEVER, FLAG_NOLIMIT);
			startDecay(splash.get());
			splash.release(); // Ownership transferred to tile
		}
		break;
	}
	case COMBAT_ENERGYDAMAGE: color = TEXTCOLOR_PURPLE; effect = CONST_ME_ENERGYHIT; break;
	case COMBAT_EARTHDAMAGE: color = TEXTCOLOR_LIGHTGREEN; effect = CONST_ME_GREEN_RINGS; break;
	case COMBAT_DROWNDAMAGE: color = TEXTCOLOR_LIGHTBLUE; effect = CONST_ME_LOSEENERGY; break;
	case COMBAT_FIREDAMAGE: color = TEXTCOLOR_ORANGE; effect = CONST_ME_HITBYFIRE; break;
	case COMBAT_ICEDAMAGE: color = TEXTCOLOR_TEAL; effect = CONST_ME_ICEATTACK; break;
	case COMBAT_HOLYDAMAGE: color = TEXTCOLOR_YELLOW; effect = CONST_ME_HOLYDAMAGE; break;
	case COMBAT_DEATHDAMAGE: color = TEXTCOLOR_DARKRED; effect = CONST_ME_SMALLCLOUDS; break;
	case COMBAT_LIFEDRAIN: color = TEXTCOLOR_RED; effect = CONST_ME_MAGIC_RED; break;
	default: color = TEXTCOLOR_NONE; effect = CONST_ME_NONE; break;
	}
}

bool Game::combatChangeHealth(Creature* attacker, Creature* target, CombatDamage& damage) {
	std::lock_guard<std::mutex> lock(gameMutex);
	const Position& targetPos = target->getPosition();
	if (damage.primary.value > 0) {
		if (target->getHealth() <= 0) return false;

		auto attackerPlayer = attacker ? attacker->getPlayer() : nullptr;
		auto targetPlayer = target->getPlayer();
		if (attackerPlayer && targetPlayer && attackerPlayer->getSkull() == SKULL_BLACK && attackerPlayer->getSkullClient(targetPlayer) == SKULL_NONE) {
			return false;
		}

		if (damage.origin != ORIGIN_NONE) {
			const auto& events = target->getCreatureEvents(CREATURE_EVENT_HEALTHCHANGE);
			if (!events.empty()) {
				for (auto* event : events) {
					event->executeHealthChange(target, attacker, damage);
				}
				damage.origin = ORIGIN_NONE;
				return combatChangeHealth(attacker, target, damage);
			}
		}

		int32_t realHealthChange = target->getHealth();
		target->gainHealth(attacker, damage.primary.value);
		realHealthChange = target->getHealth() - realHealthChange;

		if (realHealthChange > 0 && !target->isInGhostMode()) {
			SpectatorVec spectators;
			map.getSpectators(spectators, targetPos, false, true);
			spectators.reserve(spectators.size());
			addAnimatedText(fmt::format("{}", realHealthChange), targetPos, TEXTCOLOR_MAYABLUE);

			std::string spectatorMessage;
			TextMessage message;
			for (auto* spectator : spectators) {
				auto tmpPlayer = spectator->getPlayer();
				if (!tmpPlayer) continue;

				message.type = MESSAGE_STATUS_DEFAULT;
				auto damageString = fmt::format("{:d} hitpoint{:s}", realHealthChange, realHealthChange != 1 ? "s" : "");
				if (tmpPlayer == attackerPlayer && attackerPlayer != targetPlayer) {
					message.text = fmt::format("You heal {:s} for {:s}.", target->getNameDescription(), damageString);
				}
				else if (tmpPlayer == targetPlayer) {
					message.text = !attacker ? fmt::format("You were healed for {:s}.", damageString) :
						(targetPlayer == attackerPlayer) ? fmt::format("You healed yourself for {:s}.", damageString) :
						fmt::format("You were healed by {:s} for {:s}.", attacker->getNameDescription(), damageString);
				}
				else {
					if (spectatorMessage.empty()) {
						spectatorMessage = !attacker ? fmt::format("{:s} was healed for {:s}", target->getNameDescription(), damageString) :
							(attacker == target) ? fmt::format("{:s} healed {:s}self for {:s}", attacker->getNameDescription(), targetPlayer ? (targetPlayer->getSex() == PLAYERSEX_FEMALE ? "her" : "him") : "it", damageString) :
							fmt::format("{:s} healed {:s} for {:s}", attacker->getNameDescription(), target->getNameDescription(), damageString);
						spectatorMessage[0] = std::toupper(spectatorMessage[0]);
					}
					message.text = spectatorMessage;
				}
				tmpPlayer->sendTextMessage(message);
			}
		}
	}
	else {
		if (!target->isAttackable()) {
			if (!target->isInGhostMode()) addMagicEffect(targetPos, CONST_ME_POFF);
			return true;
		}

		auto attackerPlayer = attacker ? attacker->getPlayer() : nullptr;
		auto targetPlayer = target->getPlayer();
		if (attackerPlayer && targetPlayer && attackerPlayer->getSkull() == SKULL_BLACK && attackerPlayer->getSkullClient(targetPlayer) == SKULL_NONE) {
			return false;
		}

		damage.primary.value = std::abs(damage.primary.value);
		damage.secondary.value = std::abs(damage.secondary.value);
		int32_t healthChange = damage.primary.value + damage.secondary.value;
		if (healthChange == 0) return true;

		SpectatorVec spectators;
		TextMessage message;

		if (targetPlayer && target->hasCondition(CONDITION_MANASHIELD) && damage.primary.type != COMBAT_UNDEFINEDDAMAGE) {
			int32_t manaDamage = std::min<int32_t>(targetPlayer->getMana(), healthChange);
			if (manaDamage != 0) {
				if (damage.origin != ORIGIN_NONE) {
					const auto& events = target->getCreatureEvents(CREATURE_EVENT_MANACHANGE);
					if (!events.empty()) {
						for (auto* event : events) {
							event->executeManaChange(target, attacker, damage);
						}
						healthChange = damage.primary.value + damage.secondary.value;
						if (healthChange == 0) return true;
						manaDamage = std::min<int32_t>(targetPlayer->getMana(), healthChange);
					}
				}

				targetPlayer->drainMana(attacker, manaDamage);
				map.getSpectators(spectators, targetPos, true, true);
				spectators.reserve(spectators.size());
				addMagicEffect(spectators, targetPos, CONST_ME_LOSEENERGY);
				addAnimatedText(fmt::format("{}", manaDamage), targetPos, TEXTCOLOR_BLUE);

				std::string spectatorMessage;
				for (auto* spectator : spectators) {
					auto tmpPlayer = spectator->getPlayer();
					if (tmpPlayer->getPosition().z != targetPos.z) continue;

					message.type = MESSAGE_STATUS_DEFAULT;
					if (tmpPlayer == attackerPlayer && attackerPlayer != targetPlayer) {
						message.text = fmt::format("{:s} loses {:d} mana due to your attack.", target->getNameDescription(), manaDamage);
						message.text[0] = std::toupper(message.text[0]);
					}
					else if (tmpPlayer == targetPlayer) {
						message.text = !attacker ? fmt::format("You lose {:d} mana.", manaDamage) :
							(targetPlayer == attackerPlayer) ? fmt::format("You lose {:d} mana due to your own attack.", manaDamage) :
							fmt::format("You lose {:d} mana due to an attack by {:s}.", manaDamage, attacker->getNameDescription());
					}
					else {
						if (spectatorMessage.empty()) {
							spectatorMessage = !attacker ? fmt::format("{:s} loses {:d} mana.", target->getNameDescription(), manaDamage) :
								(attacker == target) ? fmt::format("{:s} loses {:d} mana due to {:s} own attack.", target->getNameDescription(), manaDamage, targetPlayer->getSex() == PLAYERSEX_FEMALE ? "her" : "his") :
								fmt::format("{:s} loses {:d} mana due to an attack by {:s}.", target->getNameDescription(), manaDamage, attacker->getNameDescription());
							spectatorMessage[0] = std::toupper(spectatorMessage[0]);
						}
						message.text = spectatorMessage;
					}
					tmpPlayer->sendTextMessage(message);
				}

				damage.primary.value -= manaDamage;
				if (damage.primary.value < 0) {
					damage.secondary.value = std::max<int32_t>(0, damage.secondary.value + damage.primary.value);
					damage.primary.value = 0;
				}
			}
		}

		int32_t realDamage = damage.primary.value + damage.secondary.value;
		if (realDamage == 0) return true;

		if (damage.origin != ORIGIN_NONE) {
			const auto& events = target->getCreatureEvents(CREATURE_EVENT_HEALTHCHANGE);
			if (!events.empty()) {
				for (auto* event : events) {
					event->executeHealthChange(target, attacker, damage);
				}
				damage.origin = ORIGIN_NONE;
				return combatChangeHealth(attacker, target, damage);
			}
		}

		int32_t targetHealth = target->getHealth();
		if (damage.primary.value >= targetHealth) {
			damage.primary.value = targetHealth;
			damage.secondary.value = 0;
		}
		else if (damage.secondary.value) {
			damage.secondary.value = std::min<int32_t>(damage.secondary.value, targetHealth - damage.primary.value);
		}

		realDamage = damage.primary.value + damage.secondary.value;
		if (realDamage == 0) return true;

		if (spectators.empty()) {
			map.getSpectators(spectators, targetPos, true, true);
			spectators.reserve(spectators.size());
		}

		message.primary.value = damage.primary.value;
		message.secondary.value = damage.secondary.value;

		uint8_t hitEffect;
		if (message.primary.value) {
			combatGetTypeInfo(damage.primary.type, target, message.primary.color, hitEffect);
			if (hitEffect != CONST_ME_NONE) addMagicEffect(spectators, targetPos, hitEffect);
			if (message.primary.color != TEXTCOLOR_NONE) {
				addAnimatedText(fmt::format("{}", message.primary.value), targetPos, message.primary.color);
			}
		}

		if (message.secondary.value) {
			combatGetTypeInfo(damage.secondary.type, target, message.secondary.color, hitEffect);
			if (hitEffect != CONST_ME_NONE) addMagicEffect(spectators, targetPos, hitEffect);
			if (message.secondary.color != TEXTCOLOR_NONE) {
				addAnimatedText(fmt::format("{}", message.secondary.value), targetPos, message.secondary.color);
			}
		}

		if (message.primary.color != TEXTCOLOR_NONE || message.secondary.color != TEXTCOLOR_NONE) {
			auto damageString = fmt::format("{:d} hitpoint{:s}", realDamage, realDamage != 1 ? "s" : "");
			std::string spectatorMessage;

			for (auto* spectator : spectators) {
				auto tmpPlayer = spectator->getPlayer();
				if (tmpPlayer->getPosition().z != targetPos.z) continue;

				message.type = MESSAGE_STATUS_DEFAULT;
				if (tmpPlayer == attackerPlayer && attackerPlayer != targetPlayer) {
					message.text = fmt::format("{:s} loses {:s} due to your attack.", target->getNameDescription(), damageString);
					message.text[0] = std::toupper(message.text[0]);
				}
				else if (tmpPlayer == targetPlayer) {
					message.text = !attacker ? fmt::format("You lose {:s}.", damageString) :
						(targetPlayer == attackerPlayer) ? fmt::format("You lose {:s} due to your own attack.", damageString) :
						fmt::format("You lose {:s} due to an attack by {:s}.", damageString, attacker->getNameDescription());
				}
				else {
					if (spectatorMessage.empty()) {
						spectatorMessage = !attacker ? fmt::format("{:s} loses {:s}.", target->getNameDescription(), damageString) :
							(attacker == target) ? fmt::format("{:s} loses {:s} due to {:s} own attack.", target->getNameDescription(), damageString, targetPlayer ? (targetPlayer->getSex() == PLAYERSEX_FEMALE ? "her" : "his") : "its") :
							fmt::format("{:s} loses {:s} due to an attack by {:s}.", target->getNameDescription(), damageString, attacker->getNameDescription());
						spectatorMessage[0] = std::toupper(spectatorMessage[0]);
					}
					message.text = spectatorMessage;
				}
				tmpPlayer->sendTextMessage(message);
			}
		}

		if (realDamage >= targetHealth) {
			for (auto* event : target->getCreatureEvents(CREATURE_EVENT_PREPAREDEATH)) {
				if (!event->executeOnPrepareDeath(target, attacker)) return false;
			}
		}

		target->drainHealth(attacker, realDamage);
		addCreatureHealth(spectators, target);
	}
	return true;
}

bool Game::combatChangeMana(Creature* attacker, Creature* target, CombatDamage& damage) {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto targetPlayer = target->getPlayer();
	if (!targetPlayer) return true;

	int32_t manaChange = damage.primary.value + damage.secondary.value;
	if (manaChange > 0) {
		if (attacker) {
			auto attackerPlayer = attacker->getPlayer();
			if (attackerPlayer && attackerPlayer->getSkull() == SKULL_BLACK && attackerPlayer->getSkullClient(target) == SKULL_NONE) {
				return false;
			}
		}

		if (damage.origin != ORIGIN_NONE) {
			const auto& events = target->getCreatureEvents(CREATURE_EVENT_MANACHANGE);
			if (!events.empty()) {
				for (auto* event : events) {
					event->executeManaChange(target, attacker, damage);
				}
				damage.origin = ORIGIN_NONE;
				return combatChangeMana(attacker, target, damage);
			}
		}

		targetPlayer->changeMana(manaChange);
	}
	else {
		const Position& targetPos = target->getPosition();
		if (!target->isAttackable()) {
			if (!target->isInGhostMode()) addMagicEffect(targetPos, CONST_ME_POFF);
			return false;
		}

		auto attackerPlayer = attacker ? attacker->getPlayer() : nullptr;
		if (attackerPlayer && targetPlayer && attackerPlayer->getSkull() == SKULL_BLACK && attackerPlayer->getSkullClient(targetPlayer) == SKULL_NONE) {
			return false;
		}

		int32_t manaLoss = std::min<int32_t>(targetPlayer->getMana(), -manaChange);
		BlockType_t blockType = target->blockHit(attacker, COMBAT_MANADRAIN, manaLoss);
		if (blockType != BLOCK_NONE) {
			addMagicEffect(targetPos, CONST_ME_POFF);
			return false;
		}

		if (manaLoss <= 0) return true;

		if (damage.origin != ORIGIN_NONE) {
			const auto& events = target->getCreatureEvents(CREATURE_EVENT_MANACHANGE);
			if (!events.empty()) {
				for (auto* event : events) {
					event->executeManaChange(target, attacker, damage);
				}
				damage.origin = ORIGIN_NONE;
				return combatChangeMana(attacker, target, damage);
			}
		}

		targetPlayer->drainMana(attacker, manaLoss);
		SpectatorVec spectators;
		map.getSpectators(spectators, targetPos, false, true);
		spectators.reserve(spectators.size());
		addAnimatedText(fmt::format("{}", manaLoss), targetPos, TEXTCOLOR_BLUE);

		std::string spectatorMessage;
		TextMessage message;
		for (auto* spectator : spectators) {
			auto tmpPlayer = spectator->getPlayer();
			message.type = MESSAGE_STATUS_DEFAULT;
			if (tmpPlayer == attackerPlayer && attackerPlayer != targetPlayer) {
				message.text = fmt::format("{:s} loses {:d} mana due to your attack.", target->getNameDescription(), manaLoss);
				message.text[0] = std::toupper(message.text[0]);
			}
			else if (tmpPlayer == targetPlayer) {
				message.text = !attacker ? fmt::format("You lose {:d} mana.", manaLoss) :
					(targetPlayer == attackerPlayer) ? fmt::format("You lose {:d} mana due to your own attack.", manaLoss) :
					fmt::format("You lose {:d} mana due to an attack by {:s}.", manaLoss, attacker->getNameDescription());
			}
			else {
				if (spectatorMessage.empty()) {
					spectatorMessage = !attacker ? fmt::format("{:s} loses {:d} mana.", target->getNameDescription(), manaLoss) :
						(attacker == target) ? fmt::format("{:s} loses {:d} mana due to {:s} own attack.", target->getNameDescription(), manaLoss, targetPlayer->getSex() == PLAYERSEX_FEMALE ? "her" : "his") :
						fmt::format("{:s} loses {:d} mana due to an attack by {:s}.", target->getNameDescription(), manaLoss, attacker->getNameDescription());
					spectatorMessage[0] = std::toupper(spectatorMessage[0]);
				}
				message.text = spectatorMessage;
			}
			tmpPlayer->sendTextMessage(message);
		}
	}
	return true;
}

void Game::addCreatureHealth(const Creature* target) {
	SpectatorVec spectators;
	map.getSpectators(spectators, target->getPosition(), true, true);
	spectators.reserve(spectators.size());
	addCreatureHealth(spectators, target);
}

void Game::addCreatureHealth(const SpectatorVec& spectators, const Creature* target) {
	for (auto* spectator : spectators) {
		if (auto tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendCreatureHealth(target);
		}
	}
}

void Game::addAnimatedText(const std::string& message, const Position& pos, TextColor_t color) {
	SpectatorVec spectators;
	map.getSpectators(spectators, pos, true, true);
	if (spectators.empty()) return;
	spectators.reserve(spectators.size());
	addAnimatedText(spectators, message, pos, color);
}

void Game::addAnimatedText(const SpectatorVec& spectators, const std::string& message, const Position& pos, TextColor_t color) {
	for (auto* spectator : spectators) {
		if (auto tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendAnimatedText(message, pos, color);
		}
	}
}

void Game::addMagicEffect(const Position& pos, uint8_t effect) {
	SpectatorVec spectators;
	map.getSpectators(spectators, pos, true, true);
	spectators.reserve(spectators.size());
	addMagicEffect(spectators, pos, effect);
}

void Game::addMagicEffect(const SpectatorVec& spectators, const Position& pos, uint8_t effect) {
	for (auto* spectator : spectators) {
		if (auto tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendMagicEffect(pos, effect);
		}
	}
}

void Game::addDistanceEffect(const Position& fromPos, const Position& toPos, uint8_t effect) {
	SpectatorVec spectators, toPosSpectators;
	map.getSpectators(spectators, fromPos, true, true);
	map.getSpectators(toPosSpectators, toPos, true, true);
	spectators.addSpectators(toPosSpectators);
	spectators.reserve(spectators.size());
	addDistanceEffect(spectators, fromPos, toPos, effect);
}

void Game::addDistanceEffect(const SpectatorVec& spectators, const Position& fromPos, const Position& toPos, uint8_t effect) {
	for (auto* spectator : spectators) {
		if (auto tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendDistanceShoot(fromPos, toPos, effect);
		}
	}
}
void Game::playerInviteToParty(uint32_t playerId, uint32_t invitedId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (playerId == invitedId) return;

	Player* player = getPlayerByID(playerId);
	if (!player) return;

	Player* invitedPlayer = getPlayerByID(invitedId);
	if (!invitedPlayer || invitedPlayer->isInviting(player)) return;

	if (invitedPlayer->getParty()) {
		player->sendTextMessage(MESSAGE_INFO_DESCR, fmt::format("{:s} is already in a party.", invitedPlayer->getName()));
		return;
	}

	Party* party = player->getParty();
	if (!party) {
		party = new Party(player); // Raw pointer as per original; consider std::unique_ptr in future
	}
	else if (party->getLeader() != player) {
		return;
	}
	party->invitePlayer(*invitedPlayer);
}

void Game::playerJoinParty(uint32_t playerId, uint32_t leaderId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	Player* leader = getPlayerByID(leaderId);
	if (!leader || !leader->isInviting(player)) return;

	Party* party = leader->getParty();
	if (!party || party->getLeader() != leader) return;

	if (player->getParty()) {
		player->sendTextMessage(MESSAGE_INFO_DESCR, "You are already in a party.");
		return;
	}
	party->joinParty(*player);
}

void Game::playerRevokePartyInvitation(uint32_t playerId, uint32_t invitedId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	Party* party = player->getParty();
	if (!party || party->getLeader() != player) return;

	Player* invitedPlayer = getPlayerByID(invitedId);
	if (!invitedPlayer || !player->isInviting(invitedPlayer)) return;

	party->revokeInvitation(*invitedPlayer);
}

void Game::playerPassPartyLeadership(uint32_t playerId, uint32_t newLeaderId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	Party* party = player->getParty();
	if (!party || party->getLeader() != player) return;

	Player* newLeader = getPlayerByID(newLeaderId);
	if (!newLeader || !player->isPartner(newLeader)) return;

	party->passPartyLeadership(newLeader);
}

void Game::playerLeaveParty(uint32_t playerId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	Party* party = player->getParty();
	if (!party || player->hasCondition(CONDITION_INFIGHT)) return;

	party->leaveParty(player);
}

void Game::playerEnableSharedPartyExperience(uint32_t playerId, bool sharedExpActive) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	Party* party = player->getParty();
	if (!party || (player->hasCondition(CONDITION_INFIGHT) && player->getZone() != ZONE_PROTECTION)) return;

	party->setSharedExperience(player, sharedExpActive);
}

void Game::sendGuildMotd(uint32_t playerId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	if (Guild* guild = player->getGuild()) {
		player->sendChannelMessage("Message of the Day", guild->getMotd(), TALKTYPE_CHANNEL_R1, CHANNEL_GUILD);
	}
}

void Game::kickPlayer(uint32_t playerId, bool displayEffect) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	player->kickPlayer(displayEffect);
}

void Game::playerReportRuleViolation(uint32_t playerId, const std::string& targetName, uint8_t reportType, uint8_t reportReason, const std::string& comment, const std::string& translation) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	g_events->eventPlayerOnReportRuleViolation(player, targetName, reportType, reportReason, comment, translation);
}

void Game::playerReportBug(uint32_t playerId, const std::string& message) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	g_events->eventPlayerOnReportBug(player, message);
}

void Game::playerDebugAssert(uint32_t playerId, const std::string& assertLine, const std::string& date, const std::string& description, const std::string& comment) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	FILE* file = fopen("client_assertions.txt", "a");
	if (file) {
		fprintf(file, "----- %s - %s (%s) -----\n%s\n%s\n%s\n%s\n",
			formatDate(time(nullptr)).c_str(),
			player->getName().c_str(),
			convertIPToString(player->getIP()).c_str(),
			assertLine.c_str(),
			date.c_str(),
			description.c_str(),
			comment.c_str());
		fclose(file);
	}
}

void Game::parsePlayerExtendedOpcode(uint32_t playerId, uint8_t opcode, const std::string& buffer) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Player* player = getPlayerByID(playerId);
	if (!player) return;

	for (CreatureEvent* creatureEvent : player->getCreatureEvents(CREATURE_EVENT_EXTENDED_OPCODE)) {
		creatureEvent->executeExtendedOpcode(player, opcode, buffer);
	}
}

void Game::forceAddCondition(uint32_t creatureId, Condition* condition) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Creature* creature = getCreatureByID(creatureId);
	if (!creature) {
		delete condition; // Raw pointer as per original; consider smart pointer in future
		return;
	}
	creature->addCondition(condition, true);
}

void Game::forceRemoveCondition(uint32_t creatureId, ConditionType_t type) {
	std::lock_guard<std::mutex> lock(gameMutex);
	Creature* creature = getCreatureByID(creatureId);
	if (!creature) return;

	creature->removeCondition(type, true);
}
void Game::setAccountStorageValue(uint32_t accountId, uint32_t key, int32_t value) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (value == -1) {
		accountStorageMap[accountId].erase(key);
		if (accountStorageMap[accountId].empty()) {
			accountStorageMap.erase(accountId);
		}
	}
	else {
		accountStorageMap[accountId][key] = value;
	}
}

int32_t Game::getAccountStorageValue(uint32_t accountId, uint32_t key) const {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto accountIt = accountStorageMap.find(accountId);
	if (accountIt != accountStorageMap.end()) {
		auto storageIt = accountIt->second.find(key);
		if (storageIt != accountIt->second.end()) {
			return storageIt->second;
		}
	}
	return -1;
}

void Game::loadAccountStorageValues() {
	std::lock_guard<std::mutex> lock(gameMutex);
	Database& db = Database::getInstance();
	if (DBResult_ptr result = db.storeQuery("SELECT `account_id`, `key`, `value` FROM `account_storage`")) {
		do {
			setAccountStorageValue(result->getNumber<uint32_t>("account_id"),
				result->getNumber<uint32_t>("key"),
				result->getNumber<int32_t>("value"));
		} while (result->next());
	}
}

bool Game::saveAccountStorageValues() const {
	std::lock_guard<std::mutex> lock(gameMutex);
	DBTransaction transaction;
	Database& db = Database::getInstance();

	if (!transaction.begin() || !db.executeQuery("DELETE FROM `account_storage`")) {
		return false;
	}

	for (const auto& [accountId, storage] : accountStorageMap) {
		if (storage.empty()) continue;

		DBInsert accountStorageQuery("INSERT INTO `account_storage` (`account_id`, `key`, `value`) VALUES");
		for (const auto& [key, value] : storage) {
			if (!accountStorageQuery.addRow(fmt::format("{:d}, {:d}, {:d}", accountId, key, value))) {
				return false;
			}
		}
		if (!accountStorageQuery.execute()) {
			return false;
		}
	}
	return transaction.commit();
}

void Game::startDecay(Item* item) {
	std::lock_guard<std::mutex> lock(decayMutex);
	if (!item || !item->canDecay() || item->getDecaying() == DECAYING_TRUE) return;

	if (item->getDuration() > 0) {
		item->incrementReferenceCounter();
		item->setDecaying(DECAYING_TRUE);
		toDecayItems.push_front(item);
	}
	else {
		internalDecayItem(item);
	}
}

void Game::internalDecayItem(Item* item) {
	std::lock_guard<std::mutex> lock(gameMutex);
	const ItemType& it = Item::items[item->getID()];
	if (it.decayTo != 0) {
		Item* newItem = transformItem(item, it.decayTo);
		startDecay(newItem);
	}
	else {
		ReturnValue ret = internalRemoveItem(item);
		if (ret != RETURNVALUE_NOERROR) {
			std::cout << "[Debug - Game::internalDecayItem] Failed with error code: " << static_cast<uint32_t>(ret) << ", item id: " << item->getID() << std::endl;
		}
	}
}

void Game::checkDecay() {
	std::lock_guard<std::mutex> lock(decayMutex);
	g_scheduler.addEvent(createSchedulerTask(EVENT_DECAYINTERVAL, [this]() { checkDecay(); }));
	size_t bucket = (lastBucket + 1) % EVENT_DECAY_BUCKETS;
	auto& bucketList = decayItems[bucket];

	for (auto it = bucketList.begin(); it != bucketList.end();) {
		Item* item = *it;
		if (!item->canDecay()) {
			item->setDecaying(DECAYING_FALSE);
			ReleaseItem(item);
			it = bucketList.erase(it);
			continue;
		}

		int32_t duration = item->getDuration();
		int32_t decreaseTime = std::min<int32_t>(EVENT_DECAYINTERVAL * EVENT_DECAY_BUCKETS, duration);
		duration -= decreaseTime;
		item->decreaseDuration(decreaseTime);

		if (duration <= 0) {
			it = bucketList.erase(it);
			internalDecayItem(item);
			ReleaseItem(item);
		}
		else if (duration < EVENT_DECAYINTERVAL * EVENT_DECAY_BUCKETS) {
			it = bucketList.erase(it);
			size_t newBucket = (bucket + ((duration + EVENT_DECAYINTERVAL / 2) / 1000)) % EVENT_DECAY_BUCKETS;
			if (newBucket == bucket) {
				internalDecayItem(item);
				ReleaseItem(item);
			}
			else {
				decayItems[newBucket].push_back(item);
			}
		}
		else {
			++it;
		}
	}
	lastBucket = bucket;
	cleanup();
}

void Game::checkLight() {
	std::lock_guard<std::mutex> lock(gameMutex);
	g_scheduler.addEvent(createSchedulerTask(EVENT_LIGHTINTERVAL, [this]() { checkLight(); }));
	uint8_t previousLightLevel = lightLevel;
	updateWorldLightLevel();
	if (previousLightLevel != lightLevel) {
		LightInfo lightInfo = getWorldLightInfo();
		for (const auto& [id, player] : players) {
			player->sendWorldLight(lightInfo);
		}
	}
}

void Game::updateWorldLightLevel() {
	if (getWorldTime() >= GAME_SUNRISE && getWorldTime() <= GAME_DAYTIME) {
		lightLevel = ((GAME_DAYTIME - GAME_SUNRISE) - (GAME_DAYTIME - getWorldTime())) * float(LIGHT_CHANGE_SUNRISE) + LIGHT_NIGHT;
	}
	else if (getWorldTime() >= GAME_SUNSET && getWorldTime() <= GAME_NIGHTTIME) {
		lightLevel = LIGHT_DAY - ((getWorldTime() - GAME_SUNSET) * float(LIGHT_CHANGE_SUNSET));
	}
	else if (getWorldTime() >= GAME_NIGHTTIME || getWorldTime() < GAME_SUNRISE) {
		lightLevel = LIGHT_NIGHT;
	}
	else {
		lightLevel = LIGHT_DAY;
	}
}

void Game::updateWorldTime() {
	g_scheduler.addEvent(createSchedulerTask(EVENT_WORLDTIMEINTERVAL, [this]() { updateWorldTime(); }));
	time_t osTime = time(nullptr);
	tm* timeInfo = localtime(&osTime);
	worldTime = (timeInfo->tm_sec + (timeInfo->tm_min * 60)) / 2.5f;
}

void Game::shutdown() {
	std::lock_guard<std::mutex> lock(gameMutex);
	std::cout << "Shutting down..." << std::flush;
	g_scheduler.shutdown();
	g_databaseTasks.shutdown();
	g_dispatcher.shutdown();
	map.spawns.clear();
	raids.clear();
	cleanup();
	if (serviceManager) {
		serviceManager->stop();
	}
	ConnectionManager::getInstance().closeAll();
	std::cout << " done!" << std::endl;
}

void Game::cleanup() {
	std::lock_guard<std::mutex> lock(gameMutex);
	for (auto* creature : ToReleaseCreatures) {
		creature->decrementReferenceCounter();
	}
	ToReleaseCreatures.clear();

	for (auto* item : ToReleaseItems) {
		item->decrementReferenceCounter();
	}
	ToReleaseItems.clear();

	std::lock_guard<std::mutex> decayLock(decayMutex);
	for (Item* item : toDecayItems) {
		uint32_t dur = item->getDuration();
		if (dur >= EVENT_DECAYINTERVAL * EVENT_DECAY_BUCKETS) {
			decayItems[lastBucket].push_back(item);
		}
		else {
			decayItems[(lastBucket + 1 + dur / 1000) % EVENT_DECAY_BUCKETS].push_back(item);
		}
	}
	toDecayItems.clear();
}

void Game::ReleaseCreature(Creature* creature) {
	std::lock_guard<std::mutex> lock(gameMutex);
	ToReleaseCreatures.push_back(creature);
}

void Game::ReleaseItem(Item* item) {
	std::lock_guard<std::mutex> lock(gameMutex);
	ToReleaseItems.push_back(item);
}

void Game::broadcastMessage(const std::string& text, MessageClasses type) const {
	std::lock_guard<std::mutex> lock(gameMutex);
	std::cout << "> Broadcasted message: \"" << text << "\"." << std::endl;
	for (const auto& [id, player] : players) {
		player->sendTextMessage(type, text);
	}
}

void Game::updateCreatureWalkthrough(const Creature* creature) {
	std::lock_guard<std::mutex> lock(gameMutex);
	SpectatorVec spectators;
	map.getSpectators(spectators, creature->getPosition(), true, true);
	spectators.reserve(spectators.size());
	for (auto* spectator : spectators) {
		if (Player* tmpPlayer = spectator->getPlayer()) {
			tmpPlayer->sendCreatureWalkthrough(creature, tmpPlayer->canWalkthroughEx(creature));
		}
	}
}

void Game::updateCreatureSkull(const Creature* creature) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (getWorldType() != WORLD_TYPE_PVP) return;

	SpectatorVec spectators;
	map.getSpectators(spectators, creature->getPosition(), true, true);
	spectators.reserve(spectators.size());
	for (auto* spectator : spectators) {
		spectator->getPlayer()->sendCreatureSkull(creature);
	}
}

void Game::updatePlayerShield(Player* player) {
	std::lock_guard<std::mutex> lock(gameMutex);
	SpectatorVec spectators;
	map.getSpectators(spectators, player->getPosition(), true, true);
	spectators.reserve(spectators.size());
	for (auto* spectator : spectators) {
		spectator->getPlayer()->sendCreatureShield(player);
	}
}

void Game::loadMotdNum() {
	std::lock_guard<std::mutex> lock(gameMutex);
	Database& db = Database::getInstance();
	if (DBResult_ptr result = db.storeQuery("SELECT `value` FROM `server_config` WHERE `config` = 'motd_num'")) {
		motdNum = result->getNumber<uint32_t>("value");
	}
	else {
		db.executeQuery("INSERT INTO `server_config` (`config`, `value`) VALUES ('motd_num', '0')");
	}

	if (DBResult_ptr result = db.storeQuery("SELECT `value` FROM `server_config` WHERE `config` = 'motd_hash'")) {
		motdHash = result->getString("value");
		if (motdHash != transformToSHA1(g_config.getString(ConfigManager::MOTD))) {
			++motdNum;
		}
	}
	else {
		db.executeQuery("INSERT INTO `server_config` (`config`, `value`) VALUES ('motd_hash', '')");
	}
}

void Game::saveMotdNum() const {
	std::lock_guard<std::mutex> lock(gameMutex);
	Database& db = Database::getInstance();
	db.executeQuery(fmt::format("UPDATE `server_config` SET `value` = '{:d}' WHERE `config` = 'motd_num'", motdNum));
	db.executeQuery(fmt::format("UPDATE `server_config` SET `value` = '{:s}' WHERE `config` = 'motd_hash'", transformToSHA1(g_config.getString(ConfigManager::MOTD))));
}

void Game::checkPlayersRecord() {
	std::lock_guard<std::mutex> lock(gameMutex);
	size_t playersOnline = players.size();
	if (playersOnline > playersRecord) {
		uint32_t previousRecord = playersRecord;
		playersRecord = playersOnline;
		for (auto& [_, event] : g_globalEvents->getEventMap(GLOBALEVENT_RECORD)) {
			event.executeRecord(playersRecord, previousRecord);
		}
		updatePlayersRecord();
	}
}

void Game::updatePlayersRecord() const {
	std::lock_guard<std::mutex> lock(gameMutex);
	Database& db = Database::getInstance();
	db.executeQuery(fmt::format("UPDATE `server_config` SET `value` = '{:d}' WHERE `config` = 'players_record'", playersRecord));
}

void Game::loadPlayersRecord() {
	std::lock_guard<std::mutex> lock(gameMutex);
	Database& db = Database::getInstance();
	if (DBResult_ptr result = db.storeQuery("SELECT `value` FROM `server_config` WHERE `config` = 'players_record'")) {
		playersRecord = result->getNumber<uint32_t>("value");
	}
	else {
		db.executeQuery("INSERT INTO `server_config` (`config`, `value`) VALUES ('players_record', '0')");
	}
}

void Game::addPlayer(Player* player) {
	std::lock_guard<std::mutex> lock(gameMutex);
	const std::string& lowercase_name = asLowerCaseString(player->getName());
	mappedPlayerNames[lowercase_name] = player;
	mappedPlayerGuids[player->getGUID()] = player;
	wildcardTree.insert(lowercase_name);
	players[player->getID()] = player;
}

void Game::removePlayer(Player* player) {
	std::lock_guard<std::mutex> lock(gameMutex);
	const std::string& lowercase_name = asLowerCaseString(player->getName());
	mappedPlayerNames.erase(lowercase_name);
	mappedPlayerGuids.erase(player->getGUID());
	wildcardTree.remove(lowercase_name);
	players.erase(player->getID());
}

void Game::addNpc(Npc* npc) {
	std::lock_guard<std::mutex> lock(gameMutex);
	npcs[npc->getID()] = npc;
}

void Game::removeNpc(Npc* npc) {
	std::lock_guard<std::mutex> lock(gameMutex);
	npcs.erase(npc->getID());
}

void Game::addMonster(Monster* monster) {
	std::lock_guard<std::mutex> lock(gameMutex);
	monsters[monster->getID()] = monster;
}

void Game::removeMonster(Monster* monster) {
	std::lock_guard<std::mutex> lock(gameMutex);
	monsters.erase(monster->getID());
}

Guild* Game::getGuild(uint32_t id) const {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto it = guilds.find(id);
	return (it != guilds.end()) ? it->second : nullptr;
}

void Game::addGuild(Guild* guild) {
	std::lock_guard<std::mutex> lock(gameMutex);
	guilds[guild->getId()] = guild;
}

void Game::removeGuild(uint32_t guildId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	guilds.erase(guildId);
}

void Game::internalRemoveItems(std::vector<Item*> itemList, uint32_t amount, bool stackable) {
	std::lock_guard<std::mutex> lock(gameMutex);
	if (stackable) {
		for (Item* item : itemList) {
			if (item->getItemCount() > amount) {
				internalRemoveItem(item, amount);
				break;
			}
			else {
				amount -= item->getItemCount();
				internalRemoveItem(item);
			}
		}
	}
	else {
		for (Item* item : itemList) {
			internalRemoveItem(item);
		}
	}
}

BedItem* Game::getBedBySleeper(uint32_t guid) const {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto it = bedSleepersMap.find(guid);
	return (it != bedSleepersMap.end()) ? it->second : nullptr;
}

void Game::setBedSleeper(BedItem* bed, uint32_t guid) {
	std::lock_guard<std::mutex> lock(gameMutex);
	bedSleepersMap[guid] = bed;
}

void Game::removeBedSleeper(uint32_t guid) {
	std::lock_guard<std::mutex> lock(gameMutex);
	bedSleepersMap.erase(guid);
}

Item* Game::getUniqueItem(uint16_t uniqueId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto it = uniqueItems.find(uniqueId);
	return (it != uniqueItems.end()) ? it->second : nullptr;
}

bool Game::addUniqueItem(uint16_t uniqueId, Item* item) {
	std::lock_guard<std::mutex> lock(gameMutex);
	auto result = uniqueItems.emplace(uniqueId, item);
	if (!result.second) {
		std::cout << "Duplicate unique id: " << uniqueId << std::endl;
	}
	return result.second;
}

void Game::removeUniqueItem(uint16_t uniqueId) {
	std::lock_guard<std::mutex> lock(gameMutex);
	uniqueItems.erase(uniqueId);
}

bool Game::reload(ReloadTypes_t reloadType) {
	std::lock_guard<std::mutex> lock(gameMutex);
	switch (reloadType) {
	case RELOAD_TYPE_ACTIONS: return g_actions->reload();
	case RELOAD_TYPE_CHAT: return g_chat->load();
	case RELOAD_TYPE_CONFIG: return g_config.reload();
	case RELOAD_TYPE_CREATURESCRIPTS:
		g_creatureEvents->reload();
		g_creatureEvents->removeInvalidEvents();
		return true;
	case RELOAD_TYPE_EVENTS: return g_events->load();
	case RELOAD_TYPE_GLOBALEVENTS: return g_globalEvents->reload();
	case RELOAD_TYPE_ITEMS: return Item::items.reload();
	case RELOAD_TYPE_MONSTERS: return g_monsters.reload();
	case RELOAD_TYPE_MOVEMENTS: return g_moveEvents->reload();
	case RELOAD_TYPE_NPCS: Npcs::reload(); return true;
	case RELOAD_TYPE_QUESTS: return quests.reload();
	case RELOAD_TYPE_RAIDS: return raids.reload() && raids.startup();
	case RELOAD_TYPE_SPELLS:
		if (!g_spells->reload() || !g_monsters.reload()) {
			std::cout << "[Error - Game::reload] Failed to reload spells or monsters." << std::endl;
			std::terminate();
		}
		return true;
	case RELOAD_TYPE_TALKACTIONS: return g_talkActions->reload();
	case RELOAD_TYPE_WEAPONS:
		g_weapons->reload();
		g_weapons->loadDefaults();
		return true;
	case RELOAD_TYPE_SCRIPTS:
		g_actions->clear(true);
		g_creatureEvents->clear(true);
		g_moveEvents->clear(true);
		g_talkActions->clear(true);
		g_globalEvents->clear(true);
		g_weapons->clear(true);
		g_weapons->loadDefaults();
		g_spells->clear(true);
		g_scripts->loadScripts("scripts", false, true);
		g_creatureEvents->removeInvalidEvents();
		return true;
	default:
		if (!g_spells->reload() || !g_monsters.reload()) {
			std::cout << "[Error - Game::reload] Failed to reload spells or monsters." << std::endl;
			std::terminate();
		}
		g_actions->reload();
		g_config.reload();
		g_creatureEvents->reload();
		g_monsters.reload();
		g_moveEvents->reload();
		Npcs::reload();
		raids.reload() && raids.startup();
		g_talkActions->reload();
		Item::items.reload();
		g_weapons->reload();
		g_weapons->clear(true);
		g_weapons->loadDefaults();
		quests.reload();
		g_globalEvents->reload();
		g_events->load();
		g_chat->load();
		g_actions->clear(true);
		g_creatureEvents->clear(true);
		g_moveEvents->clear(true);
		g_talkActions->clear(true);
		g_globalEvents->clear(true);
		g_spells->clear(true);
		g_scripts->loadScripts("scripts", false, true);
		g_creatureEvents->removeInvalidEvents();
		return true;
	}
	return true;
}
