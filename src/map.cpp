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
#include "iomap.h"
#include "iomapserialize.h"
#include "combat.h"
#include "creature.h"
#include "game.h"
#include "monster.h"

#include <fmt/format.h>
#include <memory>   // For unique_ptr
#include <mutex>    // For thread safety
#include <random>   // For std::shuffle
#include <vector>

extern Game g_game;

class Map {
private:
	static constexpr uint8_t MAP_MAX_LAYERS = 16; // Assumed value, adjust as needed
	static constexpr int32_t maxViewportX = 11;   // From original code context
	static constexpr int32_t maxViewportY = 11;
	static constexpr uint32_t FLOOR_SIZE = 32;    // Assumed from FLOOR_MASK usage
	static constexpr uint32_t FLOOR_MASK = FLOOR_SIZE - 1;

	QTreeNode root;
	std::map<Position, SpectatorVec> spectatorCache;
	std::map<Position, SpectatorVec> playersSpectatorCache;
	mutable std::mutex spectatorMutex; // Thread safety for caches

public:
	bool loadMap(const std::string& identifier, bool loadHouses);
	bool save();
	Tile* getTile(uint16_t x, uint16_t y, uint8_t z) const;
	void setTile(uint16_t x, uint16_t y, uint8_t z, Tile* newTile);
	void removeTile(uint16_t x, uint16_t y, uint8_t z);
	bool placeCreature(const Position& centerPos, Creature* creature, bool extendedPos = false, bool forceLogin = false);
	void moveCreature(Creature& creature, Tile& newTile, bool forceTeleport = false);
	void getSpectators(SpectatorVec& spectators, const Position& centerPos, bool multifloor = false, bool onlyPlayers = false,
		int32_t minRangeX = 0, int32_t maxRangeX = 0, int32_t minRangeY = 0, int32_t maxRangeY = 0);
	void clearSpectatorCache();
	void clearPlayersSpectatorCache();
	bool canThrowObjectTo(const Position& fromPos, const Position& toPos, bool checkLineOfSight = true, bool sameFloor = false,
		int32_t rangex = maxViewportX, int32_t rangey = maxViewportY) const;
	bool isSightClear(const Position& fromPos, const Position& toPos, bool sameFloor = false) const;
	const Tile* canWalkTo(const Creature& creature, const Position& pos) const;
	bool getPathMatching(const Creature& creature, std::vector<Direction>& dirList, const FrozenPathingConditionCall& pathCondition,
		const FindPathParams& fpp) const;
	uint32_t clean() const;

private:
	void getSpectatorsInternal(SpectatorVec& spectators, const Position& centerPos, int32_t minRangeX, int32_t maxRangeX,
		int32_t minRangeY, int32_t maxRangeY, int32_t minRangeZ, int32_t maxRangeZ, bool onlyPlayers) const;
	QTreeLeafNode* getQTNode(uint16_t x, uint16_t y) { return root.getLeaf(x, y); }
	bool isTileClear(uint16_t x, uint16_t y, uint8_t z, bool blockFloor = false) const;
	bool checkSightLine(uint16_t x0, uint16_t y0, uint16_t x1, uint16_t y1, uint8_t z) const;
};

// Supporting classes (Floor, QTreeNode, etc.) follow...
bool Map::loadMap(const std::string& identifier, bool loadHouses)
{
	IOMap loader;
	if (!loader.loadMap(this, identifier)) {
		std::cout << "[Fatal - Map::loadMap] " << loader.getLastErrorString() << std::endl;
		return false;
	}

	if (!IOMap::loadSpawns(this)) {
		std::cout << "[Warning - Map::loadMap] Failed to load spawn data." << std::endl;
	}

	if (loadHouses) {
		if (!IOMap::loadHouses(this)) {
			std::cout << "[Warning - Map::loadMap] Failed to load house data." << std::endl;
		}

		IOMapSerialize::loadHouseInfo();
		IOMapSerialize::loadHouseItems(this);
	}
	return true;
}

bool Map::save()
{
	bool saved = false;
	for (uint32_t tries = 0; tries < 3; ++tries) {
		if (IOMapSerialize::saveHouseInfo()) {
			saved = true;
			break;
		}
	}

	if (!saved) {
		return false;
	}

	saved = false;
	for (uint32_t tries = 0; tries < 3; ++tries) {
		if (IOMapSerialize::saveHouseItems()) {
			saved = true;
			break;
		}
	}
	return saved;
}

Tile* Map::getTile(uint16_t x, uint16_t y, uint8_t z) const
{
	if (z >= MAP_MAX_LAYERS) {
		return nullptr;
	}

	const QTreeLeafNode* leaf = QTreeNode::getLeafStatic<const QTreeLeafNode*, const QTreeNode*>(&root, x, y);
	if (!leaf) {
		return nullptr;
	}

	const Floor* floor = leaf->getFloor(z);
	if (!floor) {
		return nullptr;
	}
	return floor->tiles[x & FLOOR_MASK][y & FLOOR_MASK];
}

void Map::setTile(uint16_t x, uint16_t y, uint8_t z, Tile* newTile)
{
	if (z >= MAP_MAX_LAYERS) {
		std::cout << "ERROR: Attempt to set tile on invalid coordinate " << Position(x, y, z) << "!" << std::endl;
		return;
	}

	QTreeLeafNode::newLeaf = false;
	QTreeLeafNode* leaf = root.createLeaf(x, y, 15);

	if (QTreeLeafNode::newLeaf) {
		if (QTreeLeafNode* northLeaf = root.getLeaf(x, y - FLOOR_SIZE)) {
			northLeaf->leafS = leaf;
		}
		if (QTreeLeafNode* westLeaf = root.getLeaf(x - FLOOR_SIZE, y)) {
			westLeaf->leafE = leaf;
		}
		if (QTreeLeafNode* southLeaf = root.getLeaf(x, y + FLOOR_SIZE)) {
			leaf->leafS = southLeaf;
		}
		if (QTreeLeafNode* eastLeaf = root.getLeaf(x + FLOOR_SIZE, y)) {
			leaf->leafE = eastLeaf;
		}
	}

	Floor* floor = leaf->createFloor(z);
	uint32_t offsetX = x & FLOOR_MASK;
	uint32_t offsetY = y & FLOOR_MASK;

	std::unique_ptr<Tile>& tile = floor->tiles[offsetX][offsetY];
	if (tile) {
		TileItemVector* items = newTile->getItemList();
		if (items) {
			for (auto it = items->rbegin(), end = items->rend(); it != end; ++it) {
				tile->addThing(*it);
			}
			items->clear();
		}

		Item* ground = newTile->getGround();
		if (ground) {
			tile->addThing(ground);
			newTile->setGround(nullptr);
		}
		delete newTile; // Assuming newTile is not managed elsewhere
	}
	else {
		tile.reset(newTile);
	}
}

void Map::removeTile(uint16_t x, uint16_t y, uint8_t z)
{
	if (z >= MAP_MAX_LAYERS) {
		return;
	}

	const QTreeLeafNode* leaf = QTreeNode::getLeafStatic<const QTreeLeafNode*, const QTreeNode*>(&root, x, y);
	if (!leaf) {
		return;
	}

	const Floor* floor = leaf->getFloor(z);
	if (!floor) {
		return;
	}

	std::unique_ptr<Tile>& tile = floor->tiles[x & FLOOR_MASK][y & FLOOR_MASK];
	if (tile) {
		if (const CreatureVector* creatures = tile->getCreatures()) {
			for (int32_t i = creatures->size() - 1; i >= 0; --i) {
				if (Player* player = (*creatures)[i]->getPlayer()) {
					g_game.internalTeleport(player, player->getTown()->getTemplePosition(), false, FLAG_NOLIMIT);
				}
				else {
					g_game.removeCreature((*creatures)[i]);
				}
			}
		}

		if (TileItemVector* items = tile->getItemList()) {
			for (Item* item : *items) {
				g_game.internalRemoveItem(item);
			}
		}

		Item* ground = tile->getGround();
		if (ground) {
			g_game.internalRemoveItem(ground);
			tile->setGround(nullptr);
		}
	}
}
bool Map::placeCreature(const Position& centerPos, Creature* creature, bool extendedPos, bool forceLogin)
{
	bool foundTile = false;
	bool placeInPZ = false;

	Tile* tile = getTile(centerPos.x, centerPos.y, centerPos.z);
	if (tile) {
		placeInPZ = tile->hasFlag(TILESTATE_PROTECTIONZONE);
		ReturnValue ret = tile->queryAdd(0, *creature, 1, FLAG_IGNOREBLOCKITEM);
		foundTile = forceLogin || ret == RETURNVALUE_NOERROR || ret == RETURNVALUE_PLAYERISNOTINVITED;
	}

	if (!foundTile) {
		static constexpr std::vector<std::pair<int32_t, int32_t>> extendedRelList{
			{0, -2}, {-1, -1}, {0, -1}, {1, -1}, {-2, 0}, {-1, 0}, {1, 0}, {2, 0},
			{-1, 1}, {0, 1}, {1, 1}, {0, 2}
		};
		static constexpr std::vector<std::pair<int32_t, int32_t>> normalRelList{
			{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}
		};

		std::vector<std::pair<int32_t, int32_t>> relList = extendedPos ? extendedRelList : normalRelList;
		std::random_device rd;
		std::mt19937 gen(rd());
		if (extendedPos) {
			std::shuffle(relList.begin(), relList.begin() + 4, gen);
			std::shuffle(relList.begin() + 4, relList.end(), gen);
		}
		else {
			std::shuffle(relList.begin(), relList.end(), gen);
		}

		for (const auto& [dx, dy] : relList) {
			Position tryPos(centerPos.x + dx, centerPos.y + dy, centerPos.z);
			tile = getTile(tryPos.x, tryPos.y, tryPos.z);
			if (!tile || (placeInPZ && !tile->hasFlag(TILESTATE_PROTECTIONZONE))) {
				continue;
			}

			if (tile->queryAdd(0, *creature, 1, 0) == RETURNVALUE_NOERROR) {
				if (!extendedPos || isSightClear(centerPos, tryPos, false)) {
					foundTile = true;
					break;
				}
			}
		}

		if (!foundTile) {
			return false;
		}
	}

	int32_t index = 0;
	uint32_t flags = 0;
	Item* toItem = nullptr;
	Cylinder* toCylinder = tile->queryDestination(index, *creature, &toItem, flags);
	toCylinder->internalAddThing(creature);

	const Position& dest = toCylinder->getPosition();
	getQTNode(dest.x, dest.y)->addCreature(creature);
	return true;
}

void Map::moveCreature(Creature& creature, Tile& newTile, bool forceTeleport)
{
	Tile& oldTile = *creature.getTile();
	Position oldPos = oldTile.getPosition();
	Position newPos = newTile.getPosition();

	bool teleport = forceTeleport || !newTile.getGround() || !Position::areInRange<1, 1, 0>(oldPos, newPos);

	SpectatorVec spectators, newPosSpectators;
	{
		std::lock_guard<std::mutex> lock(spectatorMutex);
		getSpectators(spectators, oldPos, true);
		getSpectators(newPosSpectators, newPos, true);
		spectators.addSpectators(newPosSpectators);
	}

	std::vector<int32_t> oldStackPosVector;
	oldStackPosVector.reserve(spectators.size());
	for (Creature* spectator : spectators) {
		if (Player* tmpPlayer = spectator->getPlayer()) {
			oldStackPosVector.push_back(tmpPlayer->canSeeCreature(&creature) ? oldTile.getClientIndexOfCreature(tmpPlayer, &creature) : -1);
		}
	}

	oldTile.removeThing(&creature, 0);
	QTreeLeafNode* leaf = getQTNode(oldPos.x, oldPos.y);
	QTreeLeafNode* newLeaf = getQTNode(newPos.x, newPos.y);
	if (leaf != newLeaf) {
		leaf->removeCreature(&creature);
		newLeaf->addCreature(&creature);
	}

	newTile.addThing(&creature);

	if (!teleport) {
		if (oldPos.y > newPos.y) {
			creature.setDirection(DIRECTION_NORTH);
		}
		else if (oldPos.y < newPos.y) {
			creature.setDirection(DIRECTION_SOUTH);
		}
		if (oldPos.x < newPos.x) {
			creature.setDirection(DIRECTION_EAST);
		}
		else if (oldPos.x > newPos.x) {
			creature.setDirection(DIRECTION_WEST);
		}
	}

	size_t i = 0;
	for (Creature* spectator : spectators) {
		if (Player* tmpPlayer = spectator->getPlayer()) {
			int32_t stackpos = oldStackPosVector[i++];
			if (stackpos != -1) {
				tmpPlayer->sendCreatureMove(&creature, newPos, newTile.getClientIndexOfCreature(tmpPlayer, &creature), oldPos, stackpos, teleport);
			}
		}
	}

	for (Creature* spectator : spectators) {
		spectator->onCreatureMove(&creature, &newTile, newPos, &oldTile, oldPos, teleport);
	}

	oldTile.postRemoveNotification(&creature, &newTile, 0);
	newTile.postAddNotification(&creature, &oldTile, 0);
}
void Map::getSpectatorsInternal(SpectatorVec& spectators, const Position& centerPos, int32_t minRangeX, int32_t maxRangeX,
	int32_t minRangeY, int32_t maxRangeY, int32_t minRangeZ, int32_t maxRangeZ, bool onlyPlayers) const
{
	int32_t min_y = centerPos.y + minRangeY;
	int32_t min_x = centerPos.x + minRangeX;
	int32_t max_y = centerPos.y + maxRangeY;
	int32_t max_x = centerPos.x + maxRangeX;

	int32_t minoffset = centerPos.z - maxRangeZ;
	uint16_t x1 = std::min<uint32_t>(0xFFFF, std::max<int32_t>(0, min_x + minoffset));
	uint16_t y1 = std::min<uint32_t>(0xFFFF, std::max<int32_t>(0, min_y + minoffset));

	int32_t maxoffset = centerPos.z - minRangeZ;
	uint16_t x2 = std::min<uint32_t>(0xFFFF, std::max<int32_t>(0, max_x + maxoffset));
	uint16_t y2 = std::min<uint32_t>(0xFFFF, std::max<int32_t>(0, max_y + maxoffset));

	int32_t startx1 = x1 - (x1 % FLOOR_SIZE);
	int32_t starty1 = y1 - (y1 % FLOOR_SIZE);
	int32_t endx2 = x2 - (x2 % FLOOR_SIZE);
	int32_t endy2 = y2 - (y2 % FLOOR_SIZE);

	const QTreeLeafNode* startLeaf = QTreeNode::getLeafStatic<const QTreeLeafNode*, const QTreeNode*>(&root, startx1, starty1);
	const QTreeLeafNode* leafS = startLeaf;
	const QTreeLeafNode* leafE;

	for (int32_t ny = starty1; ny <= endy2; ny += FLOOR_SIZE) {
		leafE = leafS;
		for (int32_t nx = startx1; nx <= endx2; nx += FLOOR_SIZE) {
			if (leafE) {
				const CreatureVector& node_list = onlyPlayers ? leafE->player_list : leafE->creature_list;
				for (Creature* creature : node_list) {
					const Position& cpos = creature->getPosition();
					if (cpos.z < minRangeZ || cpos.z > maxRangeZ) {
						continue;
					}
					int16_t offsetZ = Position::getOffsetZ(centerPos, cpos);
					if ((min_y + offsetZ) > cpos.y || (max_y + offsetZ) < cpos.y || (min_x + offsetZ) > cpos.x || (max_x + offsetZ) < cpos.x) {
						continue;
					}
					spectators.emplace_back(creature);
				}
				leafE = leafE->leafE;
			}
			else {
				leafE = QTreeNode::getLeafStatic<const QTreeLeafNode*, const QTreeNode*>(&root, nx + FLOOR_SIZE, ny);
			}
		}
		leafS = leafS ? leafS->leafS : QTreeNode::getLeafStatic<const QTreeLeafNode*, const QTreeNode*>(&root, startx1, ny + FLOOR_SIZE);
	}
}

void Map::getSpectators(SpectatorVec& spectators, const Position& centerPos, bool multifloor, bool onlyPlayers,
	int32_t minRangeX, int32_t maxRangeX, int32_t minRangeY, int32_t maxRangeY)
{
	if (centerPos.z >= MAP_MAX_LAYERS) {
		return;
	}

	minRangeX = (minRangeX == 0 ? -maxViewportX : -minRangeX);
	maxRangeX = (maxRangeX == 0 ? maxViewportX : maxRangeX);
	minRangeY = (minRangeY == 0 ? -maxViewportY : -minRangeY);
	maxRangeY = (maxRangeY == 0 ? maxViewportY : maxRangeY);

	std::lock_guard<std::mutex> lock(spectatorMutex);
	if (minRangeX == -maxViewportX && maxRangeX == maxViewportX && minRangeY == -maxViewportY && maxRangeY == maxViewportY && multifloor) {
		auto it = onlyPlayers ? playersSpectatorCache.find(centerPos) : spectatorCache.find(centerPos);
		if (it != (onlyPlayers ? playersSpectatorCache.end() : spectatorCache.end())) {
			spectators.addSpectators(it->second);
			return;
		}
	}

	int32_t minRangeZ, maxRangeZ;
	if (multifloor) {
		if (centerPos.z > 7) {
			minRangeZ = std::max<int32_t>(centerPos.z - 2, 0);
			maxRangeZ = std::min<int32_t>(centerPos.z + 2, MAP_MAX_LAYERS - 1);
		}
		else {
			minRangeZ = 0;
			maxRangeZ = (centerPos.z == 7) ? 9 : (centerPos.z == 6 ? 8 : 7);
		}
	}
	else {
		minRangeZ = maxRangeZ = centerPos.z;
	}

	getSpectatorsInternal(spectators, centerPos, minRangeX, maxRangeX, minRangeY, maxRangeY, minRangeZ, maxRangeZ, onlyPlayers);

	if (minRangeX == -maxViewportX && maxRangeX == maxViewportX && minRangeY == -maxViewportY && maxRangeY == maxViewportY && multifloor) {
		(onlyPlayers ? playersSpectatorCache : spectatorCache)[centerPos] = spectators;
	}
}

void Map::clearSpectatorCache()
{
	std::lock_guard<std::mutex> lock(spectatorMutex);
	spectatorCache.clear();
}

void Map::clearPlayersSpectatorCache()
{
	std::lock_guard<std::mutex> lock(spectatorMutex);
	playersSpectatorCache.clear();
}
bool Map::canThrowObjectTo(const Position& fromPos, const Position& toPos, bool checkLineOfSight, bool sameFloor,
	int32_t rangex, int32_t rangey) const
{
	if (Position::getDistanceX(fromPos, toPos) > rangex || Position::getDistanceY(fromPos, toPos) > rangey) {
		return false;
	}
	return !checkLineOfSight || isSightClear(fromPos, toPos, sameFloor);
}

bool Map::isTileClear(uint16_t x, uint16_t y, uint8_t z, bool blockFloor) const
{
	const Tile* tile = getTile(x, y, z);
	return !tile || (blockFloor && tile->getGround()) || !tile->hasProperty(CONST_PROP_BLOCKPROJECTILE);
}

bool Map::checkSightLine(uint16_t x0, uint16_t y0, uint16_t x1, uint16_t y1, uint8_t z) const
{
	if (x0 == x1 && y0 == y1) {
		return true;
	}

	auto steepLine = [this, z](uint16_t x0, uint16_t y0, uint16_t x1, uint16_t y1) {
		float dx = x1 - x0;
		float slope = (dx == 0) ? 1 : (y1 - y0) / dx;
		float yi = y0 + slope;
		for (uint16_t x = x0 + 1; x < x1; ++x) {
			if (!isTileClear(x, static_cast<uint16_t>(yi + 0.1f), z)) {
				return false;
			}
			yi += slope;
		}
		return true;
		};

	auto slightLine = [this, z](uint16_t x0, uint16_t y0, uint16_t x1, uint16_t y1) {
		float dx = x1 - x0;
		float slope = (dx == 0) ? 1 : (y1 - y0) / dx;
		float yi = y0 + slope;
		for (uint16_t x = x0 + 1; x < x1; ++x) {
			if (!isTileClear(static_cast<uint16_t>(yi + 0.1f), x, z)) {
				return false;
			}
			yi += slope;
		}
		return true;
		};

	if (std::abs(y1 - y0) > std::abs(x1 - x0)) {
		return (y1 > y0) ? steepLine(y0, x0, y1, x1) : steepLine(y1, x1, y0, x0);
	}
	return (x0 > x1) ? slightLine(x1, y1, x0, y0) : slightLine(x0, y0, x1, y1);
}

bool Map::isSightClear(const Position& fromPos, const Position& toPos, bool sameFloor) const
{
	if (fromPos.z == toPos.z) {
		if (Position::getDistanceX(fromPos, toPos) < 2 && Position::getDistanceY(fromPos, toPos) < 2) {
			return true;
		}
		bool sightClear = checkSightLine(fromPos.x, fromPos.y, toPos.x, toPos.y, fromPos.z);
		if (sightClear || sameFloor) {
			return sightClear;
		}
		if (fromPos.z == 0) {
			return true;
		}
		uint8_t newZ = fromPos.z - 1;
		return isTileClear(fromPos.x, fromPos.y, newZ, true) && isTileClear(toPos.x, toPos.y, newZ, true) &&
			checkSightLine(fromPos.x, fromPos.y, toPos.x, toPos.y, newZ);
	}

	if (sameFloor || (fromPos.z < 8 && toPos.z > 7) || (fromPos.z > 7 && toPos.z < 8)) {
		return false;
	}

	if (fromPos.z > toPos.z) {
		if (Position::getDistanceZ(fromPos, toPos) > 1) {
			return false;
		}
		uint8_t newZ = fromPos.z - 1;
		return isTileClear(fromPos.x, fromPos.y, newZ, true) && checkSightLine(fromPos.x, fromPos.y, toPos.x, toPos.y, newZ);
	}

	for (uint8_t z = fromPos.z; z < toPos.z; ++z) {
		if (!isTileClear(toPos.x, toPos.y, z, true)) {
			return false;
		}
	}
	return checkSightLine(fromPos.x, fromPos.y, toPos.x, toPos.y, fromPos.z);
}

const Tile* Map::canWalkTo(const Creature& creature, const Position& pos) const
{
	int32_t walkCache = creature.getWalkCache(pos);
	if (walkCache == 0) {
		return nullptr;
	}
	if (walkCache == 1) {
		return getTile(pos.x, pos.y, pos.z);
	}

	Tile* tile = getTile(pos.x, pos.y, pos.z);
	if (creature.getTile() != tile && (!tile || tile->queryAdd(0, creature, 1, FLAG_PATHFINDING | FLAG_IGNOREFIELDDAMAGE) != RETURNVALUE_NOERROR)) {
		return nullptr;
	}
	return tile;
}
bool Map::getPathMatching(const Creature& creature, std::vector<Direction>& dirList, const FrozenPathingConditionCall& pathCondition,
	const FindPathParams& fpp) const
{
	Position pos = creature.getPosition();
	Position endPos;
	AStarNodes nodes(pos.x, pos.y);

	int32_t bestMatch = 0;
	static constexpr int_fast32_t dirNeighbors[8][5][2] = {
		{{-1, 0}, {0, 1}, {1, 0}, {1, 1}, {-1, 1}}, {{-1, 0}, {0, 1}, {0, -1}, {-1, -1}, {-1, 1}},
		{{-1, 0}, {1, 0}, {0, -1}, {-1, -1}, {1, -1}}, {{0, 1}, {1, 0}, {0, -1}, {1, -1}, {1, 1}},
		{{1, 0}, {0, -1}, {-1, -1}, {1, -1}, {1, 1}}, {{-1, 0}, {0, -1}, {-1, -1}, {1, -1}, {-1, 1}},
		{{0, 1}, {1, 0}, {1, -1}, {1, 1}, {-1, 1}}, {{-1, 0}, {0, 1}, {-1, -1}, {1, 1}, {-1, 1}}
	};
	static constexpr int_fast32_t allNeighbors[8][2] = {
		{-1, 0}, {0, 1}, {1, 0}, {0, -1}, {-1, -1}, {1, -1}, {1, 1}, {-1, 1}
	};
	const Position startPos = pos;

	AStarNode* found = nullptr;
	while (fpp.maxSearchDist != 0 || nodes.getClosedNodes() < 100) {
		AStarNode* n = nodes.getBestNode();
		if (!n) {
			if (found) break;
			return false;
		}

		pos.x = n->x;
		pos.y = n->y;
		if (pathCondition(startPos, pos, fpp, bestMatch)) {
			found = n;
			endPos = pos;
			if (bestMatch == 0) break;
		}

		uint_fast32_t dirCount = n->parent ? (fpp.allowDiagonal ? 5 : 3) : 8;
		const int_fast32_t* neighbors = n->parent ? dirNeighbors[Position::getOffsetX({ n->parent->x, n->parent->y, 0 }, pos) + 1 +
			(Position::getOffsetY({ n->parent->x, n->parent->y, 0 }, pos) + 1) * 3][0] : allNeighbors[0];

		const int_fast32_t f = n->f;
		for (uint_fast32_t i = 0; i < dirCount; ++i, neighbors += 2) {
			pos.x = n->x + neighbors[0];
			pos.y = n->y + neighbors[1];

			if (fpp.maxSearchDist != 0 && (Position::getDistanceX(startPos, pos) > fpp.maxSearchDist || Position::getDistanceY(startPos, pos) > fpp.maxSearchDist)) {
				continue;
			}
			if (fpp.keepDistance && !pathCondition.isInRange(startPos, pos, fpp)) {
				continue;
			}

			const Tile* tile;
			AStarNode* neighborNode = nodes.getNodeByPosition(pos.x, pos.y);
			if (neighborNode) {
				tile = getTile(pos.x, pos.y, pos.z);
			}
			else if (!(tile = canWalkTo(creature, pos))) {
				continue;
			}

			const int_fast32_t cost = AStarNodes::getMapWalkCost(n, pos);
			const int_fast32_t extraCost = AStarNodes::getTileWalkCost(creature, tile);
			const int_fast32_t newf = f + cost + extraCost;

			if (neighborNode) {
				if (neighborNode->f <= newf) continue;
				neighborNode->f = newf;
				neighborNode->parent = n;
				nodes.openNode(neighborNode);
			}
			else if (AStarNode* newNode = nodes.createOpenNode(n, pos.x, pos.y, newf)) {
				neighborNode = newNode;
			}
			else if (found) {
				break;
			}
			else {
				return false;
			}
		}
		nodes.closeNode(n);
	}

	if (!found) return false;

	int_fast32_t prevx = endPos.x;
	int_fast32_t prevy = endPos.y;
	found = found->parent;
	while (found) {
		pos.x = found->x;
		pos.y = found->y;
		int_fast32_t dx = pos.x - prevx;
		int_fast32_t dy = pos.y - prevy;
		prevx = pos.x;
		prevy = pos.y;

		if (dx == 1) dirList.push_back(dy == 1 ? DIRECTION_NORTHWEST : dy == -1 ? DIRECTION_SOUTHWEST : DIRECTION_WEST);
		else if (dx == -1) dirList.push_back(dy == 1 ? DIRECTION_NORTHEAST : dy == -1 ? DIRECTION_SOUTHEAST : DIRECTION_EAST);
		else if (dy == 1) dirList.push_back(DIRECTION_NORTH);
		else if (dy == -1) dirList.push_back(DIRECTION_SOUTH);

		found = found->parent;
	}
	return true;
}

// Updated supporting classes
class Floor {
public:
	~Floor() = default; // unique_ptr handles tile cleanup
	std::array<std::array<std::unique_ptr<Tile>, FLOOR_SIZE>, FLOOR_SIZE> tiles;
};

class QTreeNode {
public:
	virtual ~QTreeNode() = default;
	std::array<std::unique_ptr<QTreeNode>, 4> child{};
	virtual bool isLeaf() const { return false; }
	QTreeLeafNode* getLeaf(uint32_t x, uint32_t y);
	QTreeLeafNode* createLeaf(uint32_t x, uint32_t y, uint32_t level);
};

class QTreeLeafNode : public QTreeNode {
public:
	static bool newLeaf;
	~QTreeLeafNode() = default;
	bool isLeaf() const override { return true; }
	std::array<std::unique_ptr<Floor>, MAP_MAX_LAYERS> array{};
	CreatureVector creature_list;
	CreatureVector player_list;
	QTreeLeafNode* leafS = nullptr, * leafE = nullptr;
	Floor* createFloor(uint32_t z) { if (!array[z]) array[z] = std::make_unique<Floor>(); return array[z].get(); }
	void addCreature(Creature* c) { creature_list.push_back(c); if (c->getPlayer()) player_list.push_back(c); }
	void removeCreature(Creature* c) {
		creature_list.erase(std::remove(creature_list.begin(), creature_list.end(), c), creature_list.end());
		if (c->getPlayer()) player_list.erase(std::remove(player_list.begin(), player_list.end(), c), player_list.end());
	}
};
bool QTreeLeafNode::newLeaf = false;
uint32_t Map::clean() const
{
	uint64_t start = OTSYS_TIME();
	size_t tiles = 0;

	if (g_game.getGameState() == GAME_STATE_NORMAL) {
		g_game.setGameState(GAME_STATE_MAINTAIN);
	}

	std::vector<Item*> toRemove;
	toRemove.reserve(256); // Pre-allocate for efficiency

	for (const Tile* tile : g_game.getTilesToClean()) {
		if (!tile) continue;
		if (TileItemVector* items = tile->getItemList()) {
			++tiles;
			for (Item* item : *items) {
				if (item->isCleanable()) {
					toRemove.push_back(item);
				}
			}
		}
	}

	for (Item* item : toRemove) {
		g_game.internalRemoveItem(item, -1);
	}

	size_t count = toRemove.size();
	g_game.clearTilesToClean();

	if (g_game.getGameState() == GAME_STATE_MAINTAIN) {
		g_game.setGameState(GAME_STATE_NORMAL);
	}

	std::cout << fmt::format("> CLEAN: Removed {:d} item{} from {:d} tile{} in {:.3f} seconds.\n",
		count, count != 1 ? "s" : "", tiles, tiles != 1 ? "s" : "", (OTSYS_TIME() - start) / 1000.0);
	return count;
}

// AStarNodes unchanged for brevity, but here's the updated version with constexpr
class AStarNodes {
private:
	static constexpr size_t MAX_NODES = 512; // Assumed value
	std::array<AStarNode, MAX_NODES> nodes{};
	std::array<bool, MAX_NODES> openNodes{};
	std::unordered_map<uint32_t, AStarNode*> nodeTable;
	size_t curNode = 0;
	int_fast32_t closedNodes = 0;

public:
	AStarNodes(uint32_t x, uint32_t y) {
		curNode = 1;
		openNodes[0] = true;
		nodes[0] = { nullptr, x, y, 0 };
		nodeTable[(x << 16) | y] = &nodes[0];
	}

	AStarNode* createOpenNode(AStarNode* parent, uint32_t x, uint32_t y, int_fast32_t f) {
		if (curNode >= MAX_NODES) return nullptr;
		size_t retNode = curNode++;
		openNodes[retNode] = true;
		nodes[retNode] = { parent, x, y, f };
		nodeTable[(x << 16) | y] = &nodes[retNode];
		return &nodes[retNode];
	}

	AStarNode* getBestNode() {
		int32_t best_node_f = std::numeric_limits<int32_t>::max();
		int32_t best_node = -1;
		for (size_t i = 0; i < curNode; ++i) {
			if (openNodes[i] && nodes[i].f < best_node_f) {
				best_node_f = nodes[i].f;
				best_node = i;
			}
		}
		return (best_node >= 0) ? &nodes[best_node] : nullptr;
	}

	void closeNode(AStarNode* node) { openNodes[node - nodes.data()] = false; ++closedNodes; }
	void openNode(AStarNode* node) { size_t idx = node - nodes.data(); if (!openNodes[idx]) { openNodes[idx] = true; --closedNodes; } }
	int_fast32_t getClosedNodes() const { return closedNodes; }
	AStarNode* getNodeByPosition(uint32_t x, uint32_t y) {
		auto it = nodeTable.find((x << 16) | y);
		return (it != nodeTable.end()) ? it->second : nullptr;
	}
	static constexpr int_fast32_t getMapWalkCost(AStarNode* node, const Position& neighborPos) {
		return (std::abs(node->x - neighborPos.x) == std::abs(node->y - neighborPos.y)) ? MAP_DIAGONALWALKCOST : MAP_NORMALWALKCOST;
	}
	static int_fast32_t getTileWalkCost(const Creature& creature, const Tile* tile);
};
