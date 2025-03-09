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
#include "protocolgame.h"
#include "outputmessage.h"
#include "player.h"
#include "configmanager.h"
#include "actions.h"
#include "game.h"
#include "iologindata.h"
#include "ban.h"
#include "scheduler.h"
#include <fmt/format.h>
#include <mutex>
#include <optional>

extern ConfigManager g_config;
extern Actions actions;
extern CreatureEvents* g_creatureEvents;
extern Chat* g_chat;

namespace {
	using WaitList = std::deque<std::pair<int64_t, uint32_t>>;
	inline std::mutex waitListMutex;
	inline WaitList priorityWaitList, waitList;
	inline std::mutex playerMutex; // Added for thread-safe player access
}
namespace {

	std::tuple<WaitList&, WaitList::iterator, WaitList::size_type> findClient(const Player& player) {
		const auto fn = [&](const WaitList::value_type& it) { return it.second == player.getGUID(); };
		std::lock_guard<std::mutex> lock(waitListMutex);

		auto it = std::find_if(priorityWaitList.begin(), priorityWaitList.end(), fn);
		if (it != priorityWaitList.end()) {
			return std::make_tuple(std::ref(priorityWaitList), it, std::distance(it, priorityWaitList.end()) + 1);
		}

		it = std::find_if(waitList.begin(), waitList.end(), fn);
		if (it != waitList.end()) {
			return std::make_tuple(std::ref(waitList), it, priorityWaitList.size() + std::distance(it, waitList.end()) + 1);
		}

		return std::make_tuple(std::ref(waitList), waitList.end(), priorityWaitList.size() + waitList.size());
	}

	uint8_t getWaitTime(std::size_t slot) noexcept {
		if (slot < 5) return 5;
		if (slot < 10) return 10;
		if (slot < 20) return 20;
		if (slot < 50) return 60;
		return 120;
	}

	int64_t getTimeout(std::size_t slot) noexcept {
		return getWaitTime(slot) + 15;
	}

	void cleanupList(WaitList& list) {
		std::lock_guard<std::mutex> lock(waitListMutex);
		int64_t time = OTSYS_TIME();
		list.erase(std::remove_if(list.begin(), list.end(), [time](const auto& it) { return it.first <= time; }), list.end());
	}

	std::size_t clientLogin(const Player& player) {
		if (player.hasFlag(PlayerFlag_CanAlwaysLogin) || player.getAccountType() >= ACCOUNT_TYPE_GAMEMASTER) {
			return 0;
		}

		cleanupList(priorityWaitList);
		cleanupList(waitList);

		uint32_t maxPlayers = g_config.getNumber(ConfigManager::MAX_PLAYERS);
		std::size_t onlinePlayers = g_game.getPlayersOnline();
		if (maxPlayers == 0 || (priorityWaitList.empty() && waitList.empty() && onlinePlayers < maxPlayers)) {
			return 0;
		}

		auto [list, it, currentSlot] = findClient(player);
		if (it != list.get().end()) {
			if (onlinePlayers + currentSlot <= maxPlayers) {
				std::lock_guard<std::mutex> lock(waitListMutex);
				list.get().erase(it);
				return 0;
			}
			it->first = OTSYS_TIME() + (getTimeout(currentSlot) * 1000);
			return currentSlot;
		}

		std::lock_guard<std::mutex> lock(waitListMutex);
		currentSlot = priorityWaitList.size();
		if (player.isPremium()) {
			priorityWaitList.emplace_back(OTSYS_TIME() + (getTimeout(++currentSlot) * 1000), player.getGUID());
		}
		else {
			currentSlot += waitList.size();
			waitList.emplace_back(OTSYS_TIME() + (getTimeout(++currentSlot) * 1000), player.getGUID());
		}
		return currentSlot;
	}

}
void ProtocolGame::release() {
	std::lock_guard<std::mutex> lock(playerMutex);
	if (player && player->client.lock() == shared_from_this()) {
		player->client.reset();
		player->decrementReferenceCounter();
		player = nullptr;
	}
	OutputMessagePool::getInstance().removeProtocolFromAutosend(shared_from_this());
	Protocol::release();
}

void ProtocolGame::login(const std::string& name, uint32_t accountId, OperatingSystem_t operatingSystem) {
	auto foundPlayer = g_game.getPlayerByName(name);
	if (!foundPlayer || g_config.getBoolean(ConfigManager::ALLOW_CLONES)) {
		player = std::make_shared<Player>(getThis());
		player->setName(name);
		player->incrementReferenceCounter();
		player->setID();

		if (!IOLoginData::preloadPlayer(player, name)) {
			disconnectClient("Your character could not be loaded.");
			return;
		}

		if (IOBan::isPlayerNamelocked(player->getGUID())) {
			disconnectClient("Your character has been namelocked.");
			return;
		}

		if (g_game.getGameState() == GAME_STATE_CLOSING && !player->hasFlag(PlayerFlag_CanAlwaysLogin)) {
			disconnectClient("The game is just going down.\nPlease try again later.");
			return;
		}

		if (g_game.getGameState() == GAME_STATE_CLOSED && !player->hasFlag(PlayerFlag_CanAlwaysLogin)) {
			disconnectClient("Server is currently closed.\nPlease try again later.");
			return;
		}

		if (g_config.getBoolean(ConfigManager::ONE_PLAYER_ON_ACCOUNT) && player->getAccountType() < ACCOUNT_TYPE_GAMEMASTER && g_game.getPlayerByAccount(player->getAccount())) {
			disconnectClient("You may only login with one character\nof your account at the same time.");
			return;
		}

		if (!player->hasFlag(PlayerFlag_CannotBeBanned)) {
			BanInfo banInfo;
			if (IOBan::isAccountBanned(accountId, banInfo)) {
				std::string reason = banInfo.reason.empty() ? "(none)" : banInfo.reason;
				std::string message = banInfo.expiresAt > 0 ?
					fmt::format("Your account has been banned until {:s} by {:s}.\n\nReason: {:s}", formatDateShort(banInfo.expiresAt), banInfo.bannedBy, reason) :
					fmt::format("Your account has been permanently banned by {:s}.\n\nReason: {:s}", banInfo.bannedBy, reason);
				disconnectClient(message);
				return;
			}
		}

		if (std::size_t currentSlot = clientLogin(*player)) {
			auto output = OutputMessagePool::getOutputMessage();
			output->addByte(0x16);
			output->addString(fmt::format("Too many players online.\nYou are at place {:d} on the waiting list.", currentSlot));
			output->addByte(getWaitTime(currentSlot));
			send(output);
			disconnect();
			return;
		}

		if (!IOLoginData::loadPlayerById(player, player->getGUID())) {
			disconnectClient("Your character could not be loaded.");
			return;
		}

		player->setOperatingSystem(operatingSystem);
		if (!g_game.placeCreature(player, player->getLoginPosition()) && !g_game.placeCreature(player, player->getTemplePosition(), false, true)) {
			disconnectClient("Temple position is wrong. Contact the administrator.");
			return;
		}

		if (operatingSystem >= CLIENTOS_OTCLIENT_LINUX) {
			player->registerCreatureEvent("ExtendedOpcode");
		}

		player->lastIP = player->getIP();
		player->lastLoginSaved = std::max<time_t>(time(nullptr), player->lastLoginSaved + 1);
		acceptPackets = true;
	}
	else {
		if (eventConnect != 0 || !g_config.getBoolean(ConfigManager::REPLACE_KICK_ON_LOGIN)) {
			disconnectClient("You are already logged in.");
			return;
		}

		if (foundPlayer->client.lock()) {
			foundPlayer->disconnect();
			foundPlayer->isConnecting = true;
			eventConnect = g_scheduler.addEvent(createSchedulerTask(1000, std::bind(&ProtocolGame::connect, getThis(), foundPlayer->getID(), operatingSystem)));
		}
		else {
			connect(foundPlayer->getID(), operatingSystem);
		}
	}
	OutputMessagePool::getInstance().addProtocolToAutosend(shared_from_this());
}

void ProtocolGame::connect(uint32_t playerId, OperatingSystem_t operatingSystem) {
	eventConnect = 0;
	auto foundPlayer = g_game.getPlayerByID(playerId);
	if (!foundPlayer || foundPlayer->client.lock()) {
		disconnectClient("You are already logged in.");
		return;
	}

	if (isConnectionExpired()) {
		return;
	}

	player = foundPlayer;
	player->incrementReferenceCounter();
	g_chat->removeUserFromAllChannels(*player);
	player->setOperatingSystem(operatingSystem);
	player->isConnecting = false;
	player->client = getThis();
	sendAddCreature(player, player->getPosition(), 0, false);
	player->lastIP = player->getIP();
	player->lastLoginSaved = std::max<time_t>(time(nullptr), player->lastLoginSaved + 1);
	player->resetIdleTime();
	acceptPackets = true;
}

void ProtocolGame::logout(bool displayEffect, bool forced) {
	std::lock_guard<std::mutex> lock(playerMutex);
	if (!player) return;

	if (!player->isRemoved() && !forced) {
		if (!player->isAccessPlayer()) {
			if (player->getTile()->hasFlag(TILESTATE_NOLOGOUT)) {
				player->sendCancelMessage(RETURNVALUE_YOUCANNOTLOGOUTHERE);
				return;
			}
			if (!player->getTile()->hasFlag(TILESTATE_PROTECTIONZONE) && player->hasCondition(CONDITION_INFIGHT)) {
				player->sendCancelMessage(RETURNVALUE_YOUMAYNOTLOGOUTDURINGAFIGHT);
				return;
			}
		}
		if (!g_creatureEvents->playerLogout(player)) {
			return;
		}
	}

	if (displayEffect && player->getHealth() > 0 && !player->isInGhostMode()) {
		g_game.addMagicEffect(player->getPosition(), CONST_ME_POFF);
	}

	disconnect();
	g_game.removeCreature(player);
}
void ProtocolGame::onRecvFirstMessage(NetworkMessage& msg) {
	if (g_game.getGameState() == GAME_STATE_SHUTDOWN) {
		disconnect();
		return;
	}

	OperatingSystem_t operatingSystem = static_cast<OperatingSystem_t>(msg.get<uint16_t>());
	version = msg.get<uint16_t>();

	if (!Protocol::RSA_decrypt(msg)) {
		disconnect();
		return;
	}

	xtea::key key{ msg.get<uint32_t>(), msg.get<uint32_t>(), msg.get<uint32_t>(), msg.get<uint32_t>() };
	enableXTEAEncryption();
	setXTEAKey(std::move(key));

	if (operatingSystem >= CLIENTOS_OTCLIENT_LINUX) {
		NetworkMessage opcodeMessage;
		opcodeMessage.addByte(0x32);
		opcodeMessage.addByte(0x00);
		opcodeMessage.add<uint16_t>(0x00);
		writeToOutputBuffer(opcodeMessage);
	}

	msg.skipBytes(1); // gamemaster flag
	std::string accountName = msg.getString();
	std::string characterName = msg.getString();
	std::string password = msg.getString();

	if (accountName.empty()) {
		disconnectClient("You must enter your account name.");
		return;
	}

	uint32_t timeStamp = msg.get<uint32_t>();
	uint8_t randNumber = msg.getByte();
	if (challengeTimestamp != timeStamp || challengeRandom != randNumber) {
		disconnect();
		return;
	}

	if (version < CLIENT_VERSION_MIN || version > CLIENT_VERSION_MAX) {
		disconnectClient(fmt::format("Only clients with protocol {:s} allowed!", CLIENT_VERSION_STR));
		return;
	}

	if (g_game.getGameState() == GAME_STATE_STARTUP) {
		disconnectClient("Gameworld is starting up. Please wait.");
		return;
	}

	if (g_game.getGameState() == GAME_STATE_MAINTAIN) {
		disconnectClient("Gameworld is under maintenance. Please re-connect in a while.");
		return;
	}

	BanInfo banInfo;
	if (IOBan::isIpBanned(getIP(), banInfo)) {
		std::string reason = banInfo.reason.empty() ? "(none)" : banInfo.reason;
		disconnectClient(fmt::format("Your IP has been banned until {:s} by {:s}.\n\nReason: {:s}", formatDateShort(banInfo.expiresAt), banInfo.bannedBy, reason));
		return;
	}

	uint32_t accountId = IOLoginData::gameworldAuthentication(accountName, password, characterName);
	if (accountId == 0) {
		disconnectClient("Account name or password is not correct.");
		return;
	}

	g_dispatcher.addTask(createTask(std::bind(&ProtocolGame::login, getThis(), characterName, accountId, operatingSystem)));
}

void ProtocolGame::onConnect() {
	auto output = OutputMessagePool::getOutputMessage();
	static std::random_device rd;
	static std::ranlux24 generator(rd());
	static std::uniform_int_distribution<uint16_t> randNumber(0x00, 0xFF);

	output->skipBytes(sizeof(uint32_t));
	output->add<uint16_t>(0x0006);
	output->addByte(0x1F);
	challengeTimestamp = static_cast<uint32_t>(time(nullptr));
	output->add<uint32_t>(challengeTimestamp);
	challengeRandom = randNumber(generator);
	output->addByte(challengeRandom);
	output->skipBytes(-12);
	output->add<uint32_t>(adlerChecksum(output->getOutputBuffer() + sizeof(uint32_t), 8));
	send(output);
}

void ProtocolGame::disconnectClient(const std::string& message) const {
	auto output = OutputMessagePool::getOutputMessage();
	output->addByte(0x14);
	output->addString(message);
	send(output);
	disconnect();
}

void ProtocolGame::writeToOutputBuffer(const NetworkMessage& msg) {
	auto out = getOutputBuffer(msg.getLength());
	out->append(msg);
}
void ProtocolGame::parsePacket(NetworkMessage& msg) {
	if (!acceptPackets || g_game.getGameState() == GAME_STATE_SHUTDOWN || msg.getLength() == 0) {
		return;
	}

	uint8_t recvbyte = msg.getByte();
	if (!player) {
		if (recvbyte == 0x0F) disconnect();
		return;
	}

	if (player->isRemoved() || player->getHealth() <= 0) {
		if (recvbyte == 0x0F) disconnect();
		else if (recvbyte != 0x14) return;
	}

	switch (recvbyte) {
	case 0x14: g_dispatcher.addTask(createTask(std::bind(&ProtocolGame::logout, getThis(), true, false))); break;
	case 0x1E: addGameTask(&Game::playerReceivePing, player->getID()); break;
	case 0x32: parseExtendedOpcode(msg); break;
	case 0x64: parseAutoWalk(msg); break;
	case 0x65: addGameTask(&Game::playerMove, player->getID(), DIRECTION_NORTH); break;
	case 0x66: addGameTask(&Game::playerMove, player->getID(), DIRECTION_EAST); break;
	case 0x67: addGameTask(&Game::playerMove, player->getID(), DIRECTION_SOUTH); break;
	case 0x68: addGameTask(&Game::playerMove, player->getID(), DIRECTION_WEST); break;
	case 0x69: addGameTask(&Game::playerStopAutoWalk, player->getID()); break;
	case 0x6A: addGameTask(&Game::playerMove, player->getID(), DIRECTION_NORTHEAST); break;
	case 0x6B: addGameTask(&Game::playerMove, player->getID(), DIRECTION_SOUTHEAST); break;
	case 0x6C: addGameTask(&Game::playerMove, player->getID(), DIRECTION_SOUTHWEST); break;
	case 0x6D: addGameTask(&Game::playerMove, player->getID(), DIRECTION_NORTHWEST); break;
	case 0x6F: addGameTaskTimed(DISPATCHER_TASK_EXPIRATION, &Game::playerTurn, player->getID(), DIRECTION_NORTH); break;
	case 0x70: addGameTaskTimed(DISPATCHER_TASK_EXPIRATION, &Game::playerTurn, player->getID(), DIRECTION_EAST); break;
	case 0x71: addGameTaskTimed(DISPATCHER_TASK_EXPIRATION, &Game::playerTurn, player->getID(), DIRECTION_SOUTH); break;
	case 0x72: addGameTaskTimed(DISPATCHER_TASK_EXPIRATION, &Game::playerTurn, player->getID(), DIRECTION_WEST); break;
	case 0x78: parseThrow(msg); break;
	case 0x79: parseLookInShop(msg); break;
	case 0x7A: parsePlayerPurchase(msg); break;
	case 0x7B: parsePlayerSale(msg); break;
	case 0x7C: addGameTask(&Game::playerCloseShop, player->getID()); break;
	case 0x7D: parseRequestTrade(msg); break;
	case 0x7E: parseLookInTrade(msg); break;
	case 0x7F: addGameTask(&Game::playerAcceptTrade, player->getID()); break;
	case 0x80: addGameTask(&Game::playerCloseTrade, player->getID()); break;
	case 0x82: parseUseItem(msg); break;
	case 0x83: parseUseItemEx(msg); break;
	case 0x84: parseUseWithCreature(msg); break;
	case 0x85: parseRotateItem(msg); break;
	case 0x87: parseCloseContainer(msg); break;
	case 0x88: parseUpArrowContainer(msg); break;
	case 0x89: parseTextWindow(msg); break;
	case 0x8A: parseHouseWindow(msg); break;
	case 0x8C: parseLookAt(msg); break;
	case 0x8D: parseLookInBattleList(msg); break;
	case 0x8E: break; // join aggression
	case 0x96: parseSay(msg); break;
	case 0x97: addGameTask(&Game::playerRequestChannels, player->getID()); break;
	case 0x98: parseOpenChannel(msg); break;
	case 0x99: parseCloseChannel(msg); break;
	case 0x9A: parseOpenPrivateChannel(msg); break;
	case 0x9E: addGameTask(&Game::playerCloseNpcChannel, player->getID()); break;
	case 0xA0: parseFightModes(msg); break;
	case 0xA1: parseAttack(msg); break;
	case 0xA2: parseFollow(msg); break;
	case 0xA3: parseInviteToParty(msg); break;
	case 0xA4: parseJoinParty(msg); break;
	case 0xA5: parseRevokePartyInvite(msg); break;
	case 0xA6: parsePassPartyLeadership(msg); break;
	case 0xA7: addGameTask(&Game::playerLeaveParty, player->getID()); break;
	case 0xA8: parseEnableSharedPartyExperience(msg); break;
	case 0xAA: addGameTask(&Game::playerCreatePrivateChannel, player->getID()); break;
	case 0xAB: parseChannelInvite(msg); break;
	case 0xAC: parseChannelExclude(msg); break;
	case 0xBE: addGameTask(&Game::playerCancelAttackAndFollow, player->getID()); break;
	case 0xC9: break; // update tile
	case 0xCA: parseUpdateContainer(msg); break;
	case 0xD2: addGameTask(&Game::playerRequestOutfit, player->getID()); break;
	case 0xD3: parseSetOutfit(msg); break;
	case 0xDC: parseAddVip(msg); break;
	case 0xDD: parseRemoveVip(msg); break;
	case 0xE6: parseBugReport(msg); break;
	case 0xE7: break; // thank you
	case 0xE8: parseDebugAssert(msg); break;
	case 0xF0: addGameTaskTimed(DISPATCHER_TASK_EXPIRATION, &Game::playerShowQuestLog, player->getID()); break;
	case 0xF1: parseQuestLine(msg); break;
	case 0xF2: parseRuleViolationReport(msg); break;
	case 0xF3: break; // get object info
	default: break;
	}

	if (msg.isOverrun()) {
		disconnect();
	}
}

void ProtocolGame::parseChannelInvite(NetworkMessage& msg) {
	addGameTask(&Game::playerChannelInvite, player->getID(), msg.getString());
}

void ProtocolGame::parseChannelExclude(NetworkMessage& msg) {
	addGameTask(&Game::playerChannelExclude, player->getID(), msg.getString());
}

void ProtocolGame::parseOpenChannel(NetworkMessage& msg) {
	addGameTask(&Game::playerOpenChannel, player->getID(), msg.get<uint16_t>());
}

void ProtocolGame::parseCloseChannel(NetworkMessage& msg) {
	addGameTask(&Game::playerCloseChannel, player->getID(), msg.get<uint16_t>());
}

void ProtocolGame::parseOpenPrivateChannel(NetworkMessage& msg) {
	addGameTask(&Game::playerOpenPrivateChannel, player->getID(), msg.getString());
}
void ProtocolGame::parseAutoWalk(NetworkMessage& msg) {
	uint8_t numdirs = msg.getByte();
	if (numdirs == 0 || (msg.getBufferPosition() + numdirs) != (msg.getLength() + 8)) {
		return;
	}

	std::vector<Direction> path;
	path.reserve(numdirs);
	for (uint8_t i = 0; i < numdirs; ++i) {
		switch (msg.getByte()) {
		case 1: path.push_back(DIRECTION_EAST); break;
		case 2: path.push_back(DIRECTION_NORTHEAST); break;
		case 3: path.push_back(DIRECTION_NORTH); break;
		case 4: path.push_back(DIRECTION_NORTHWEST); break;
		case 5: path.push_back(DIRECTION_WEST); break;
		case 6: path.push_back(DIRECTION_SOUTHWEST); break;
		case 7: path.push_back(DIRECTION_SOUTH); break;
		case 8: path.push_back(DIRECTION_SOUTHEAST); break;
		default: break;
		}
	}
	if (!path.empty()) {
		addGameTask(&Game::playerAutoWalk, player->getID(), std::move(path));
	}
}

void ProtocolGame::parseSetOutfit(NetworkMessage& msg) {
	Outfit_t newOutfit;
	newOutfit.lookType = msg.get<uint16_t>();
	newOutfit.lookHead = msg.getByte();
	newOutfit.lookBody = msg.getByte();
	newOutfit.lookLegs = msg.getByte();
	newOutfit.lookFeet = msg.getByte();
	newOutfit.lookAddons = msg.getByte();
	addGameTask(&Game::playerChangeOutfit, player->getID(), newOutfit);
}

void ProtocolGame::parseUseItem(NetworkMessage& msg) {
	Position pos = msg.getPosition();
	uint16_t spriteId = msg.get<uint16_t>();
	uint8_t stackpos = msg.getByte();
	uint8_t index = msg.getByte();
	addGameTaskTimed(DISPATCHER_TASK_EXPIRATION, &Game::playerUseItem, player->getID(), pos, stackpos, index, spriteId);
}

void ProtocolGame::parseUseItemEx(NetworkMessage& msg) {
	Position fromPos = msg.getPosition();
	uint16_t fromSpriteId = msg.get<uint16_t>();
	uint8_t fromStackPos = msg.getByte();
	Position toPos = msg.getPosition();
	uint16_t toSpriteId = msg.get<uint16_t>();
	uint8_t toStackPos = msg.getByte();
	addGameTaskTimed(DISPATCHER_TASK_EXPIRATION, &Game::playerUseItemEx, player->getID(), fromPos, fromStackPos, fromSpriteId, toPos, toStackPos, toSpriteId);
}

void ProtocolGame::parseUseWithCreature(NetworkMessage& msg) {
	Position fromPos = msg.getPosition();
	uint16_t spriteId = msg.get<uint16_t>();
	uint8_t fromStackPos = msg.getByte();
	uint32_t creatureId = msg.get<uint32_t>();
	addGameTaskTimed(DISPATCHER_TASK_EXPIRATION, &Game::playerUseWithCreature, player->getID(), fromPos, fromStackPos, creatureId, spriteId);
}

void ProtocolGame::parseCloseContainer(NetworkMessage& msg) {
	addGameTask(&Game::playerCloseContainer, player->getID(), msg.getByte());
}

void ProtocolGame::parseUpArrowContainer(NetworkMessage& msg) {
	addGameTask(&Game::playerMoveUpContainer, player->getID(), msg.getByte());
}

void ProtocolGame::parseUpdateContainer(NetworkMessage& msg) {
	addGameTask(&Game::playerUpdateContainer, player->getID(), msg.getByte());
}

void ProtocolGame::parseThrow(NetworkMessage& msg) {
	Position fromPos = msg.getPosition();
	uint16_t spriteId = msg.get<uint16_t>();
	uint8_t fromStackpos = msg.getByte();
	Position toPos = msg.getPosition();
	uint8_t count = msg.getByte();
	if (toPos != fromPos) {
		addGameTaskTimed(DISPATCHER_TASK_EXPIRATION, &Game::playerMoveThing, player->getID(), fromPos, spriteId, fromStackpos, toPos, count);
	}
}
void ProtocolGame::sendOpenPrivateChannel(const std::string& receiver) {
	NetworkMessage msg;
	msg.addByte(0xAD);
	msg.addString(receiver);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendCreatureOutfit(const Creature* creature, const Outfit_t& outfit) {
	if (!canSee(creature)) return;
	NetworkMessage msg;
	msg.addByte(0x8E);
	msg.add<uint32_t>(creature->getID());
	AddOutfit(msg, outfit);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendCreatureLight(const Creature* creature) {
	if (!canSee(creature)) return;
	NetworkMessage msg;
	AddCreatureLight(msg, creature);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendWorldLight(LightInfo lightInfo) {
	NetworkMessage msg;
	AddWorldLight(msg, lightInfo);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendCreatureWalkthrough(const Creature* creature, bool walkthrough) {
	if (!canSee(creature)) return;
	NetworkMessage msg;
	msg.addByte(0x92);
	msg.add<uint32_t>(creature->getID());
	msg.addByte(walkthrough ? 0x00 : 0x01);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendCreatureShield(const Creature* creature) {
	if (!canSee(creature)) return;
	NetworkMessage msg;
	msg.addByte(0x91);
	msg.add<uint32_t>(creature->getID());
	msg.addByte(player->getPartyShield(creature->getPlayer()));
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendCreatureSkull(const Creature* creature) {
	if (g_game.getWorldType() != WORLD_TYPE_PVP || !canSee(creature)) return;
	NetworkMessage msg;
	msg.addByte(0x90);
	msg.add<uint32_t>(creature->getID());
	msg.addByte(player->getSkullClient(creature));
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendCreatureSquare(const Creature* creature, SquareColor_t color) {
	if (!canSee(creature)) return;
	NetworkMessage msg;
	msg.addByte(0x86);
	msg.add<uint32_t>(creature->getID());
	msg.addByte(color);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendTutorial(uint8_t tutorialId) {
	NetworkMessage msg;
	msg.addByte(0xDC);
	msg.addByte(tutorialId);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendAddMarker(const Position& pos, uint8_t markType, const std::string& desc) {
	NetworkMessage msg;
	msg.addByte(0xDD);
	msg.addPosition(pos);
	msg.addByte(markType);
	msg.addString(desc);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendReLoginWindow() {
	NetworkMessage msg;
	msg.addByte(0x28);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendStats() {
	NetworkMessage msg;
	AddPlayerStats(msg);
	writeToOutputBuffer(msg);
}

void ProtocolGame::sendTextMessage(const TextMessage& message) {
	NetworkMessage msg;
	msg.addByte(0xB4);
	msg.addByte(message.type);
	msg.addString(message.text);
	writeToOutputBuffer(msg);
}
void ProtocolGame::GetTileDescription(const Tile* tile, NetworkMessage& msg) {
	int32_t count = 0;
	if (Item* ground = tile->getGround()) {
		msg.addItem(ground);
		count = 1;
	}

	if (const TileItemVector* items = tile->getItemList()) {
		for (auto it = items->getBeginTopItem(), end = items->getEndTopItem(); it != end && count < 10; ++it) {
			msg.addItem(*it);
			++count;
		}
	}

	if (const CreatureVector* creatures = tile->getCreatures()) {
		for (auto it = creatures->rbegin(), end = creatures->rend(); it != end && count < 10; ++it) {
			if (player->canSeeCreature(*it)) {
				bool known;
				uint32_t removedKnown;
				checkCreatureAsKnown((*it)->getID(), known, removedKnown);
				AddCreature(msg, *it, known, removedKnown);
				++count;
			}
		}
	}

	if (items && count < 10) {
		for (auto it = items->getBeginDownItem(), end = items->getEndDownItem(); it != end && count < 10; ++it) {
			msg.addItem(*it);
			++count;
		}
	}
}

void ProtocolGame::GetMapDescription(int32_t x, int32_t y, int32_t z, int32_t width, int32_t height, NetworkMessage& msg) {
	int32_t skip = -1;
	int32_t startz, endz, zstep;

	if (z > 7) {
		startz = z - 2;
		endz = std::min<int32_t>(MAP_MAX_LAYERS - 1, z + 2);
		zstep = 1;
	}
	else {
		startz = 7;
		endz = 0;
		zstep = -1;
	}

	for (int32_t nz = startz; nz != endz + zstep; nz += zstep) {
		GetFloorDescription(msg, x, y, nz, width, height, z - nz, skip);
	}

	if (skip >= 0) {
		msg.addByte(skip);
		msg.addByte(0xFF);
	}
}

void ProtocolGame::GetFloorDescription(NetworkMessage& msg, int32_t x, int32_t y, int32_t z, int32_t width, int32_t height, int32_t offset, int32_t& skip) {
	for (int32_t nx = 0; nx < width; nx++) {
		for (int32_t ny = 0; ny < height; ny++) {
			Tile* tile = g_game.map.getTile(x + nx + offset, y + ny + offset, z);
			if (tile) {
				if (skip >= 0) {
					msg.addByte(skip);
					msg.addByte(0xFF);
				}
				skip = 0;
				GetTileDescription(tile, msg);
			}
			else if (skip == 0xFE) {
				msg.addByte(0xFF);
				msg.addByte(0xFF);
				skip = -1;
			}
			else {
				++skip;
			}
		}
	}
}

void ProtocolGame::checkCreatureAsKnown(uint32_t id, bool& known, uint32_t& removedKnown) {
	auto result = knownCreatureSet.insert(id);
	if (!result.second) {
		known = true;
		return;
	}

	known = false;
	if (knownCreatureSet.size() > 250) {
		for (auto it = knownCreatureSet.begin(), end = knownCreatureSet.end(); it != end; ++it) {
			Creature* creature = g_game.getCreatureByID(*it);
			if (!canSee(creature)) {
				removedKnown = *it;
				knownCreatureSet.erase(it);
				return;
			}
		}
		auto it = knownCreatureSet.begin();
		if (*it == id) ++it;
		removedKnown = *it;
		knownCreatureSet.erase(it);
	}
	else {
		removedKnown = 0;
	}
}

bool ProtocolGame::canSee(const Creature* c) const {
	if (!c || !player || c->isRemoved()) return false;
	if (!player->canSeeCreature(c)) return false;
	return canSee(c->getPosition());
}

bool ProtocolGame::canSee(const Position& pos) const {
	return canSee(pos.x, pos.y, pos.z);
}

bool ProtocolGame::canSee(int32_t x, int32_t y, int32_t z) const {
	if (!player) return false;

	const Position& myPos = player->getPosition();
	if (myPos.z <= 7) {
		if (z > 7) return false;
	}
	else if (std::abs(myPos.getZ() - z) > 2) {
		return false;
	}

	int32_t offsetz = myPos.getZ() - z;
	return (x >= myPos.getX() - Map::maxClientViewportX + offsetz) &&
		(x <= myPos.getX() + (Map::maxClientViewportX + 1) + offsetz) &&
		(y >= myPos.getY() - Map::maxClientViewportY + offsetz) &&
		(y <= myPos.getY() + (Map::maxClientViewportY + 1) + offsetz);
}

void ProtocolGame::AddCreature(NetworkMessage& msg, const Creature* creature, bool known, uint32_t remove) {
	const Player* otherPlayer = creature->getPlayer();
	if (known) {
		msg.add<uint16_t>(0x62);
		msg.add<uint32_t>(creature->getID());
	}
	else {
		msg.add<uint16_t>(0x61);
		msg.add<uint32_t>(remove);
		msg.add<uint32_t>(creature->getID());
		msg.addString(creature->getName());
	}

	msg.addByte(creature->isHealthHidden() ? 0x00 : std::ceil((static_cast<double>(creature->getHealth()) / std::max<int32_t>(creature->getMaxHealth(), 1)) * 100));
	msg.addByte(creature->getDirection());

	if (!creature->isInGhostMode() && !creature->isInvisible()) {
		AddOutfit(msg, creature->getCurrentOutfit());
	}
	else {
		static Outfit_t outfit;
		AddOutfit(msg, outfit);
	}

	LightInfo lightInfo = creature->getCreatureLight();
	msg.addByte(player->isAccessPlayer() ? 0xFF : lightInfo.level);
	msg.addByte(lightInfo.color);
	msg.add<uint16_t>(creature->getStepSpeed());
	msg.addByte(player->getSkullClient(otherPlayer));
	msg.addByte(player->getPartyShield(otherPlayer));
	if (!known) msg.addByte(player->getGuildEmblem(otherPlayer));
	msg.addByte(player->canWalkthroughEx(creature) ? 0x00 : 0x01);
}

void ProtocolGame::AddOutfit(NetworkMessage& msg, const Outfit_t& outfit) {
	msg.add<uint16_t>(outfit.lookType);
	if (outfit.lookType != 0) {
		msg.addByte(outfit.lookHead);
		msg.addByte(outfit.lookBody);
		msg.addByte(outfit.lookLegs);
		msg.addByte(outfit.lookFeet);
		msg.addByte(outfit.lookAddons);
	}
	else {
		msg.addItemId(outfit.lookTypeEx);
	}
}

void ProtocolGame::AddCreatureLight(NetworkMessage& msg, const Creature* creature) {
	LightInfo lightInfo = creature->getCreatureLight();
	msg.addByte(0x8D);
	msg.add<uint32_t>(creature->getID());
	msg.addByte(player->isAccessPlayer() ? 0xFF : lightInfo.level);
	msg.addByte(lightInfo.color);
}

void ProtocolGame::AddWorldLight(NetworkMessage& msg, LightInfo lightInfo) {
	msg.addByte(0x82);
	msg.addByte(player->isAccessPlayer() ? 0xFF : lightInfo.level);
	msg.addByte(lightInfo.color);
}

void ProtocolGame::AddPlayerStats(NetworkMessage& msg) {
	msg.addByte(0xA0);
	msg.add<uint16_t>(std::min<int32_t>(player->getHealth(), std::numeric_limits<uint16_t>::max()));
	msg.add<uint16_t>(std::min<int32_t>(player->getMaxHealth(), std::numeric_limits<uint16_t>::max()));
	msg.add<uint32_t>(player->getFreeCapacity());
	msg.add<uint32_t>(std::min<uint32_t>(player->getExperience(), 0x7FFFFFFF));
	msg.add<uint16_t>(player->getLevel());
	msg.addByte(player->getLevelPercent());
	msg.add<uint16_t>(std::min<int32_t>(player->getMana(), std::numeric_limits<uint16_t>::max()));
	msg.add<uint16_t>(std::min<int32_t>(player->getMaxMana(), std::numeric_limits<uint16_t>::max()));
	msg.addByte(std::min<uint32_t>(player->getMagicLevel(), std::numeric_limits<uint8_t>::max()));
	msg.addByte(player->getMagicLevelPercent());
	msg.addByte(player->getSoul());
	msg.add<uint16_t>(player->getStaminaMinutes());
}
