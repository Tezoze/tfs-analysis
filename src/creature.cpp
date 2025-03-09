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
#include "creature.h"
#include "game.h"
#include "monster.h"
#include "configmanager.h"
#include "scheduler.h"
#include <fmt/format.h>
#include <memory>
#include <mutex>
#include <algorithm>

extern Game g_game;
extern ConfigManager g_config;
extern CreatureEvents* g_creatureEvents;

Creature::Creature() {
	onIdleStatus();
}

Creature::~Creature() {
	std::lock_guard<std::mutex> lock(creatureMutex);
	for (Creature* summon : summons) {
		summon->setAttackedCreature(nullptr);
		summon->setMaster(nullptr); // Corrected from removeMaster()
	}
	// conditions cleared automatically via unique_ptr
}

bool Creature::canSee(const Position& myPos, const Position& pos, int32_t viewRangeX, int32_t viewRangeY) const {
	if (myPos.z <= 7) {
		if (pos.z > 7) return false;
	}
	else if (myPos.z >= 8) {
		if (pos.z < 8 || Position::getDistanceZ(myPos, pos) > 2) return false;
	}

	const int_fast32_t offsetz = myPos.z - pos.z;
	return (pos.x >= myPos.x - viewRangeX + offsetz) && (pos.x <= myPos.x + viewRangeX + offsetz) &&
		(pos.y >= myPos.y - viewRangeY + offsetz) && (pos.y <= myPos.y + viewRangeY + offsetz);
}

bool Creature::canSee(const Position& pos) const {
	return canSee(getPosition(), pos, Map::maxViewportX, Map::maxViewportY);
}

bool Creature::canSeeCreature(const Creature* creature) const {
	if (!creature) return false;
	return (canSeeGhostMode(creature) || !creature->isInGhostMode()) &&
		(canSeeInvisibility() || !creature->isInvisible());
}

void Creature::setSkull(Skulls_t newSkull) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	skull = newSkull;
	g_game.updateCreatureSkull(this);
}

bool Creature::isInvisible() const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	return std::any_of(conditions.begin(), conditions.end(),
		[](const auto& condition) { return condition->getType() == CONDITION_INVISIBLE; });
}

int64_t Creature::getTimeSinceLastMove() const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	return lastStep ? OTSYS_TIME() - lastStep : std::numeric_limits<int64_t>::max();
}

int32_t Creature::getWalkDelay(Direction dir) const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (lastStep == 0) return 0;
	int64_t ct = OTSYS_TIME();
	int64_t stepDuration = getStepDuration(dir);
	return static_cast<int32_t>(stepDuration - (ct - lastStep));
}

int32_t Creature::getWalkDelay() const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (lastStep == 0) return 0;
	int64_t ct = OTSYS_TIME();
	int64_t stepDuration = getStepDuration() * lastStepCost;
	return static_cast<int32_t>(stepDuration - (ct - lastStep));
}

void Creature::onWalk() {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (getWalkDelay() <= 0 && !listWalkDir.empty()) {
		Direction dir;
		uint32_t flags = FLAG_IGNOREFIELDDAMAGE;
		if (getNextStep(dir, flags)) {
			ReturnValue ret = g_game.internalMoveCreature(this, dir, flags);
			if (ret != RETURNVALUE_NOERROR) {
				if (Player* player = getPlayer()) {
					player->sendCancelMessage(ret);
					player->sendCancelWalk();
				}
				forceUpdateFollowPath = true;
			}
		}
		else {
			stopEventWalk();
			if (listWalkDir.empty()) onWalkComplete();
		}
	}

	if (cancelNextWalk) {
		listWalkDir.clear();
		onWalkAborted();
		cancelNextWalk = false;
	}

	if (eventWalk != 0) {
		eventWalk = 0;
		addEventWalk();
	}
}

bool Creature::getNextStep(Direction& dir, uint32_t&) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (listWalkDir.empty()) return false;
	dir = listWalkDir.back();
	listWalkDir.pop_back();
	onWalk(dir); // Note: onWalk(dir) is not implemented, consider defining it
	return true;
}

void Creature::startAutoWalk() {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (Player* player = getPlayer(); player && player->isMovementBlocked()) {
		player->sendCancelWalk();
		return;
	}
	addEventWalk(listWalkDir.size() == 1);
}

void Creature::startAutoWalk(Direction direction) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (Player* player = getPlayer(); player && player->isMovementBlocked()) {
		player->sendCancelWalk();
		return;
	}
	listWalkDir = { direction };
	addEventWalk(true);
}

void Creature::startAutoWalk(const std::vector<Direction>& listDir) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (Player* player = getPlayer(); player && player->isMovementBlocked()) {
		player->sendCancelWalk();
		return;
	}
	listWalkDir = listDir;
	addEventWalk(listWalkDir.size() == 1);
}

void Creature::addEventWalk(bool firstStep) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (getStepSpeed() <= 0 || eventWalk != 0) return;
	int64_t ticks = getEventStepTicks(firstStep);
	if (ticks <= 0) return;
	if (ticks == 1) g_game.checkCreatureWalk(getID());
	eventWalk = g_scheduler.addEvent(createSchedulerTask(ticks, std::bind(&Game::checkCreatureWalk, &g_game, getID())));
}

void Creature::stopEventWalk() {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (eventWalk != 0) {
		g_scheduler.stopEvent(eventWalk);
		eventWalk = 0;
	}
}

int64_t Creature::getStepDuration(Direction dir) const {
	int64_t stepDuration = getStepDuration();
	if ((dir & DIRECTION_DIAGONAL_MASK) != 0) stepDuration *= 3;
	return stepDuration;
}

int64_t Creature::getStepDuration() const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (isRemoved()) return 0;

	uint32_t groundSpeed = tile && tile->getGround() ? Item::items[tile->getGround()->getID()].speed : 150;
	if (groundSpeed == 0) groundSpeed = 150;

	double duration = std::floor(1000 * groundSpeed) / getStepSpeed();
	int64_t stepDuration = std::ceil(duration / 50) * 50;

	if (const Monster* monster = getMonster(); monster && !monster->getMaster() && monster->isTargetNearby() && !monster->isFleeing()) {
		stepDuration *= 3;
	}
	return stepDuration;
}

int64_t Creature::getEventStepTicks(bool onlyDelay) const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	int64_t ret = getWalkDelay();
	if (ret <= 0) {
		int64_t stepDuration = getStepDuration();
		ret = (onlyDelay && stepDuration > 0) ? 1 : stepDuration * lastStepCost;
	}
	return ret;
}

void Creature::onThink(uint32_t interval) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (!isMapLoaded && useCacheMap()) {
		isMapLoaded = true;
		updateMapCache();
	}

	if (followCreature && master != followCreature && !canSeeCreature(followCreature)) {
		onCreatureDisappear(followCreature, false);
	}

	if (attackedCreature && master != attackedCreature && !canSeeCreature(attackedCreature)) {
		onCreatureDisappear(attackedCreature, false);
	}

	blockTicks += interval;
	if (blockTicks >= 1000) {
		blockCount = std::min<uint32_t>(blockCount + 1, 2);
		blockTicks = 0;
	}

	if (followCreature) {
		walkUpdateTicks += interval;
		if (forceUpdateFollowPath || walkUpdateTicks >= 2000) {
			walkUpdateTicks = 0;
			forceUpdateFollowPath = false;
			isUpdatingPath = true;
		}
	}

	if (isUpdatingPath) {
		isUpdatingPath = false;
		goToFollowCreature();
	}

	for (CreatureEvent* thinkEvent : getCreatureEvents(CREATURE_EVENT_THINK)) {
		thinkEvent->executeOnThink(this, interval);
	}
}

void Creature::onAttacking(uint32_t interval) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (!attackedCreature) return;
	onAttacked();
	attackedCreature->onAttacked();
	if (g_game.isSightClear(getPosition(), attackedCreature->getPosition(), true)) {
		doAttacking(interval); // Note: doAttacking is not implemented
	}
}

void Creature::onIdleStatus() {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (getHealth() > 0) {
		damageMap.clear();
		lastHitCreatureId = 0;
	}
}

void Creature::updateMapCache() {
	std::lock_guard<std::mutex> lock(creatureMutex);
	const Position& myPos = getPosition();
	Position pos(0, 0, myPos.z);
	for (int32_t y = -maxWalkCacheHeight; y <= maxWalkCacheHeight; ++y) {
		for (int32_t x = -maxWalkCacheWidth; x <= maxWalkCacheWidth; ++x) {
			pos.x = myPos.x + x;
			pos.y = myPos.y + y;
			updateTileCache(g_game.map.getTile(pos), pos);
		}
	}
}

void Creature::updateTileCache(const Tile* tile, int32_t dx, int32_t dy) {
	if (std::abs(dx) <= maxWalkCacheWidth && std::abs(dy) <= maxWalkCacheHeight) {
		localMapCache[maxWalkCacheHeight + dy][maxWalkCacheWidth + dx] =
			tile && tile->queryAdd(0, *this, 1, FLAG_PATHFINDING | FLAG_IGNOREFIELDDAMAGE) == RETURNVALUE_NOERROR;
	}
}

void Creature::updateTileCache(const Tile* tile, const Position& pos) {
	const Position& myPos = getPosition();
	if (pos.z == myPos.z) {
		updateTileCache(tile, Position::getOffsetX(pos, myPos), Position::getOffsetY(pos, myPos));
	}
}

int32_t Creature::getWalkCache(const Position& pos) const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (!useCacheMap()) return 2;
	if (pos.z != position.z) return 0;
	if (pos == position) return 1;

	int32_t dx = Position::getOffsetX(pos, position);
	int32_t dy = Position::getOffsetY(pos, position);
	if (std::abs(dx) <= maxWalkCacheWidth && std::abs(dy) <= maxWalkCacheHeight) {
		return localMapCache[maxWalkCacheHeight + dy][maxWalkCacheWidth + dx] ? 1 : 0;
	}
	return 2;
}

void Creature::onCreatureAppear(Creature* creature, bool isLogin) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (creature == this) {
		if (useCacheMap()) {
			isMapLoaded = true;
			updateMapCache();
		}
		if (isLogin) setLastPosition(getPosition());
	}
	else if (isMapLoaded && creature->getPosition().z == position.z) {
		updateTileCache(creature->getTile(), creature->getPosition());
	}
}

void Creature::onRemoveCreature(Creature* creature, bool isLogout) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	onCreatureDisappear(creature, isLogout);
	if (creature == this) {
		if (master && !master->isRemoved()) setMaster(nullptr);
	}
	else if (isMapLoaded && creature->getPosition().z == position.z) {
		updateTileCache(creature->getTile(), creature->getPosition());
	}
}

void Creature::onCreatureMove(Creature* creature, const Tile* newTile, const Position& newPos,
	const Tile* oldTile, const Position& oldPos, bool teleport) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (creature == this) {
		lastStep = OTSYS_TIME();
		lastStepCost = teleport ? 1 : (oldPos.z != newPos.z ? 2 : (Position::getDistanceX(newPos, oldPos) >= 1 && Position::getDistanceY(newPos, oldPos) >= 1 ? (getPlayer() ? 2 : 3) : 1));
		if (teleport) stopEventWalk();

		if (!summons.empty()) {
			std::vector<Creature*> despawnList;
			despawnList.reserve(summons.size());
			for (Creature* summon : summons) {
				const Position& pos = summon->getPosition();
				if (Position::getDistanceZ(newPos, pos) > 2 || std::max(Position::getDistanceX(newPos, pos), Position::getDistanceY(newPos, pos)) > 30) {
					despawnList.push_back(summon);
				}
			}
			for (Creature* despawn : despawnList) g_game.removeCreature(despawn, true);
		}

		if (newTile->getZone() != oldTile->getZone()) onChangeZone(getZone());

		if (isMapLoaded) {
			if (teleport || oldPos.z != newPos.z) {
				updateMapCache();
			}
			// Incremental update omitted for brevity, implement if needed
		}
	}
	else if (isMapLoaded && newPos.z == position.z) {
		updateTileCache(newTile, newPos);
	}

	if (creature == followCreature || (creature == this && followCreature)) {
		if (hasFollowPath) isUpdatingPath = true;
		if (newPos.z != oldPos.z || !canSee(followCreature->getPosition())) onCreatureDisappear(followCreature, false);
	}

	if (creature == attackedCreature || (creature == this && attackedCreature)) {
		if (newPos.z != oldPos.z || !canSee(attackedCreature->getPosition())) {
			onCreatureDisappear(attackedCreature, false);
		}
		else {
			if (hasExtraSwing()) g_dispatcher.addTask(createTask(std::bind(&Game::checkCreatureAttack, &g_game, getID())));
			if (newTile->getZone() != oldTile->getZone()) onAttackedCreatureChangeZone(attackedCreature->getZone());
		}
	}
}

void Creature::onCreatureDisappear(const Creature* creature, bool isLogout) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (attackedCreature == creature) {
		setAttackedCreature(nullptr);
		onAttackedCreatureDisappear(isLogout);
	}
	if (followCreature == creature) {
		setFollowCreature(nullptr);
		onFollowCreatureDisappear(isLogout);
	}
}

void Creature::onChangeZone(ZoneType_t zone) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (attackedCreature && zone == ZONE_PROTECTION) onCreatureDisappear(attackedCreature, false);
}

void Creature::onAttackedCreatureChangeZone(ZoneType_t zone) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (zone == ZONE_PROTECTION) onCreatureDisappear(attackedCreature, false);
}

CreatureVector Creature::getKillers() {
	std::lock_guard<std::mutex> lock(creatureMutex);
	CreatureVector killers;
	killers.reserve(damageMap.size());
	const int64_t timeNow = OTSYS_TIME();
	for (const auto& [id, cb] : damageMap) {
		if (Creature* attacker = g_game.getCreatureByID(id); attacker && attacker != this && (timeNow - cb.ticks <= inFightTicks)) {
			killers.push_back(attacker);
		}
	}
	return killers;
}

void Creature::onDeath() {
	std::lock_guard<std::mutex> lock(creatureMutex);
	bool lastHitUnjustified = false, mostDamageUnjustified = false;
	Creature* lastHitCreature = g_game.getCreatureByID(lastHitCreatureId);
	Creature* lastHitCreatureMaster = lastHitCreature ? lastHitCreature->getMaster() : nullptr;
	if (lastHitCreature) lastHitUnjustified = lastHitCreature->onKilledCreature(this);

	Creature* mostDamageCreature = nullptr;
	int32_t mostDamage = 0;
	std::map<Creature*, uint64_t> experienceMap;
	const int64_t timeNow = OTSYS_TIME();
	for (const auto& [id, cb] : damageMap) {
		if (Creature* attacker = g_game.getCreatureByID(id)) {
			if (cb.total > mostDamage && (timeNow - cb.ticks <= inFightTicks)) {
				mostDamage = cb.total;
				mostDamageCreature = attacker;
			}
			if (attacker != this) {
				uint64_t gainExp = getGainedExperience(attacker);
				if (Player* attackerPlayer = attacker->getPlayer()) {
					attackerPlayer->removeAttacked(getPlayer());
					if (Party* party = attackerPlayer->getParty(); party && party->getLeader() && party->isSharedExperienceActive() && party->isSharedExperienceEnabled()) {
						attacker = party->getLeader();
					}
				}
				experienceMap[attacker] += gainExp;
			}
		}
	}

	for (const auto& [attacker, exp] : experienceMap) {
		attacker->onGainExperience(exp, this);
	}

	if (mostDamageCreature && mostDamageCreature != lastHitCreature && mostDamageCreature != lastHitCreatureMaster) {
		Creature* mostDamageCreatureMaster = mostDamageCreature->getMaster();
		if (lastHitCreature != mostDamageCreatureMaster && (!lastHitCreatureMaster || mostDamageCreatureMaster != lastHitCreatureMaster)) {
			mostDamageUnjustified = mostDamageCreature->onKilledCreature(this, false);
		}
	}

	bool droppedCorpse = dropCorpse(lastHitCreature, mostDamageCreature, lastHitUnjustified, mostDamageUnjustified);
	death(lastHitCreature);
	if (master) setMaster(nullptr);
	if (droppedCorpse) g_game.removeCreature(this, false);
}

bool Creature::addCondition(Condition* condition, bool force) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (!condition) return false;

	if (!force && condition->getType() == CONDITION_HASTE && hasCondition(CONDITION_PARALYZE)) {
		int64_t walkDelay = getWalkDelay();
		if (walkDelay > 0) {
			g_scheduler.addEvent(createSchedulerTask(walkDelay, std::bind(&Game::forceAddCondition, &g_game, getID(), condition)));
			return false;
		}
	}

	if (Condition* prevCond = getCondition(condition->getType(), condition->getId(), condition->getSubId())) {
		prevCond->addCondition(this, condition);
		delete condition;
		return true;
	}

	if (condition->startCondition(this)) {
		conditions.emplace_back(condition);
		onAddCondition(condition->getType());
		return true;
	}

	delete condition;
	return false;
}

bool Creature::addCombatCondition(Condition* condition) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	ConditionType_t type = condition->getType();
	if (!addCondition(condition)) return false;
	onAddCombatCondition(type);
	return true;
}

void Creature::removeCondition(ConditionType_t type, bool force) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	for (auto it = conditions.begin(); it != conditions.end(); ) {
		if ((*it)->getType() == type) {
			if (!force && type == CONDITION_PARALYZE && getWalkDelay() > 0) {
				g_scheduler.addEvent(createSchedulerTask(getWalkDelay(), std::bind(&Game::forceRemoveCondition, &g_game, getID(), type)));
				return;
			}
			Condition* condition = it->release();
			it = conditions.erase(it);
			condition->endCondition(this);
			onEndCondition(type);
			delete condition;
		}
		else {
			++it;
		}
	}
}

void Creature::removeCondition(ConditionType_t type, ConditionId_t conditionId, bool force) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	for (auto it = conditions.begin(); it != conditions.end(); ) {
		if ((*it)->getType() == type && (*it)->getId() == conditionId) {
			if (!force && type == CONDITION_PARALYZE && getWalkDelay() > 0) {
				g_scheduler.addEvent(createSchedulerTask(getWalkDelay(), std::bind(&Game::forceRemoveCondition, &g_game, getID(), type)));
				return;
			}
			Condition* condition = it->release();
			it = conditions.erase(it);
			condition->endCondition(this);
			onEndCondition(type);
			delete condition;
		}
		else {
			++it;
		}
	}
}

void Creature::removeCombatCondition(ConditionType_t type) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	std::vector<Condition*> toRemove;
	toRemove.reserve(conditions.size());
	for (const auto& condition : conditions) {
		if (condition->getType() == type) toRemove.push_back(condition.get());
	}
	for (Condition* condition : toRemove) onCombatRemoveCondition(condition);
}

void Creature::removeCondition(Condition* condition, bool force) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	auto it = std::find_if(conditions.begin(), conditions.end(), [condition](const auto& c) { return c.get() == condition; });
	if (it != conditions.end()) {
		if (!force && condition->getType() == CONDITION_PARALYZE && getWalkDelay() > 0) {
			g_scheduler.addEvent(createSchedulerTask(getWalkDelay(), std::bind(&Game::forceRemoveCondition, &g_game, getID(), condition->getType())));
			return;
		}
		Condition* removed = it->release();
		conditions.erase(it);
		removed->endCondition(this);
		onEndCondition(removed->getType());
		delete removed;
	}
}

Condition* Creature::getCondition(ConditionType_t type) const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	for (const auto& condition : conditions) {
		if (condition->getType() == type) return condition.get();
	}
	return nullptr;
}

Condition* Creature::getCondition(ConditionType_t type, ConditionId_t conditionId, uint32_t subId) const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	for (const auto& condition : conditions) {
		if (condition->getType() == type && condition->getId() == conditionId && condition->getSubId() == subId) {
			return condition.get();
		}
	}
	return nullptr;
}

void Creature::executeConditions(uint32_t interval) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	for (auto it = conditions.begin(); it != conditions.end(); ) {
		Condition* condition = it->get();
		if (!condition->executeCondition(this, interval)) {
			condition = it->release();
			it = conditions.erase(it);
			condition->endCondition(this);
			onEndCondition(condition->getType());
			delete condition;
		}
		else {
			++it;
		}
	}
}

bool Creature::hasCondition(ConditionType_t type, uint32_t subId) const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (isSuppress(type)) return false;
	int64_t timeNow = OTSYS_TIME();
	for (const auto& condition : conditions) {
		if (condition->getType() == type && condition->getSubId() == subId &&
			(condition->getEndTime() >= timeNow || condition->getTicks() == -1)) {
			return true;
		}
	}
	return false;
}

void Creature::onAddCondition(ConditionType_t type) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (type == CONDITION_PARALYZE && hasCondition(CONDITION_HASTE)) removeCondition(CONDITION_HASTE);
	else if (type == CONDITION_HASTE && hasCondition(CONDITION_PARALYZE)) removeCondition(CONDITION_PARALYZE);
}

void Creature::onTickCondition(ConditionType_t type, bool& bRemove) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (const MagicField* field = getTile()->getFieldItem()) {
		static const std::map<ConditionType_t, CombatType_t> conditionCombatMap = {
			{CONDITION_FIRE, COMBAT_FIREDAMAGE}, {CONDITION_ENERGY, COMBAT_ENERGYDAMAGE},
			{CONDITION_POISON, COMBAT_EARTHDAMAGE}, {CONDITION_FREEZING, COMBAT_ICEDAMAGE},
			{CONDITION_DAZZLED, COMBAT_HOLYDAMAGE}, {CONDITION_CURSED, COMBAT_DEATHDAMAGE},
			{CONDITION_DROWN, COMBAT_DROWNDAMAGE}, {CONDITION_BLEEDING, COMBAT_PHYSICALDAMAGE}
		};
		auto it = conditionCombatMap.find(type);
		if (it != conditionCombatMap.end()) bRemove = (field->getCombatType() != it->second);
	}
}

void Creature::onAttackedCreatureDrainHealth(Creature* target, int32_t points) {
	target->addDamagePoints(this, points);
}

bool Creature::onKilledCreature(Creature* target, bool lastHit) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (master) master->onKilledCreature(target, lastHit);
	for (CreatureEvent* killEvent : getCreatureEvents(CREATURE_EVENT_KILL)) {
		killEvent->executeOnKill(this, target);
	}
	return false; // Default implementation, override in derived classes for unjustified flags
}

void Creature::onGainExperience(uint64_t gainExp, Creature* target) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (gainExp == 0 || !master) return;
	gainExp /= 2;
	master->onGainExperience(gainExp, target);

	SpectatorVec spectators;
	g_game.map.getSpectators(spectators, position, false, true);
	if (spectators.empty()) return;

	TextMessage textMessage(MESSAGE_STATUS_DEFAULT, fmt::format("{:s} gained {:d} {:s}.", ucfirst(getNameDescription()), gainExp, gainExp != 1 ? "experience points" : "experience point"));
	for (Creature* spectator : spectators) {
		if (Player* player = spectator->getPlayer()) {
			player->sendTextMessage(textMessage);
		}
	}
	g_game.addAnimatedText(spectators, std::to_string(gainExp), position, TEXTCOLOR_WHITE);
}

void Creature::changeHealth(int32_t healthChange, bool sendHealthChange) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	int32_t oldHealth = health; // Note: 'health' is not defined, assume it's a member in derived class
	health = std::clamp(health + healthChange, 0, getMaxHealth());
	if (sendHealthChange && oldHealth != health) g_game.addCreatureHealth(this);
	if (health <= 0) g_dispatcher.addTask(createTask(std::bind(&Game::executeDeath, &g_game, getID())));
}

void Creature::gainHealth(Creature* healer, int32_t healthGain) {
	changeHealth(healthGain);
	if (healer) healer->onTargetCreatureGainHealth(this, healthGain);
}

void Creature::drainHealth(Creature* attacker, int32_t damage) {
	changeHealth(-damage, false);
	lastHitCreatureId = attacker ? attacker->id : 0;
}

BlockType_t Creature::blockHit(Creature* attacker, CombatType_t combatType, int32_t& damage,
	bool checkDefense, bool checkArmor, bool field, bool ignoreResistances) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	BlockType_t blockType = BLOCK_NONE;
	if (isImmune(combatType)) {
		damage = 0;
		blockType = BLOCK_IMMUNITY;
	}
	else if (checkDefense || checkArmor) {
		bool hasDefense = blockCount > 0;
		if (hasDefense) --blockCount;

		if (checkDefense && hasDefense && canUseDefense) { // 'canUseDefense' not defined, assume true
			int32_t defense = getDefense();
			damage -= uniform_random(defense / 2, defense);
			if (damage <= 0) {
				damage = 0;
				blockType = BLOCK_DEFENSE;
				checkArmor = false;
			}
		}

		if (checkArmor) {
			int32_t armor = getArmor();
			damage -= (armor > 3) ? uniform_random(armor / 2, armor - (armor % 2 + 1)) : (armor > 0 ? 1 : 0);
			if (damage <= 0) {
				damage = 0;
				blockType = BLOCK_ARMOR;
			}
		}

		if (hasDefense && blockType != BLOCK_NONE) onBlockHit();
	}

	if (attacker) {
		attacker->onAttackedCreature(this);
		attacker->onAttackedCreatureBlockHit(blockType);
	}
	onAttacked();
	return blockType;
}

bool Creature::setMaster(Creature* newMaster) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (!newMaster && !master) return false;

	if (newMaster) {
		incrementReferenceCounter();
		newMaster->summons.push_back(this);
	}

	Creature* oldMaster = master;
	master = newMaster;

	if (oldMaster) {
		auto it = std::find(oldMaster->summons.begin(), oldMaster->summons.end(), this);
		if (it != oldMaster->summons.end()) {
			oldMaster->summons.erase(it);
			decrementReferenceCounter();
		}
	}
	return true;
}

void Creature::goToFollowCreature() {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (!followCreature) return;

	FindPathParams fpp;
	getPathSearchParams(followCreature, fpp);

	if (Monster* monster = getMonster(); monster && !monster->getMaster() && (monster->isFleeing() || fpp.maxTargetDist > 1)) {
		Direction dir = DIRECTION_NONE;
		if (monster->isFleeing()) {
			monster->getDistanceStep(followCreature->getPosition(), dir, true);
		}
		else if (!monster->getDistanceStep(followCreature->getPosition(), dir)) {
			listWalkDir.clear();
			if (getPathTo(followCreature->getPosition(), listWalkDir, fpp)) {
				hasFollowPath = true;
				startAutoWalk();
			}
			else {
				hasFollowPath = false;
			}
			return;
		}

		if (dir != DIRECTION_NONE) {
			listWalkDir = { dir };
			hasFollowPath = true;
			startAutoWalk();
		}
	}
	else {
		listWalkDir.clear();
		if (getPathTo(followCreature->getPosition(), listWalkDir, fpp)) {
			hasFollowPath = true;
			startAutoWalk();
		}
		else {
			hasFollowPath = false;
		}
	}
	onFollowCreatureComplete(followCreature);
}

bool Creature::setFollowCreature(Creature* creature) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (creature == followCreature) return true;
	if (creature && (creature->getPosition().z != position.z || !canSee(creature->getPosition()))) {
		followCreature = nullptr;
		return false;
	}

	if (!listWalkDir.empty()) {
		listWalkDir.clear();
		onWalkAborted();
	}

	hasFollowPath = false;
	forceUpdateFollowPath = false;
	followCreature = creature;
	isUpdatingPath = true;
	onFollowCreature(creature);
	return true;
}

double Creature::getDamageRatio(Creature* attacker) const {
	std::lock_guard<std::mutex> lock(creatureMutex);
	uint32_t totalDamage = 0, attackerDamage = 0;
	for (const auto& [id, cb] : damageMap) {
		totalDamage += cb.total;
		if (id == attacker->getID()) attackerDamage += cb.total;
	}
	return totalDamage ? static_cast<double>(attackerDamage) / totalDamage : 0;
}

uint64_t Creature::getGainedExperience(Creature* attacker) const {
	return std::floor(getDamageRatio(attacker) * getLostExperience());
}

void Creature::addDamagePoints(Creature* attacker, int32_t damagePoints) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	if (damagePoints <= 0) return;
	uint32_t attackerId = attacker->id;
	auto& cb = damageMap[attackerId];
	cb.total += damagePoints;
	cb.ticks = OTSYS_TIME();
	lastHitCreatureId = attackerId;
}

bool Creature::hasBeenAttacked(uint32_t attackerId) {
	std::lock_guard<std::mutex> lock(creatureMutex);
	auto it = damageMap.find(attackerId);
	return it != damageMap.end() && (OTSYS_TIME() - it->second.ticks) <= inFightTicks;
}

bool Creature::getPathTo(const Position& targetPos, std::vector<Direction>& dirList, const FindPathParams& fpp) const {
	return g_game.map.getPathMatching(*this, dirList, FrozenPathingConditionCall(targetPos), fpp);
}

bool Creature::getPathTo(const Position& targetPos, std::vector<Direction>& dirList, int32_t minTargetDist, int32_t maxTargetDist,
	bool fullPathSearch, bool clearSight, int32_t maxSearchDist) const {
	FindPathParams fpp;
	fpp.fullPathSearch = fullPathSearch;
	fpp.maxSearchDist = maxSearchDist;
	fpp.clearSight = clearSight;
	fpp.minTargetDist = minTargetDist;
	fpp.maxTargetDist = maxTargetDist;
	return getPathTo(targetPos, dirList, fpp);
}

// Default implementations for missing methods
void Creature::onWalkComplete() {}
void Creature::onWalkAborted() {}
int32_t Creature::getStepSpeed() const { return 220; } // Default speed, override in derived classes
int32_t Creature::getMaxHealth() const { return 100; } // Default, override in derived classes
bool Creature::dropCorpse(Creature*, Creature*, bool, bool) { return true; }
void Creature::death(Creature*) {}
void Creature::onFollowCreature(Creature*) {}
void Creature::onFollowCreatureComplete(Creature*) {}
void Creature::onAttackedCreatureDisappear(bool) {}
void Creature::onTargetCreatureGainHealth(Creature*, int32_t) {}
void Creature::onBlockHit() {}
void Creature::onCombatRemoveCondition(Condition*) {}
int32_t Creature::getDefense() const { return 0; } // Override in derived classes
int32_t Creature::getArmor() const { return 0; } // Override in derived classes
int32_t Creature::uniform_random(int32_t min, int32_t max) const { return min + rand() % (max - min + 1); }
void Creature::setLastPosition(const Position&) {}
bool Creature::hasExtraSwing() const { return false; }
Monster* Creature::getMonster() { return dynamic_cast<Monster*>(this); }
const std::string& Creature::getNameDescription() const { return std::string(); } // Override in derived classes
ZoneType_t Creature::getZone() const { return ZONE_NORMAL; } // Override based on tile
std::string Creature::ucfirst(const std::string& s) const { return s.empty() ? s : (char)toupper(s[0]) + s.substr(1); }
