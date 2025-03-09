#include "otpch.h"
#include "bed.h"
#include "chat.h"
#include "combat.h"
#include "configmanager.h"
#include "creatureevent.h"
#include "events.h"
#include "game.h"
#include "iologindata.h"
#include "monster.h"
#include "movement.h"
#include "scheduler.h"
#include "weapons.h"

#include <fmt/format.h>
#include <array>
#include <mutex>
#include <memory> // For unique_ptr

extern ConfigManager g_config;
extern Game g_game;
extern Chat* g_chat;
extern Vocations g_vocations;
extern MoveEvents* g_moveEvents;
extern Weapons* g_weapons;
extern CreatureEvents* g_creatureEvents;
extern Events* g_events;

using MuteCountMap = std::unordered_map<uint32_t, uint32_t>;
MuteCountMap Player::muteCountMap;
std::mutex Player::muteCountMutex;

uint32_t Player::playerAutoID = 0x10000000;

class Player : public Creature {
private:
	static const int32_t kickAfterMinutes = g_config.getNumber(ConfigManager::KICK_AFTER_MINUTES);
	static const uint32_t unjustKillThreshold = g_config.getNumber(ConfigManager::UNJUST_KILL_THRESHOLD);
	static const uint32_t pzLockedDuration = g_config.getNumber(ConfigManager::PZ_LOCKED);
	static constexpr uint32_t SCHEDULER_MINTICKS = 50; // Example value, adjust as needed
	std::array<std::unique_ptr<Item>, CONST_SLOT_LAST + 1> inventory;

public:
	Player(ProtocolGame_ptr p) noexcept :
		Creature(),
		lastPing(OTSYS_TIME()),
		lastPong(lastPing),
		client(std::move(p))
	{
		// unique_ptr defaults to nullptr, no fill needed
	}

	~Player()
	{
		// No manual cleanup for inventory, handled by unique_ptr
		setWriteItem(nullptr);
		setEditHouse(nullptr);
	}
	// Other methods follow...
};
bool Player::setVocation(uint16_t vocId) noexcept
{
	Vocation* voc = g_vocations.getVocation(vocId);
	if (!voc) {
		return false;
	}
	vocation = voc;
	updateRegeneration();
	return true;
}

bool Player::isPushable() const noexcept
{
	return !hasFlag(PlayerFlag_CannotBePushed) && Creature::isPushable();
}

std::string Player::getDescription(int32_t lookDistance) const
{
	std::string result;
	result.reserve(256);

	if (lookDistance == -1) {
		result = "yourself.";
		if (group && group->access) {
			result += " You are ";
			result += group->name;
			result += '.';
		}
		else if (vocation && vocation->getId() != VOCATION_NONE) {
			result += " You are ";
			result += vocation->getVocDescription();
			result += '.';
		}
		else {
			result += " You have no vocation.";
		}
	}
	else {
		result = name;
		if (group && !group->access) {
			result += " (Level ";
			result += std::to_string(level);
			result += ')';
		}
		result += '.';
		result += (sex == PLAYERSEX_FEMALE) ? " She" : " He";
		if (group && group->access) {
			result += " is ";
			result += group->name;
			result += '.';
		}
		else if (vocation && vocation->getId() != VOCATION_NONE) {
			result += " is ";
			result += vocation->getVocDescription();
			result += '.';
		}
		else {
			result += " has no vocation.";
		}
	}

	if (party) {
		result += (lookDistance == -1) ? " Your party has " : ((sex == PLAYERSEX_FEMALE) ? " She is in a party with " : " He is in a party with ");
		size_t memberCount = party->getMemberCount() + 1;
		result += std::to_string(memberCount);
		result += (memberCount == 1) ? " member and " : " members and ";
		size_t invitationCount = party->getInvitationCount();
		result += std::to_string(invitationCount);
		result += (invitationCount == 1) ? " pending invitation." : " pending invitations.";
	}

	if (guild && guildRank) {
		result += (lookDistance == -1) ? " You are " : ((sex == PLAYERSEX_FEMALE) ? " She is " : " He is ");
		result += guildRank->name;
		result += " of the ";
		result += guild->getName();
		if (!guildNick.empty()) {
			result += " (";
			result += guildNick;
			result += ')';
		}
		size_t memberCount = guild->getMemberCount();
		result += ", which has ";
		result += std::to_string(memberCount);
		result += (memberCount == 1) ? " member, " : " members, ";
		result += std::to_string(guild->getMembersOnline().size());
		result += " of them online.";
	}

	return result;
}
Item* Player::getInventoryItem(slots_t slot) const noexcept
{
	if (static_cast<uint32_t>(slot) < CONST_SLOT_FIRST || static_cast<uint32_t>(slot) > CONST_SLOT_LAST) {
		return nullptr;
	}
	return inventory[slot].get();
}

Item* Player::getWeapon(slots_t slot, bool ignoreAmmo) const noexcept
{
	Item* item = getInventoryItem(slot);
	if (!item) {
		return nullptr;
	}

	WeaponType_t weaponType = item->getWeaponType();
	if (weaponType == WEAPON_NONE || weaponType == WEAPON_SHIELD || weaponType == WEAPON_AMMO) {
		return nullptr;
	}

	if (!ignoreAmmo && weaponType == WEAPON_DISTANCE) {
		const ItemType& it = Item::items.getItemType(item->getID());
		if (it.ammoType != AMMO_NONE) {
			Item* ammoItem = getInventoryItem(CONST_SLOT_AMMO);
			if (!ammoItem || ammoItem->getAmmoType() != it.ammoType) {
				return nullptr;
			}
			return ammoItem;
		}
	}
	return item;
}

Item* Player::getWeapon(bool ignoreAmmo) const noexcept
{
	if (Item* item = getWeapon(CONST_SLOT_LEFT, ignoreAmmo)) {
		return item;
	}
	return getWeapon(CONST_SLOT_RIGHT, ignoreAmmo);
}

WeaponType_t Player::getWeaponType() const noexcept
{
	if (Item* item = getWeapon()) {
		return item->getWeaponType();
	}
	return WEAPON_NONE;
}

int32_t Player::getWeaponSkill(const Item* item) const noexcept
{
	if (!item) {
		return getSkillLevel(SKILL_FIST);
	}

	switch (item->getWeaponType()) {
	case WEAPON_SWORD: return getSkillLevel(SKILL_SWORD);
	case WEAPON_CLUB: return getSkillLevel(SKILL_CLUB);
	case WEAPON_AXE: return getSkillLevel(SKILL_AXE);
	case WEAPON_DISTANCE: return getSkillLevel(SKILL_DISTANCE);
	default: return 0;
	}
}

int32_t Player::getArmor() const noexcept
{
	int32_t armor = 0;
	static constexpr std::array<slots_t, 6> armorSlots = {
		CONST_SLOT_HEAD, CONST_SLOT_NECKLACE, CONST_SLOT_ARMOR,
		CONST_SLOT_LEGS, CONST_SLOT_FEET, CONST_SLOT_RING
	};

	for (slots_t slot : armorSlots) {
		if (Item* item = inventory[slot].get()) {
			armor += item->getArmor();
		}
	}
	return static_cast<int32_t>(armor * (vocation ? vocation->armorMultiplier : 1.0f));
}

void Player::getShieldAndWeapon(const Item*& shield, const Item*& weapon) const noexcept
{
	shield = nullptr;
	weapon = nullptr;

	for (uint32_t slot = CONST_SLOT_RIGHT; slot <= CONST_SLOT_LEFT; ++slot) {
		Item* item = inventory[slot].get();
		if (!item) {
			continue;
		}

		switch (item->getWeaponType()) {
		case WEAPON_SHIELD:
			if (!shield || item->getDefense() > shield->getDefense()) {
				shield = item;
			}
			break;
		case WEAPON_NONE:
			break;
		default:
			weapon = item;
			break;
		}
	}
}

int32_t Player::getDefense() const noexcept
{
	int32_t defenseSkill = getSkillLevel(SKILL_FIST);
	int32_t defenseValue = 7;
	const Item* weapon = nullptr;
	const Item* shield = nullptr;
	getShieldAndWeapon(shield, weapon);

	if (weapon) {
		defenseValue = weapon->getDefense() + weapon->getExtraDefense();
		defenseSkill = getWeaponSkill(weapon);
	}

	if (shield) {
		defenseValue = weapon ? shield->getDefense() + weapon->getExtraDefense() : shield->getDefense();
		defenseSkill = getSkillLevel(SKILL_SHIELD);
	}

	if (defenseSkill == 0) {
		return (fightMode == FIGHTMODE_DEFENSE) ? 2 : 1;
	}

	return static_cast<int32_t>((defenseSkill / 4.0f + 2.23f) * defenseValue * 0.15f * getDefenseFactor() * (vocation ? vocation->defenseMultiplier : 1.0f));
}
void Player::addExperience(Creature* source, uint64_t exp, bool sendText)
{
	if (exp == 0 || !vocation) {
		return;
	}

	uint64_t currLevelExp = Player::getExpForLevel(level);
	uint64_t nextLevelExp = Player::getExpForLevel(level + 1);
	if (currLevelExp >= nextLevelExp) {
		levelPercent = 0;
		sendStats();
		return;
	}

	g_events->eventPlayerOnGainExperience(this, source, exp, exp);
	if (exp == 0) {
		return;
	}

	experience += exp;
	if (sendText && client) {
		std::string expString = std::to_string(exp) + (exp != 1 ? " experience points." : " experience point.");
		sendTextMessage(MESSAGE_STATUS_DEFAULT, "You gained " + expString);
		g_game.addAnimatedText(std::to_string(exp), position, TEXTCOLOR_WHITE);

		SpectatorVec spectators;
		g_game.map.getSpectators(spectators, position, false, true);
		if (!spectators.empty()) {
			spectators.erase(this);
			std::string spectatorMessage = getName() + " gained " + expString;
			TextMessage message(MESSAGE_STATUS_DEFAULT, spectatorMessage);
			for (Creature* spectator : spectators) {
				if (Player* player = spectator->getPlayer()) {
					player->sendTextMessage(message);
				}
			}
		}
	}

	uint32_t prevLevel = level;
	while (experience >= nextLevelExp) {
		++level;
		healthMax += vocation->getHPGain();
		health = std::min(health + vocation->getHPGain(), healthMax);
		manaMax += vocation->getManaGain();
		mana = std::min(mana + vocation->getManaGain(), manaMax);
		capacity += vocation->getCapGain();
		currLevelExp = nextLevelExp;
		nextLevelExp = Player::getExpForLevel(level + 1);
		if (currLevelExp >= nextLevelExp) {
			break;
		}
	}

	if (prevLevel != level) {
		updateBaseSpeed();
		setBaseSpeed(getBaseSpeed());
		g_game.changeSpeed(this, 0);
		g_game.addCreatureHealth(this);
		if (sendText && client) {
			sendTextMessage(MESSAGE_EVENT_ADVANCE, fmt::format("You advanced from Level {:d} to Level {:d}.", prevLevel, level));
		}
	}

	levelPercent = (nextLevelExp > currLevelExp) ? Player::getPercentLevel(experience - currLevelExp, nextLevelExp - currLevelExp) : 0;
	sendStats();
}

void Player::addSkillAdvance(skills_t skill, uint64_t count)
{
	if (count == 0 || !vocation || skill >= SKILL_LAST) {
		return;
	}

	auto& skillData = skills[skill];
	uint64_t currReqTries = vocation->getReqSkillTries(skill, skillData.level);
	uint64_t nextReqTries = vocation->getReqSkillTries(skill, skillData.level + 1);
	if (currReqTries >= nextReqTries) {
		return;
	}

	g_events->eventPlayerOnGainSkillTries(this, skill, count);
	if (count == 0) {
		return;
	}

	bool sendUpdate = false;
	skillData.tries += count;
	while (skillData.tries >= nextReqTries) {
		skillData.tries -= nextReqTries;
		skillData.level++;
		skillData.percent = 0;
		if (client) {
			sendTextMessage(MESSAGE_EVENT_ADVANCE, fmt::format("You advanced to {:s} level {:d}.", getSkillName(skill), skillData.level));
		}
		g_creatureEvents->playerAdvance(this, skill, skillData.level - 1, skillData.level);
		sendUpdate = true;
		currReqTries = nextReqTries;
		nextReqTries = vocation->getReqSkillTries(skill, skillData.level + 1);
		if (currReqTries >= nextReqTries) {
			break;
		}
	}

	skillData.percent = (nextReqTries > currReqTries) ? Player::getPercentLevel(skillData.tries, nextReqTries) : 0;
	if (sendUpdate || skillData.percent != skills[skill].percent) {
		sendSkills();
	}
}

void Player::doAttacking(uint32_t) noexcept
{
	if (lastAttack == 0) {
		lastAttack = OTSYS_TIME() - getAttackSpeed() - 1;
	}
	if (hasCondition(CONDITION_PACIFIED) || (OTSYS_TIME() - lastAttack) < getAttackSpeed()) {
		return;
	}

	Item* tool = getWeapon();
	const Weapon* weapon = tool ? g_weapons->getWeapon(tool) : nullptr;
	uint32_t delay = getAttackSpeed();
	bool classicSpeed = g_config.getBoolean(ConfigManager::CLASSIC_ATTACK_SPEED);

	bool result = weapon ? weapon->useWeapon(this, tool, attackedCreature) : Weapon::useFist(this, attackedCreature);
	if (result) {
		lastAttack = OTSYS_TIME();
	}

	SchedulerTask* task = createSchedulerTask(std::max<uint32_t>(SCHEDULER_MINTICKS, delay),
		std::bind(&Game::checkCreatureAttack, &g_game, getID()));
	if (!classicSpeed) {
		setNextActionTask(task, false);
	}
	else {
		g_scheduler.addEvent(task);
	}
}
void Player::updateInventoryWeight() noexcept
{
	if (hasFlag(PlayerFlag_HasInfiniteCapacity)) {
		inventoryWeight = 0;
		return;
	}

	inventoryWeight = 0;
	for (const auto& item : inventory) {
		if (item) {
			inventoryWeight += item->getWeight();
		}
	}
}

uint32_t Player::getItemTypeCount(uint16_t itemId, int32_t subType) const noexcept
{
	uint32_t count = 0;
	for (const auto& item : inventory) {
		if (item && item->getID() == itemId) {
			count += Item::countByType(item.get(), subType);
		}
		if (const Container* container = item ? item->getContainer() : nullptr) {
			for (ContainerIterator it = container->begin(), end = container->end(); it != end; ++it) {
				Item* containerItem = *it;
				if (containerItem && containerItem->getID() == itemId && (subType == -1 || subType == containerItem->getSubType())) {
					count += Item::countByType(containerItem, subType);
				}
			}
		}
	}
	return count;
}

bool Player::removeItemOfType(uint16_t itemId, uint32_t amount, int32_t subType, bool ignoreEquip) noexcept
{
	if (amount == 0) {
		return true;
	}

	std::vector<Item*> itemList;
	itemList.reserve(32);

	for (const auto& item : inventory) {
		if (!item || item->getID() != itemId) {
			continue;
		}

		if (!ignoreEquip && Item::items[item->getID()].isEquipment()) {
			continue;
		}

		if (subType == -1 || subType == item->getSubType()) {
			itemList.push_back(item.get());
		}

		if (const Container* container = item->getContainer()) {
			for (ContainerIterator it = container->begin(), end = container->end(); it != end; ++it) {
				Item* containerItem = *it;
				if (containerItem->getID() == itemId && (subType == -1 || subType == containerItem->getSubType())) {
					itemList.push_back(containerItem);
				}
			}
		}
	}

	uint32_t totalCount = 0;
	for (Item* item : itemList) {
		totalCount += Item::countByType(item, subType);
		if (totalCount >= amount) {
			break;
		}
	}

	if (totalCount < amount) {
		return false;
	}

	totalCount = 0;
	for (Item* item : itemList) {
		uint32_t count = Item::countByType(item, subType);
		totalCount += count;
		if (totalCount <= amount) {
			g_game.internalRemoveItem(item);
		}
		else {
			uint32_t diff = totalCount - amount;
			g_game.internalRemoveItem(item, count - diff);
			break;
		}
	}

	updateInventoryWeight();
	return true;
}

void Player::onAttackedCreature(Creature* target, bool isKilled)
{
	Creature::onAttackedCreature(target, isKilled);

	if (target == this || isPartner(target)) {
		return;
	}

	if (Player* targetPlayer = target->getPlayer()) {
		if (!isEnemy(targetPlayer)) {
			addUnjustifiedDead(targetPlayer);
		}

		pzLocked = true;
		if (!Combat::isInPvpZone(this, targetPlayer)) {
			addInFightTicks(pzLockedDuration, false);
		}
	}

	if (isKilled && target && !target->isAttackable()) {
		sendTextMessage(MESSAGE_STATUS_DEFAULT, "You may not attack this creature.");
	}
}

void Player::onCombatRemoveCondition(Condition* condition)
{
	Creature::onCombatRemoveCondition(condition);

	if (condition->getId() == CONDITION_PZLOCK) {
		pzLocked = false;
	}
	else if (condition->getId() == CONDITION_INFIGHT) {
		inFightTicks = 0;
	}

	if (client) {
		client->sendRemoveCondition(condition->getType());
	}
}

void Player::onAddCondition(Condition* condition)
{
	Creature::onAddCondition(condition);

	if (condition->getId() == CONDITION_PZLOCK) {
		pzLocked = true;
	}
	else if (condition->getId() == CONDITION_INFIGHT) {
		inFightTicks = condition->getTicks();
	}

	if (client) {
		client->sendAddCondition(condition);
	}
}
void Player::onThink(uint32_t interval) noexcept
{
	Creature::onThink(interval);

	if (client) {
		sendPing();
	}

	MessageBufferTicks += interval;
	if (MessageBufferTicks >= 1500) {
		MessageBufferTicks = 0;
		addMessageBuffer();
	}

	if (!getTile()->hasFlag(TILESTATE_NOLOGOUT) && !isAccessPlayer()) {
		idleTime += interval;
		if (idleTime > (kickAfterMinutes * 60000) + 60000) {
			kickPlayer(true);
		}
		else if (client && idleTime == kickAfterMinutes * 60000) {
			std::string warningMessage = fmt::format(
				"There was no variation in your behaviour for {:d} minutes. You will be disconnected in one minute if there is no change in your actions until then.",
				kickAfterMinutes
			);
			client->sendTextMessage(TextMessage(MESSAGE_STATUS_WARNING, warningMessage));
		}
	}

	if (stamina <= 0 && !isAccessPlayer() && client) {
		std::string staminaMessage = "You are exhausted and cannot gain experience until your stamina regenerates.";
		client->sendTextMessage(TextMessage(MESSAGE_STATUS_SMALL, staminaMessage));
	}
}

void Player::sendStats() noexcept
{
	if (client) {
		client->sendStats();
	}
}

void Player::sendSkills() noexcept
{
	if (client) {
		client->sendSkills();
	}
}

void Player::sendTextMessage(MessageClasses mclass, const std::string& message) const noexcept
{
	if (client) {
		client->sendTextMessage(TextMessage(mclass, message));
	}
}

bool Player::isPartner(const Creature* creature) const noexcept
{
	if (!creature || creature == this) {
		return false;
	}

	const Player* targetPlayer = creature->getPlayer();
	return targetPlayer && (party == targetPlayer->party || (guild && guild == targetPlayer->guild));
}

bool Player::isEnemy(const Player* player) const noexcept
{
	if (!player || player == this || !guild || !player->guild) {
		return false;
	}

	const GuildWar* war = g_game.getGuildWar(guild->getId(), player->guild->getId());
	return war && war->isActive();
}

void Player::setGuild(Guild* guild)
{
	if (guild == this->guild) {
		return;
	}

	Guild* oldGuild = this->guild;
	this->guildNick.clear();
	this->guild = nullptr;
	this->guildRank = nullptr;

	if (guild) {
		if (GuildRank_ptr rank = guild->getRankByLevel(1)) {
			this->guild = guild;
			this->guildRank = std::move(rank);
			guild->addMember(this);
		}
	}

	if (oldGuild) {
		oldGuild->removeMember(this);
	}

	if (client) {
		client->sendGuildInfo();
	}
}

void Player::setParty(Party* newParty)
{
	if (party == newParty) {
		return;
	}

	Party* oldParty = party;
	party = newParty;

	if (newParty) {
		newParty->addMember(this);
	}

	if (oldParty) {
		oldParty->removeMember(this);
	}

	if (client) {
		client->sendPartyInfo();
	}
}
void Player::kickPlayer(bool displayEffect) noexcept
{
	if (client) {
		client->logout(displayEffect, false);
	}
}

void Player::addUnjustifiedDead(const Player* attacked) noexcept
{
	if (!attacked || attacked == this || hasFlag(PlayerFlag_CannotKillPlayers)) {
		return;
	}

	{
		std::lock_guard<std::mutex> lock(muteCountMutex);
		unjustKills++;
	}

	if (client) {
		client->sendUnjustifiedStats();
	}

	if (unjustKills >= unjustKillThreshold) {
		addCondition(Condition::createCondition(CONDITIONID_DEFAULT, CONDITION_PZLOCK, pzLockedDuration));
	}
}

bool Player::hasFlag(uint64_t flag) const noexcept
{
	return group && (group->flags & flag) != 0;
}

bool Player::isAccessPlayer() const noexcept
{
	return group && group->access;
}

uint32_t Player::getAttackSpeed() const noexcept
{
	Item* weapon = getWeapon();
	if (!weapon) {
		return vocation ? vocation->getAttackSpeed() : 1000;
	}

	const Weapon* w = g_weapons->getWeapon(weapon);
	return w ? w->getSpeed() : vocation ? vocation->getAttackSpeed() : 1000;
}

void Player::updateBaseSpeed() noexcept
{
	if (!vocation) {
		baseSpeed = 220;
		return;
	}

	baseSpeed = vocation->getBaseSpeed() + (2 * (level - 1));
}

uint32_t Player::getBaseSpeed() const noexcept
{
	return baseSpeed + (hasCondition(CONDITION_HASTE) ? 100 : 0) - (hasCondition(CONDITION_PARALYZE) ? 50 : 0);
}
