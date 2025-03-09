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
#include "tile.h"
#include "creature.h"
#include "combat.h"
#include "game.h"
#include "mailbox.h"
#include "monster.h"
#include "movement.h"
#include "teleport.h"
#include "trashholder.h"
#include "configmanager.h"

extern Game g_game;
extern MoveEvents* g_moveEvents;
extern ConfigManager g_config;

StaticTile real_nullptr_tile(0xFFFF, 0xFFFF, 0xFF);
Tile& Tile::nullptr_tile = real_nullptr_tile;

class Tile {
public:
	Tile(uint16_t x, uint16_t y, uint8_t z) : tilePos(x, y, z) {
		items.reserve(8);   // Pre-allocate for typical item count
		creatures.reserve(4); // Pre-allocate for typical creature count
	}

	// Inline accessors
	inline size_t getCreatureCount() const { return creatures.size(); }
	inline size_t getItemCount() const { return items.size(); }
	inline uint32_t getTopItemCount() const { return topItemCount; }
	inline uint32_t getDownItemCount() const { return items.size() - topItemCount; }

	bool hasProperty(ITEMPROPERTY prop) const;
	bool hasProperty(const Item* exclude, ITEMPROPERTY prop) const;
	bool hasHeight(uint32_t n) const;

	std::string getDescription(int32_t lookDistance) const;

	Teleport* getTeleportItem() const;
	MagicField* getFieldItem() const;
	TrashHolder* getTrashHolder() const;
	Mailbox* getMailbox() const;
	BedItem* getBedItem() const;

	Creature* getTopCreature() const;
	const Creature* getBottomCreature() const;
	Creature* getTopVisibleCreature(const Creature* creature) const;
	const Creature* getBottomVisibleCreature(const Creature* creature) const;

	Item* getTopDownItem() const;
	Item* getTopTopItem() const;
	Item* getItemByTopOrder(int32_t topOrder);
	Thing* getTopVisibleThing(const Creature* creature);

	void onAddTileItem(Item* item);
	void onUpdateTileItem(Item* oldItem, const ItemType& oldType, Item* newItem, const ItemType& newType);
	void onRemoveTileItem(Item* item);
	void onUpdateTile();

	ReturnValue queryAdd(int32_t index, const Thing& thing, uint32_t count, uint32_t flags, Creature* actor = nullptr) const;
	ReturnValue queryMaxCount(int32_t index, const Thing& thing, uint32_t count, uint32_t& maxQueryCount, uint32_t flags) const;
	ReturnValue queryRemove(const Thing& thing, uint32_t count, uint32_t flags, Creature* actor = nullptr) const;
	Tile* queryDestination(int32_t& index, const Thing& thing, Item** destItem, uint32_t& flags);

	void addThing(int32_t index, Thing* thing);
	void updateThing(Thing* thing, uint16_t itemId, uint32_t count);
	void replaceThing(uint32_t index, Thing* thing);
	void removeThing(Thing* thing, uint32_t count);
	void removeCreature(Creature* creature);

	int32_t getThingIndex(const Thing* thing) const;
	int32_t getClientIndexOfCreature(const Player* player, const Creature* creature) const;
	int32_t getStackposOfItem(const Player* player, const Item* item) const;

	inline size_t getFirstIndex() const { return 0; }
	inline size_t getLastIndex() const { return getThingCount(); }
	uint32_t getItemTypeCount(uint16_t itemId, int32_t subType = -1) const;
	Thing* getThing(size_t index) const;

	void postAddNotification(Thing* thing, const Cylinder* oldParent, int32_t index, cylinderlink_t link = LINK_OWNER);
	void postRemoveNotification(Thing* thing, const Cylinder* newParent, int32_t index, cylinderlink_t link = LINK_OWNER);

	void internalAddThing(uint32_t index, Thing* thing);

	inline bool hasFlag(uint32_t flag) const { return (tileFlags & flag) != 0; }
	inline void setFlag(uint32_t flag) { tileFlags |= flag; }
	inline void resetFlag(uint32_t flag) { tileFlags &= ~flag; }

	bool isMoveableBlocking() const;
	Item* getUseItem(int32_t index) const;

private:
	void setTileFlags(const Item* item);
	void resetTileFlags(const Item* item);

	Position tilePos;
	Item* ground = nullptr;
	std::vector<Item*> items;       // Direct vector instead of pointer
	uint32_t topItemCount = 0;      // Manual tracking of top items
	std::vector<Creature*> creatures; // Direct vector instead of pointer
	uint32_t tileFlags = 0;         // Cached flags as bitset
};
bool Tile::hasProperty(ITEMPROPERTY prop) const {
	if (ground && ground->hasProperty(prop)) {
		return true;
	}
	for (const Item* item : items) {
		if (item->hasProperty(prop)) {
			return true;
		}
	}
	return false;
}

bool Tile::hasProperty(const Item* exclude, ITEMPROPERTY prop) const {
	if (ground && exclude != ground && ground->hasProperty(prop)) {
		return true;
	}
	for (const Item* item : items) {
		if (item != exclude && item->hasProperty(prop)) {
			return true;
		}
	}
	return false;
}

bool Tile::hasHeight(uint32_t n) const {
	uint32_t height = 0;
	if (ground && ground->hasProperty(CONST_PROP_HASHEIGHT)) {
		if (++height == n) return true;
	}
	for (const Item* item : items) {
		if (item->hasProperty(CONST_PROP_HASHEIGHT)) {
			if (++height == n) return true;
		}
	}
	return false;
}

std::string Tile::getDescription(int32_t) const {
	return "You dont know why, but you cant see anything!";
}

Teleport* Tile::getTeleportItem() const {
	if (!hasFlag(TILESTATE_TELEPORT)) return nullptr;
	for (auto it = items.rbegin(), end = items.rend(); it != end; ++it) {
		if (Teleport* teleport = (*it)->getTeleport()) {
			return teleport;
		}
	}
	return nullptr;
}

MagicField* Tile::getFieldItem() const {
	if (!hasFlag(TILESTATE_MAGICFIELD)) return nullptr;
	if (ground && ground->getMagicField()) return ground->getMagicField();
	for (auto it = items.rbegin(), end = items.rend(); it != end; ++it) {
		if (MagicField* field = (*it)->getMagicField()) {
			return field;
		}
	}
	return nullptr;
}

TrashHolder* Tile::getTrashHolder() const {
	if (!hasFlag(TILESTATE_TRASHHOLDER)) return nullptr;
	if (ground && ground->getTrashHolder()) return ground->getTrashHolder();
	for (auto it = items.rbegin(), end = items.rend(); it != end; ++it) {
		if (TrashHolder* holder = (*it)->getTrashHolder()) {
			return holder;
		}
	}
	return nullptr;
}

Mailbox* Tile::getMailbox() const {
	if (!hasFlag(TILESTATE_MAILBOX)) return nullptr;
	if (ground && ground->getMailbox()) return ground->getMailbox();
	for (auto it = items.rbegin(), end = items.rend(); it != end; ++it) {
		if (Mailbox* mailbox = (*it)->getMailbox()) {
			return mailbox;
		}
	}
	return nullptr;
}

BedItem* Tile::getBedItem() const {
	if (!hasFlag(TILESTATE_BED)) return nullptr;
	if (ground && ground->getBed()) return ground->getBed();
	for (auto it = items.rbegin(), end = items.rend(); it != end; ++it) {
		if (BedItem* bed = (*it)->getBed()) {
			return bed;
		}
	}
	return nullptr;
}

Creature* Tile::getTopCreature() const {
	return creatures.empty() ? nullptr : creatures.front();
}

const Creature* Tile::getBottomCreature() const {
	return creatures.empty() ? nullptr : creatures.back();
}

Creature* Tile::getTopVisibleCreature(const Creature* creature) const {
	for (Creature* tileCreature : creatures) {
		if (creature ? creature->canSeeCreature(tileCreature) :
			(!tileCreature->isInvisible() && (!tileCreature->getPlayer() || !tileCreature->getPlayer()->isInGhostMode()))) {
			return tileCreature;
		}
	}
	return nullptr;
}

const Creature* Tile::getBottomVisibleCreature(const Creature* creature) const {
	for (auto it = creatures.rbegin(), end = creatures.rend(); it != end; ++it) {
		if (creature ? creature->canSeeCreature(*it) :
			(!(*it)->isInvisible() && (!(*it)->getPlayer() || !(*it)->getPlayer()->isInGhostMode()))) {
			return *it;
		}
	}
	return nullptr;
}

Item* Tile::getTopDownItem() const {
	return (topItemCount < items.size()) ? items[topItemCount] : nullptr;
}

Item* Tile::getTopTopItem() const {
	return topItemCount > 0 ? items[topItemCount - 1] : nullptr;
}

Item* Tile::getItemByTopOrder(int32_t topOrder) {
	for (auto it = items.rbegin(), end = items.rend() - topItemCount; it != end; ++it) {
		if (Item::items[(*it)->getID()].alwaysOnTopOrder == topOrder) {
			return *it;
		}
	}
	return nullptr;
}

Thing* Tile::getTopVisibleThing(const Creature* creature) {
	if (Creature* topCreature = getTopVisibleCreature(creature)) {
		return topCreature;
	}
	for (size_t i = topItemCount; i < items.size(); ++i) {
		if (!Item::items[items[i]->getID()].lookThrough) {
			return items[i];
		}
	}
	for (auto it = items.rbegin(), end = items.rend() - topItemCount; it != end; ++it) {
		if (!Item::items[(*it)->getID()].lookThrough) {
			return *it;
		}
	}
	return ground;
}
void Tile::onAddTileItem(Item* item) {
	setTileFlags(item);
	const Position& pos = tilePos;

	SpectatorVec spectators;
	g_game.map.getSpectators(spectators, pos, true);
	for (Creature* spectator : spectators) {
		if (Player* player = spectator->getPlayer()) {
			player->sendAddTileItem(this, pos, item);
		}
	}
	for (Creature* spectator : spectators) {
		spectator->onAddTileItem(this, pos);
	}

	if ((!hasFlag(TILESTATE_PROTECTIONZONE) || g_config.getBoolean(ConfigManager::CLEAN_PROTECTION_ZONES)) && item->isCleanable()) {
		g_game.addTileToClean(this);
	}
}

void Tile::onUpdateTileItem(Item* oldItem, const ItemType& oldType, Item* newItem, const ItemType& newType) {
	const Position& pos = tilePos;
	SpectatorVec spectators;
	g_game.map.getSpectators(spectators, pos, true);
	for (Creature* spectator : spectators) {
		if (Player* player = spectator->getPlayer()) {
			player->sendUpdateTileItem(this, pos, newItem);
		}
		spectator->onUpdateTileItem(this, pos, oldItem, oldType, newItem, newType);
	}
}

void Tile::onRemoveTileItem(Item* item) {
	resetTileFlags(item);
	const Position& pos = tilePos;
	const ItemType& iType = Item::items[item->getID()];

	SpectatorVec spectators;
	g_game.map.getSpectators(spectators, pos, true);
	std::vector<int32_t> oldStackPosVector;
	oldStackPosVector.reserve(spectators.size());
	for (Creature* spectator : spectators) {
		if (Player* player = spectator->getPlayer()) {
			oldStackPosVector.push_back(getStackposOfItem(player, item));
		}
	}

	size_t i = 0;
	for (Creature* spectator : spectators) {
		if (Player* player = spectator->getPlayer()) {
			player->sendRemoveTileThing(pos, oldStackPosVector[i++]);
		}
	}
	for (Creature* spectator : spectators) {
		spectator->onRemoveTileItem(this, pos, iType, item);
	}

	if (!hasFlag(TILESTATE_PROTECTIONZONE) || g_config.getBoolean(ConfigManager::CLEAN_PROTECTION_ZONES)) {
		if (items.empty()) {
			g_game.removeTileToClean(this);
		}
		else {
			bool hasCleanable = false;
			for (const Item* toCheck : items) {
				if (toCheck->isCleanable()) {
					hasCleanable = true;
					break;
				}
			}
			if (!hasCleanable) {
				g_game.removeTileToClean(this);
			}
		}
	}
}

void Tile::onUpdateTile() {
	const Position& pos = tilePos;
	SpectatorVec spectators;
	g_game.map.getSpectators(spectators, pos, true);
	for (Creature* spectator : spectators) {
		if (Player* player = spectator->getPlayer()) {
			player->sendUpdateTile(this, pos);
		}
	}
}
ReturnValue Tile::queryAdd(int32_t, const Thing& thing, uint32_t, uint32_t flags, Creature*) const {
	if (const Creature* creature = thing.getCreature()) {
		if (hasBitSet(FLAG_NOLIMIT, flags)) return RETURNVALUE_NOERROR;
		if (hasBitSet(FLAG_PATHFINDING, flags) && hasFlag(TILESTATE_FLOORCHANGE | TILESTATE_TELEPORT)) {
			return RETURNVALUE_NOTPOSSIBLE;
		}
		if (!ground) return RETURNVALUE_NOTPOSSIBLE;

		if (const Monster* monster = creature->getMonster()) {
			if (hasFlag(TILESTATE_PROTECTIONZONE | TILESTATE_FLOORCHANGE | TILESTATE_TELEPORT)) {
				return RETURNVALUE_NOTPOSSIBLE;
			}
			if (monster->canPushCreatures() && !monster->isSummon()) {
				for (const Creature* tileCreature : creatures) {
					if (tileCreature->getPlayer() && tileCreature->getPlayer()->isInGhostMode()) continue;
					const Monster* creatureMonster = tileCreature->getMonster();
					if (!creatureMonster || !tileCreature->isPushable() ||
						(creatureMonster->isSummon() && creatureMonster->getMaster()->getPlayer())) {
						return RETURNVALUE_NOTPOSSIBLE;
					}
				}
			}
			else if (!creatures.empty()) {
				for (const Creature* tileCreature : creatures) {
					if (!tileCreature->isInGhostMode()) return RETURNVALUE_NOTENOUGHROOM;
				}
			}

			if (hasFlag(TILESTATE_IMMOVABLEBLOCKSOLID)) return RETURNVALUE_NOTPOSSIBLE;
			if (hasBitSet(FLAG_PATHFINDING, flags) && hasFlag(TILESTATE_IMMOVABLENOFIELDBLOCKPATH)) {
				return RETURNVALUE_NOTPOSSIBLE;
			}
			if (hasFlag(TILESTATE_BLOCKSOLID) || (hasBitSet(FLAG_PATHFINDING, flags) && hasFlag(TILESTATE_NOFIELDBLOCKPATH))) {
				if (!(monster->canPushItems() || hasBitSet(FLAG_IGNOREBLOCKITEM, flags))) {
					return RETURNVALUE_NOTPOSSIBLE;
				}
			}

			if (MagicField* field = getFieldItem()) {
				if (field->isBlocking() || field->getDamage() == 0) return RETURNVALUE_NOERROR;
				CombatType_t combatType = field->getCombatType();
				if (!monster->isImmune(combatType)) {
					if (hasBitSet(FLAG_IGNOREFIELDDAMAGE, flags)) {
						if (!(monster->canWalkOnFieldType(combatType) || monster->isIgnoringFieldDamage())) {
							return RETURNVALUE_NOTPOSSIBLE;
						}
					}
					else {
						return RETURNVALUE_NOTPOSSIBLE;
					}
				}
			}
			return RETURNVALUE_NOERROR;
		}

		if (const Player* player = creature->getPlayer()) {
			if (!creatures.empty() && !hasBitSet(FLAG_IGNOREBLOCKCREATURE, flags) && !player->isAccessPlayer()) {
				for (const Creature* tileCreature : creatures) {
					if (!player->canWalkthrough(tileCreature)) return RETURNVALUE_NOTPOSSIBLE;
				}
			}
			if (!player->getParent() && hasFlag(TILESTATE_NOLOGOUT)) {
				return RETURNVALUE_NOTPOSSIBLE;
			}
			const Tile* playerTile = player->getTile();
			if (playerTile && player->isPzLocked()) {
				if (!playerTile->hasFlag(TILESTATE_PVPZONE) && hasFlag(TILESTATE_PVPZONE)) {
					return RETURNVALUE_PLAYERISPZLOCKEDENTERPVPZONE;
				}
				if (playerTile->hasFlag(TILESTATE_PVPZONE) && !hasFlag(TILESTATE_PVPZONE)) {
					return RETURNVALUE_PLAYERISPZLOCKEDLEAVEPVPZONE;
				}
				if ((!playerTile->hasFlag(TILESTATE_NOPVPZONE) && hasFlag(TILESTATE_NOPVPZONE)) ||
					(!playerTile->hasFlag(TILESTATE_PROTECTIONZONE) && hasFlag(TILESTATE_PROTECTIONZONE))) {
					return RETURNVALUE_PLAYERISPZLOCKED;
				}
			}
		}
		else if (!creatures.empty() && !hasBitSet(FLAG_IGNOREBLOCKCREATURE, flags)) {
			for (const Creature* tileCreature : creatures) {
				if (!tileCreature->isInGhostMode()) return RETURNVALUE_NOTENOUGHROOM;
			}
		}

		if (!hasBitSet(FLAG_IGNOREBLOCKITEM, flags) && hasFlag(TILESTATE_BLOCKSOLID)) {
			return RETURNVALUE_NOTENOUGHROOM;
		}
		if (hasBitSet(FLAG_IGNOREBLOCKITEM, flags)) {
			if (ground) {
				const ItemType& iiType = Item::items[ground->getID()];
				if (iiType.blockSolid && (!iiType.moveable || ground->hasAttribute(ITEM_ATTRIBUTE_UNIQUEID))) {
					return RETURNVALUE_NOTPOSSIBLE;
				}
			}
			for (const Item* item : items) {
				const ItemType& iiType = Item::items[item->getID()];
				if (iiType.blockSolid && (!iiType.moveable || item->hasAttribute(ITEM_ATTRIBUTE_UNIQUEID))) {
					return RETURNVALUE_NOTPOSSIBLE;
				}
			}
		}
	}
	else if (const Item* item = thing.getItem()) {
		if (items.size() >= 0xFFFF) return RETURNVALUE_NOTPOSSIBLE;
		if (hasBitSet(FLAG_NOLIMIT, flags)) return RETURNVALUE_NOERROR;

		bool itemIsHangable = item->isHangable();
		if (!ground && !itemIsHangable) return RETURNVALUE_NOTPOSSIBLE;

		if (!creatures.empty() && item->isBlocking() && !hasBitSet(FLAG_IGNOREBLOCKCREATURE, flags)) {
			for (const Creature* tileCreature : creatures) {
				if (!tileCreature->isInGhostMode()) return RETURNVALUE_NOTENOUGHROOM;
			}
		}

		if (itemIsHangable && hasFlag(TILESTATE_SUPPORTS_HANGABLE)) {
			for (const Item* tileItem : items) {
				if (tileItem->isHangable()) return RETURNVALUE_NEEDEXCHANGE;
			}
		}
		else {
			if (ground) {
				const ItemType& iiType = Item::items[ground->getID()];
				if (iiType.blockSolid && (!iiType.ignoreBlocking || item->isMagicField() || item->isBlocking()) &&
					(!item->isPickupable() || (!iiType.hasHeight || iiType.pickupable || iiType.isBed()))) {
					return RETURNVALUE_NOTENOUGHROOM;
				}
			}
			for (const Item* tileItem : items) {
				const ItemType& iiType = Item::items[tileItem->getID()];
				if (iiType.blockSolid && (!iiType.ignoreBlocking || item->isMagicField() || item->isBlocking()) &&
					(!item->isPickupable() || (!iiType.hasHeight || iiType.pickupable || iiType.isBed()))) {
					return RETURNVALUE_NOTENOUGHROOM;
				}
			}
		}
	}
	return RETURNVALUE_NOERROR;
}

ReturnValue Tile::queryMaxCount(int32_t, const Thing&, uint32_t count, uint32_t& maxQueryCount, uint32_t) const {
	maxQueryCount = std::max<uint32_t>(1, count);
	return RETURNVALUE_NOERROR;
}

ReturnValue Tile::queryRemove(const Thing& thing, uint32_t count, uint32_t flags, Creature*) const {
	int32_t index = getThingIndex(&thing);
	if (index == -1) return RETURNVALUE_NOTPOSSIBLE;

	const Item* item = thing.getItem();
	if (!item || (count == 0 || (item->isStackable() && count > item->getItemCount()))) {
		return RETURNVALUE_NOTPOSSIBLE;
	}
	if (!item->isMoveable() && !hasBitSet(FLAG_IGNORENOTMOVEABLE, flags)) {
		return RETURNVALUE_NOTMOVEABLE;
	}
	return RETURNVALUE_NOERROR;
}

Tile* Tile::queryDestination(int32_t&, const Thing&, Item** destItem, uint32_t& flags) {
	Tile* destTile = nullptr;
	*destItem = nullptr;

	if (hasFlag(TILESTATE_FLOORCHANGE_DOWN)) {
		uint16_t dx = tilePos.x, dy = tilePos.y;
		uint8_t dz = tilePos.z + 1;
		Tile* southDownTile = g_game.map.getTile(dx, dy - 1, dz);
		if (southDownTile && southDownTile->hasFlag(TILESTATE_FLOORCHANGE_SOUTH_ALT)) {
			dy -= 2;
		}
		else if (Tile* eastDownTile = g_game.map.getTile(dx - 1, dy, dz); eastDownTile && eastDownTile->hasFlag(TILESTATE_FLOORCHANGE_EAST_ALT)) {
			dx -= 2;
		}
		else if (Tile* downTile = g_game.map.getTile(dx, dy, dz)) {
			if (downTile->hasFlag(TILESTATE_FLOORCHANGE_NORTH)) ++dy;
			if (downTile->hasFlag(TILESTATE_FLOORCHANGE_SOUTH)) --dy;
			if (downTile->hasFlag(TILESTATE_FLOORCHANGE_SOUTH_ALT)) dy -= 2;
			if (downTile->hasFlag(TILESTATE_FLOORCHANGE_EAST)) --dx;
			if (downTile->hasFlag(TILESTATE_FLOORCHANGE_EAST_ALT)) dx -= 2;
			if (downTile->hasFlag(TILESTATE_FLOORCHANGE_WEST)) ++dx;
		}
		destTile = g_game.map.getTile(dx, dy, dz);
	}
	else if (hasFlag(TILESTATE_FLOORCHANGE)) {
		uint16_t dx = tilePos.x, dy = tilePos.y;
		uint8_t dz = tilePos.z - 1;
		if (hasFlag(TILESTATE_FLOORCHANGE_NORTH)) --dy;
		if (hasFlag(TILESTATE_FLOORCHANGE_SOUTH)) ++dy;
		if (hasFlag(TILESTATE_FLOORCHANGE_EAST)) ++dx;
		if (hasFlag(TILESTATE_FLOORCHANGE_WEST)) --dx;
		if (hasFlag(TILESTATE_FLOORCHANGE_SOUTH_ALT)) dy += 2;
		if (hasFlag(TILESTATE_FLOORCHANGE_EAST_ALT)) dx += 2;
		destTile = g_game.map.getTile(dx, dy, dz);
	}

	if (!destTile) destTile = this;
	else flags |= FLAG_NOLIMIT;

	if (destTile) {
		*destItem = destTile->getTopDownItem();
	}
	return destTile;
}
void Tile::addThing(int32_t, Thing* thing) {
	Creature* creature = thing->getCreature();
	if (creature) {
		g_game.map.clearSpectatorCache();
		if (creature->getPlayer()) g_game.map.clearPlayersSpectatorCache();
		creature->setParent(this);
		creatures.insert(creatures.begin(), creature);
		return;
	}

	Item* item = thing->getItem();
	if (!item || items.size() >= 0xFFFF) return;

	item->setParent(this);
	const ItemType& itemType = Item::items[item->getID()];
	if (itemType.isGroundTile()) {
		if (!ground) {
			ground = item;
			onAddTileItem(item);
		}
		else {
			const ItemType& oldType = Item::items[ground->getID()];
			Item* oldGround = ground;
			ground->setParent(nullptr);
			g_game.ReleaseItem(ground);
			ground = item;
			resetTileFlags(oldGround);
			setTileFlags(item);
			onUpdateTileItem(oldGround, oldType, item, itemType);
			postRemoveNotification(oldGround, nullptr, 0);
		}
	}
	else if (itemType.alwaysOnTop) {
		if (itemType.isSplash()) {
			for (auto it = items.begin(); it != items.end(); ) {
				if (Item::items[(*it)->getID()].isSplash()) {
					Item* oldSplash = *it;
					it = items.erase(it);
					if (oldSplash < items.data() + topItemCount) --topItemCount;
					oldSplash->setParent(nullptr);
					g_game.ReleaseItem(oldSplash);
					postRemoveNotification(oldSplash, nullptr, 0);
				}
				else {
					++it;
				}
			}
		}
		auto it = items.begin();
		for (; it != items.begin() + topItemCount && Item::items[(*it)->getID()].alwaysOnTopOrder < itemType.alwaysOnTopOrder; ++it) {}
		items.insert(it, item);
		if (it == items.begin() + topItemCount) ++topItemCount;
		onAddTileItem(item);
	}
	else {
		items.insert(items.begin() + topItemCount, item);
		onAddTileItem(item);
	}
}

void Tile::updateThing(Thing* thing, uint16_t itemId, uint32_t count) {
	int32_t index = getThingIndex(thing);
	if (index == -1) return;

	Item* item = thing->getItem();
	if (!item) return;

	const ItemType& oldType = Item::items[item->getID()];
	resetTileFlags(item);
	item->setID(itemId);
	item->setSubType(count);
	setTileFlags(item);
	onUpdateTileItem(item, oldType, item, Item::items[itemId]);
}

void Tile::replaceThing(uint32_t index, Thing* thing) {
	Item* item = thing->getItem();
	if (!item) return;

	size_t pos = index;
	Item* oldItem = nullptr;
	bool isInserted = false;

	if (pos == 0 && ground) {
		oldItem = ground;
		ground = item;
		isInserted = true;
	}
	else if (pos > 0) {
		--pos;
		if (pos < topItemCount) {
			oldItem = items[pos];
			items[pos] = item;
			isInserted = true;
		}
		else {
			pos -= topItemCount;
			if (pos < creatures.size()) return;
			pos -= creatures.size();
			if (pos < items.size() - topItemCount) {
				oldItem = items[topItemCount + pos];
				items[topItemCount + pos] = item;
				isInserted = true;
			}
		}
	}

	if (isInserted) {
		item->setParent(this);
		resetTileFlags(oldItem);
		setTileFlags(item);
		onUpdateTileItem(oldItem, Item::items[oldItem->getID()], item, Item::items[item->getID()]);
		oldItem->setParent(nullptr);
	}
}

void Tile::removeThing(Thing* thing, uint32_t count) {
	Creature* creature = thing->getCreature();
	if (creature) {
		auto it = std::find(creatures.begin(), creatures.end(), creature);
		if (it != creatures.end()) {
			g_game.map.clearSpectatorCache();
			if (creature->getPlayer()) g_game.map.clearPlayersSpectatorCache();
			creatures.erase(it);
		}
		return;
	}

	Item* item = thing->getItem();
	if (!item) return;

	if (item == ground) {
		ground->setParent(nullptr);
		ground = nullptr;
		onRemoveTileItem(item);
		return;
	}

	auto it = std::find(items.begin(), items.end(), item);
	if (it == items.end()) return;

	const ItemType& itemType = Item::items[item->getID()];
	if (itemType.alwaysOnTop && it < items.begin() + topItemCount) --topItemCount;

	if (itemType.stackable && count != item->getItemCount()) {
		uint8_t newCount = static_cast<uint8_t>(std::max<int32_t>(0, item->getItemCount() - count));
		item->setItemCount(newCount);
		onUpdateTileItem(item, itemType, item, itemType);
	}
	else {
		items.erase(it);
		item->setParent(nullptr);
		onRemoveTileItem(item);
	}
}

void Tile::removeCreature(Creature* creature) {
	g_game.map.getQTNode(tilePos.x, tilePos.y)->removeCreature(creature);
	removeThing(creature, 0);
}
int32_t Tile::getThingIndex(const Thing* thing) const {
	int32_t n = -1;
	if (ground == thing) return 0;
	++n;

	const Item* item = thing->getItem();
	if (item && item->isAlwaysOnTop()) {
		for (size_t i = 0; i < topItemCount; ++i) {
			++n;
			if (items[i] == item) return n;
		}
	}
	else {
		n += topItemCount;
	}

	if (thing->getCreature()) {
		for (const Creature* creature : creatures) {
			++n;
			if (creature == thing) return n;
		}
	}
	else {
		n += creatures.size();
	}

	if (item && !item->isAlwaysOnTop()) {
		for (size_t i = topItemCount; i < items.size(); ++i) {
			++n;
			if (items[i] == item) return n;
		}
	}
	return -1;
}

int32_t Tile::getClientIndexOfCreature(const Player* player, const Creature* creature) const {
	int32_t n = ground ? 1 : 0;
	n += topItemCount;
	for (auto it = creatures.rbegin(), end = creatures.rend(); it != end; ++it) {
		if (*it == creature) return n;
		if (player->canSeeCreature(*it)) ++n;
	}
	return -1;
}

int32_t Tile::getStackposOfItem(const Player* player, const Item* item) const {
	int32_t n = ground == item ? 0 : 1;
	if (item->isAlwaysOnTop()) {
		for (size_t i = 0; i < topItemCount && n < 10; ++i) {
			if (items[i] == item) return n;
			++n;
		}
	}
	else {
		n += topItemCount;
		if (n < 10) {
			for (const Creature* creature : creatures) {
				if (player->canSeeCreature(creature) && ++n >= 10) return -1;
			}
			for (size_t i = topItemCount; i < items.size() && n < 10; ++i) {
				if (items[i] == item) return n;
				++n;
			}
		}
	}
	return -1;
}

uint32_t Tile::getItemTypeCount(uint16_t itemId, int32_t subType) const {
	uint32_t count = (ground && ground->getID() == itemId) ? Item::countByType(ground, subType) : 0;
	for (const Item* item : items) {
		if (item->getID() == itemId) count += Item::countByType(item, subType);
	}
	return count;
}

Thing* Tile::getThing(size_t index) const {
	if (index == 0 && ground) return ground;
	if (index > 0) --index;

	if (index < topItemCount) return items[index];
	index -= topItemCount;

	if (index < creatures.size()) return creatures[index];
	index -= creatures.size();

	if (index < items.size() - topItemCount) return items[topItemCount + index];
	return nullptr;
}

void Tile::postAddNotification(Thing* thing, const Cylinder* oldParent, int32_t index, cylinderlink_t link) {
	SpectatorVec spectators;
	g_game.map.getSpectators(spectators, tilePos, true, true);
	for (Creature* spectator : spectators) {
		if (Player* player = spectator->getPlayer()) {
			player->postAddNotification(thing, oldParent, index, LINK_NEAR);
		}
	}

	Creature* creature = thing->getCreature();
	Item* item = creature ? nullptr : thing->getItem();
	if (creature) creature->incrementReferenceCounter();
	else if (item) item->incrementReferenceCounter();

	if (link == LINK_OWNER) {
		if (hasFlag(TILESTATE_TELEPORT)) {
			if (Teleport* teleport = getTeleportItem()) teleport->addThing(thing);
		}
		else if (hasFlag(TILESTATE_TRASHHOLDER)) {
			if (TrashHolder* trashholder = getTrashHolder()) trashholder->addThing(thing);
		}
		else if (hasFlag(TILESTATE_MAILBOX)) {
			if (Mailbox* mailbox = getMailbox()) mailbox->addThing(thing);
		}
		if (creature) g_moveEvents->onCreatureMove(creature, this, MOVE_EVENT_STEP_IN);
		else if (item) g_moveEvents->onItemMove(item, this, true);
	}

	if (creature) g_game.ReleaseCreature(creature);
	else if (item) g_game.ReleaseItem(item);
}

void Tile::postRemoveNotification(Thing* thing, const Cylinder* newParent, int32_t index, cylinderlink_t) {
	SpectatorVec spectators;
	g_game.map.getSpectators(spectators, tilePos, true, true);
	if (getThingCount() > 8) onUpdateTile();
	for (Creature* spectator : spectators) {
		if (Player* player = spectator->getPlayer()) {
			player->postRemoveNotification(thing, newParent, index, LINK_NEAR);
		}
	}

	Creature* creature = thing->getCreature();
	if (creature) g_moveEvents->onCreatureMove(creature, this, MOVE_EVENT_STEP_OUT);
	else if (Item* item = thing->getItem()) g_moveEvents->onItemMove(item, this, false);
}

void Tile::internalAddThing(uint32_t, Thing* thing) {
	thing->setParent(this);
	Creature* creature = thing->getCreature();
	if (creature) {
		g_game.map.clearSpectatorCache();
		if (creature->getPlayer()) g_game.map.clearPlayersSpectatorCache();
		creatures.insert(creatures.begin(), creature);
	}
	else {
		Item* item = thing->getItem();
		if (!item || items.size() >= 0xFFFF) return;

		const ItemType& itemType = Item::items[item->getID()];
		if (itemType.isGroundTile()) {
			if (!ground) {
				ground = item;
				setTileFlags(item);
			}
		}
		else if (itemType.alwaysOnTop) {
			auto it = items.begin();
			for (; it != items.begin() + topItemCount && Item::items[(*it)->getID()].alwaysOnTopOrder < itemType.alwaysOnTopOrder; ++it) {}
			items.insert(it, item);
			if (it == items.begin() + topItemCount) ++topItemCount;
			setTileFlags(item);
		}
		else {
			items.insert(items.begin() + topItemCount, item);
			setTileFlags(item);
		}
	}
}
void Tile::setTileFlags(const Item* item) {
	const ItemType& it = Item::items[item->getID()];
	if (it.floorChange != 0) tileFlags |= it.floorChange;
	if (it.hasProperty(CONST_PROP_IMMOVABLEBLOCKSOLID)) tileFlags |= TILESTATE_IMMOVABLEBLOCKSOLID;
	if (it.hasProperty(CONST_PROP_BLOCKPATH)) tileFlags |= TILESTATE_BLOCKPATH;
	if (it.hasProperty(CONST_PROP_NOFIELDBLOCKPATH)) tileFlags |= TILESTATE_NOFIELDBLOCKPATH;
	if (it.hasProperty(CONST_PROP_IMMOVABLENOFIELDBLOCKPATH)) tileFlags |= TILESTATE_IMMOVABLENOFIELDBLOCKPATH;
	if (it.getTeleport()) tileFlags |= TILESTATE_TELEPORT;
	if (it.getMagicField()) tileFlags |= TILESTATE_MAGICFIELD;
	if (it.getMailbox()) tileFlags |= TILESTATE_MAILBOX;
	if (it.getTrashHolder()) tileFlags |= TILESTATE_TRASHHOLDER;
	if (it.hasProperty(CONST_PROP_BLOCKSOLID)) tileFlags |= TILESTATE_BLOCKSOLID;
	if (it.getBed()) tileFlags |= TILESTATE_BED;
	if (const Container* container = it.getContainer(); container && container->getDepotLocker()) tileFlags |= TILESTATE_DEPOT;
	if (it.hasProperty(CONST_PROP_SUPPORTHANGABLE)) tileFlags |= TILESTATE_SUPPORTS_HANGABLE;
}

void Tile::resetTileFlags(const Item* item) {
	const ItemType& it = Item::items[item->getID()];
	if (it.floorChange != 0) tileFlags &= ~TILESTATE_FLOORCHANGE;
	if (it.hasProperty(CONST_PROP_BLOCKSOLID) && !hasProperty(item, CONST_PROP_BLOCKSOLID)) tileFlags &= ~TILESTATE_BLOCKSOLID;
	if (it.hasProperty(CONST_PROP_IMMOVABLEBLOCKSOLID) && !hasProperty(item, CONST_PROP_IMMOVABLEBLOCKSOLID)) tileFlags &= ~TILESTATE_IMMOVABLEBLOCKSOLID;
	if (it.hasProperty(CONST_PROP_BLOCKPATH) && !hasProperty(item, CONST_PROP_BLOCKPATH)) tileFlags &= ~TILESTATE_BLOCKPATH;
	if (it.hasProperty(CONST_PROP_NOFIELDBLOCKPATH) && !hasProperty(item, CONST_PROP_NOFIELDBLOCKPATH)) tileFlags &= ~TILESTATE_NOFIELDBLOCKPATH;
	if (it.hasProperty(CONST_PROP_IMMOVABLENOFIELDBLOCKPATH) && !hasProperty(item, CONST_PROP_IMMOVABLENOFIELDBLOCKPATH)) tileFlags &= ~TILESTATE_IMMOVABLENOFIELDBLOCKPATH;
	if (it.getTeleport()) tileFlags &= ~TILESTATE_TELEPORT;
	if (it.getMagicField()) tileFlags &= ~TILESTATE_MAGICFIELD;
	if (it.getMailbox()) tileFlags &= ~TILESTATE_MAILBOX;
	if (it.getTrashHolder()) tileFlags &= ~TILESTATE_TRASHHOLDER;
	if (it.getBed()) tileFlags &= ~TILESTATE_BED;
	if (const Container* container = it.getContainer(); container && container->getDepotLocker()) tileFlags &= ~TILESTATE_DEPOT;
	if (it.hasProperty(CONST_PROP_SUPPORTHANGABLE)) tileFlags &= ~TILESTATE_SUPPORTS_HANGABLE;
}

bool Tile::isMoveableBlocking() const {
	return !ground || hasFlag(TILESTATE_BLOCKSOLID);
}

Item* Tile::getUseItem(int32_t index) const {
	if (items.empty()) return ground;
	if (Thing* thing = getThing(index)) return thing->getItem();
	return nullptr;
}
