#ifndef CREATURE_H
#define CREATURE_H

#include "otpch.h"
#include "game.h"
#include "monster.h"
#include "configmanager.h"
#include "scheduler.h"
#include <fmt/format.h>
#include <memory>
#include <mutex>
#include <algorithm>

// Forward declarations
class CreatureEvent;
class Tile;
class Position;
class Condition;
class Item;
class ItemType;
class Container;
using CreatureEventList = std::vector<CreatureEvent*>;

// Define enums and types (placeholders; replace with actual definitions)
enum CreatureType_t { CREATURETYPE_NONE = 0, CREATURETYPE_PLAYER, CREATURETYPE_MONSTER, CREATURETYPE_NPC };
enum RaceType_t { RACE_NONE = 0, RACE_BLOOD, RACE_VENOM, RACE_UNDEAD };
enum CombatType_t { COMBAT_NONE = 0, COMBAT_PHYSICALDAMAGE, COMBAT_ENERGYDAMAGE };
enum ConditionType_t { CONDITION_NONE = 0, CONDITION_POISON, CONDITION_FIRE };
enum ZoneType_t { ZONE_NORMAL = 0, ZONE_PROTECTION, ZONE_PVP };
enum Skulls_t { SKULL_NONE = 0, SKULL_YELLOW, SKULL_RED };
enum SpeakClasses { TALKTYPE_SAY = 0, TALKTYPE_YELL, TALKTYPE_PRIVATE };

// Placeholder for structs (replace with actual definitions from your codebase)
struct Position { int32_t x = 0, y = 0, z = 0; };
struct Tile { Position pos; };
struct Outfit_t { uint16_t lookType = 0; };
struct LightInfo { int32_t color = 0, level = 0; };
struct FindPathParams {
	int32_t minTargetDist = 1;
	int32_t maxTargetDist = 1;
	bool fullPathSearch = true;
	bool clearSight = true;
	int32_t maxSearchDist = 0;
	FindPathParams() = default;
};

// Define CountBlock_t
struct CountBlock_t {
	int32_t total;
	int64_t ticks;
	CountBlock_t() : total(0), ticks(0) {}
	CountBlock_t(int32_t t, int64_t tk) : total(t), ticks(tk) {}
};

// External declarations
extern Game g_game;
extern ConfigManager g_config;
extern CreatureEvents* g_creatureEvents;

class Creature {
public:
	Creature();
	virtual ~Creature();

	// Visibility and detection
	virtual bool canSee(const Position& myPos, const Position& pos, int32_t viewRangeX, int32_t viewRangeY) const;
	virtual bool canSee(const Position& pos) const;
	virtual bool canSeeCreature(const Creature* creature) const;
	virtual bool canSeeGhostMode(const Creature* creature) const { return true; }
	virtual bool canSeeInvisibility() const { return false; }
	virtual bool isInvisible() const { return false; }

	// Attributes
	virtual void setSkull(Skulls_t newSkull);
	virtual Skulls_t getSkull() const { return skull; }

	// Movement
	virtual int64_t getTimeSinceLastMove() const;
	virtual int32_t getWalkDelay(Direction dir) const;
	virtual int32_t getWalkDelay() const;
	virtual void onWalk();
	virtual bool getNextStep(Direction& dir, uint32_t& flags);
	virtual void startAutoWalk();
	virtual void startAutoWalk(Direction direction);
	virtual void startAutoWalk(const std::vector<Direction>& listDir);
	virtual void addEventWalk(bool firstStep = false);
	virtual void stopEventWalk();
	virtual int64_t getStepDuration(Direction dir) const;
	virtual int64_t getStepDuration() const;
	virtual int64_t getEventStepTicks(bool onlyDelay) const;

	// Thinking and attacking
	virtual void onThink(uint32_t interval);
	virtual void onAttacking(uint32_t interval);
	virtual void onIdleStatus();

	// Event handlers
	virtual void onCreatureAppear(Creature* creature, bool isLogin);
	virtual void onRemoveCreature(Creature* creature, bool isLogout);
	virtual void onCreatureMove(Creature* creature, const Tile* newTile, const Position& newPos,
		const Tile* oldTile, const Position& oldPos, bool teleport);
	virtual void onCreatureDisappear(const Creature* creature, bool isLogout) {}
	virtual void onChangeZone(ZoneType_t zone);
	virtual void onAttackedCreatureChangeZone(ZoneType_t zone);

	// Tile updates
	virtual void onAddTileItem(const Tile* tile, const Position& pos);
	virtual void onUpdateTileItem(const Tile* tile, const Position& pos, const Item*, const ItemType& oldType, const Item*, const ItemType& newType);
	virtual void onRemoveTileItem(const Tile* tile, const Position& pos, const ItemType& iType, const Item*);

	// Combat and health
	virtual void onAttacked();
	virtual void onAttackedCreature(Creature* target);
	virtual void onAttackedCreatureDrainHealth(Creature* target, int32_t points);
	virtual bool onKilledCreature(Creature* target, bool lastHit = true);
	virtual void onGainExperience(uint64_t gainExp, Creature* target);
	virtual void changeHealth(int32_t healthChange, bool sendHealthChange = true);
	virtual void gainHealth(Creature* healer, int32_t healthGain);
	virtual void drainHealth(Creature* attacker, int32_t damage);
	virtual BlockType_t blockHit(Creature* attacker, CombatType_t combatType, int32_t& damage,
		bool checkDefense = false, bool checkArmor = false, bool field = false, bool ignoreResistances = false);
	virtual CreatureVector getKillers();
	virtual void onDeath();
	virtual void death(Creature* lastHitCreature);

	// Conditions
	virtual bool addCondition(Condition* condition, bool force = false);
	virtual bool addCombatCondition(Condition* condition);
	virtual void removeCondition(ConditionType_t type, bool force = false);
	virtual void removeCondition(ConditionType_t type, ConditionId_t conditionId, bool force = false);
	virtual void removeCombatCondition(ConditionType_t type);
	virtual void removeCondition(Condition* condition, bool force = false);
	virtual Condition* getCondition(ConditionType_t type) const;
	virtual Condition* getCondition(ConditionType_t type, ConditionId_t conditionId, uint32_t subId = 0) const;
	virtual void executeConditions(uint32_t interval);
	virtual bool hasCondition(ConditionType_t type, uint32_t subId = 0) const;
	virtual void onAddCondition(ConditionType_t type);
	virtual void onAddCombatCondition(ConditionType_t type);
	virtual void onEndCondition(ConditionType_t type);
	virtual void onTickCondition(ConditionType_t type, bool& bRemove);

	// Immunities
	virtual bool isImmune(CombatType_t type) const;
	virtual bool isImmune(ConditionType_t type) const;
	virtual bool isSuppress(ConditionType_t type) const;

	// Pathfinding
	virtual bool getPathTo(const Position& targetPos, std::vector<Direction>& dirList, const FindPathParams& fpp) const;
	virtual bool getPathTo(const Position& targetPos, std::vector<Direction>& dirList, int32_t minTargetDist, int32_t maxTargetDist,
		bool fullPathSearch = true, bool clearSight = true, int32_t maxSearchDist = 0) const;

	// Master and summons
	virtual bool setMaster(Creature* newMaster);
	virtual bool isSummon() const { return master != nullptr; }

	// Miscellaneous
	virtual double getDamageRatio(Creature* attacker) const;
	virtual uint64_t getGainedExperience(Creature* attacker) const;
	virtual void addDamagePoints(Creature* attacker, int32_t damagePoints);
	virtual bool hasBeenAttacked(uint32_t attackerId);

	// Added virtual methods for derived classes to override
	virtual void setID(uint32_t newId) { id = newId; }
	virtual uint32_t getID() const { return id; }
	virtual void addList() {}
	virtual void removeList() {}
	virtual const std::string& getName() const { static std::string empty; return empty; }
	virtual const std::string& getNameDescription() const { static std::string empty; return empty; }
	virtual std::string getDescription(int32_t lookDistance) const { return "A creature."; }
	virtual CreatureType_t getType() const { return CREATURETYPE_NONE; }
	virtual bool isPushable() const { return false; }
	virtual bool isAttackable() const { return true; }
	virtual RaceType_t getRace() const { return RACE_NONE; }
	virtual int32_t getArmor() const { return 0; }
	virtual int32_t getDefense() const { return 0; }
	virtual void onCreatureSay(Creature* creature, SpeakClasses type, const std::string& text) {}
	virtual void onAttackedCreatureDisappear(bool isLogout) {}
	virtual void onFollowCreatureComplete(Creature* creature) {}
	virtual bool onFollowCreature(Creature* creature) { return true; }
	virtual void onWalkComplete() {}
	virtual void challengeCreature(Creature* creature) {}
	virtual void setNormalCreatureLight() {}
	virtual void getCombatValues(int32_t& min, int32_t& max) { min = 0; max = 0; }
	virtual void doAttacking(uint32_t interval) {}
	virtual bool hasExtraSwing() const { return false; }
	virtual Item* getCorpse() const { return nullptr; }
	virtual uint64_t getLostExperience() const { return 0; }
	virtual uint16_t getLookCorpse() const { return 0; }
	virtual void dropLoot(Container* corpse, Creature* lastHitCreature) {}
	virtual CombatType_t getDamageImmunities() const { return COMBAT_NONE; }
	virtual ConditionType_t getConditionImmunities() const { return CONDITION_NONE; }
	virtual ConditionType_t getConditionSuppressions() const { return CONDITION_NONE; }
	virtual FindPathParams getPathSearchParams() const { return FindPathParams(); }
	virtual bool useCacheMap() const { return true; }
	virtual void onWalkAborted() {}
	virtual void onBlockHit() {}
	virtual void onCombatRemoveCondition(Condition* condition) {}
	virtual void setLastPosition(const Position& pos) {}
	virtual void onTargetCreatureGainHealth(Creature* creature, int32_t healthGain) {}
	virtual bool dropCorpse(Creature* lastHitCreature, Creature* mostDamageCreature, bool lastHitUnjustified, bool mostDamageUnjustified) { return true; }
	virtual Monster* getMonster() { return nullptr; }
	virtual Npc* getNpc() { return nullptr; }
	virtual Player* getPlayer() { return nullptr; }
	virtual const Player* getPlayer() const { return nullptr; }
	virtual bool isInGhostMode() const { return false; }
	virtual bool setFollowCreature(Creature* creature) { followCreature = creature; return true; }
	virtual void goToFollowCreature() {}
	virtual bool setAttackedCreature(Creature* creature) { attackedCreature = creature; return true; }
	virtual void onPlacedCreature() {}
	virtual void getCreatureLight(int32_t& color, int32_t& level) const { color = 0; level = 0; }
	virtual float getAttackFactor() const { return 1.0f; }
	virtual float getDefenseFactor() const { return 1.0f; }
	virtual void onAttackedCreatureBlockHit() {}

	// Added missing methods for ProtocolGame and other uses
	virtual Position getPosition() const { return position; }
	virtual Tile* getTile() const { return tile; }
	virtual Outfit_t getCurrentOutfit() const { return Outfit_t{}; }
	virtual int32_t getSpeed() const { return baseSpeed; }
	virtual Direction getDirection() const { return Direction::NORTH; }

	// Health-related methods
	virtual int32_t getHealth() const { return health; }
	virtual int32_t getMaxHealth() const { return healthMax; }

protected:
	static constexpr int32_t maxWalkCacheWidth = 6;
	static constexpr int32_t maxWalkCacheHeight = 6;
	static constexpr uint32_t inFightTicks = 60000;

	std::mutex creatureMutex;
	std::vector<std::unique_ptr<Condition>> conditions;
	std::vector<Creature*> summons;
	std::vector<Direction> listWalkDir;
	CreatureEventList eventsList;
	std::map<uint32_t, CountBlock_t> damageMap;

	Creature* master = nullptr;
	Creature* followCreature = nullptr;
	Creature* attackedCreature = nullptr;

	Position position;
	Tile* tile = nullptr;
	uint32_t id = 0;
	int32_t baseSpeed = 220;
	int32_t health = 100;
	int32_t healthMax = 100; // Added for Player
	bool skillLoss = true;
	uint32_t lastHitCreatureId = 0;
	int64_t lastStep = 0;
	int32_t lastStepCost = 1;
	uint32_t blockCount = 0;
	uint32_t blockTicks = 0;
	uint32_t walkUpdateTicks = 0;
	uint32_t eventWalk = 0;
	Skulls_t skull = SKULL_NONE;
	LightInfo internalLight{};
	bool isMapLoaded = false;
	bool hasFollowPath = false;
	bool forceUpdateFollowPath = false;
	bool isUpdatingPath = false;
	bool cancelNextWalk = false;
	std::array<std::array<bool, 2 * maxWalkCacheWidth + 1>, 2 * maxWalkCacheHeight + 1> localMapCache{};

	virtual void updateMapCache();
	virtual void updateTileCache(const Tile* tile, int32_t dx, int32_t dy);
	virtual void updateTileCache(const Tile* tile, const Position& pos);
	virtual int32_t getWalkCache(const Position& pos) const;
	virtual bool isRemoved() const { return false; }
	virtual int32_t getStepSpeed() const { return baseSpeed; }
	virtual std::string ucfirst(const std::string& s) const { return s.empty() ? s : (char)toupper(s[0]) + s.substr(1); }
	virtual int32_t uniform_random(int32_t min, int32_t max) const { return min + rand() % (max - min + 1); }
	virtual ZoneType_t getZone() const { return ZONE_NORMAL; }
};

#endif // CREATURE_H
