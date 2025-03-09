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
#include "networkmessage.h"
#include "container.h"
#include "creature.h"
#include <string_view>
#include <cstring>

static constexpr size_t MAX_BUFFER_SIZE = 16384; // Increased from 8192 for flexibility
static constexpr uint8_t fluidMap[] = { 0, 1, 7, 6, 5, 4, 3, 2 }; // Assuming this is defined elsewhere

class NetworkMessage {
public:
	struct MsgInfo {
		size_t position = 0;
		size_t length = 0;
	};

	NetworkMessage() : buffer(new uint8_t[MAX_BUFFER_SIZE]) {}

	// Pre-reserve space for expected data size
	void reserve(size_t size) {
		if (size > MAX_BUFFER_SIZE) {
			// Log error in debug builds
#ifdef _DEBUG
			std::cerr << "NetworkMessage::reserve: Requested size " << size << " exceeds max buffer size " << MAX_BUFFER_SIZE << std::endl;
#endif
			return;
		}
		if (size > capacity) {
			capacity = size;
			std::unique_ptr<uint8_t[]> newBuffer(new uint8_t[capacity]);
			std::memcpy(newBuffer.get(), buffer.get(), info.length);
			buffer = std::move(newBuffer);
		}
	}

	std::string getString(uint16_t stringLen = 0) {
		if (stringLen == 0) {
			stringLen = get<uint16_t>();
		}
		if (!canRead(stringLen)) {
#ifdef _DEBUG
			std::cerr << "NetworkMessage::getString: Cannot read " << stringLen << " bytes at position " << info.position << std::endl;
#endif
			return std::string();
		}
		char* data = reinterpret_cast<char*>(buffer.get() + info.position);
		info.position += stringLen;
		return std::string(data, stringLen);
	}

	Position getPosition() const {
		Position pos;
		pos.x = get<uint16_t>();
		pos.y = get<uint16_t>();
		pos.z = getByte();
		return pos;
	}

	void addString(std::string_view value) {
		size_t stringLen = value.length();
		if (!canAdd(stringLen + 2) || stringLen > 8192) {
#ifdef _DEBUG
			std::cerr << "NetworkMessage::addString: Cannot add string of length " << stringLen << std::endl;
#endif
			return;
		}
		add<uint16_t>(static_cast<uint16_t>(stringLen));
		std::memcpy(buffer.get() + info.position, value.data(), stringLen);
		info.position += stringLen;
		info.length += stringLen;
	}

	void addDouble(double value, uint8_t precision = 2) {
		if (!canAdd(5)) return; // 1 byte precision + 4 bytes value
		addByte(precision);
		// Use a compact int32_t representation
		int32_t scaled = static_cast<int32_t>(value * std::pow(10.0f, precision));
		add<uint32_t>(static_cast<uint32_t>(scaled + std::numeric_limits<int32_t>::max()));
	}

	void addBytes(const char* bytes, size_t size) {
		if (!canAdd(size) || size > 8192) {
#ifdef _DEBUG
			std::cerr << "NetworkMessage::addBytes: Cannot add " << size << " bytes" << std::endl;
#endif
			return;
		}
		std::memcpy(buffer.get() + info.position, bytes, size);
		info.position += size;
		info.length += size;
	}

	void addPaddingBytes(size_t n) {
		if (!canAdd(n)) return;
		std::memset(buffer.get() + info.position, 0x33, n);
		info.position += n;
		info.length += n;
	}

	void addPosition(const Position& pos) {
		if (!canAdd(5)) return; // 2 + 2 + 1 bytes
		add<uint16_t>(pos.x);
		add<uint16_t>(pos.y);
		addByte(pos.z);
	}

	void addItem(uint16_t id, uint8_t count) {
		const ItemType& it = Item::items[id];
		if (!canAdd(it.stackable || it.isSplash() || it.isFluidContainer() ? 3 : 2)) return;
		add<uint16_t>(it.clientId);
		if (it.stackable) {
			addByte(count > 0xFF ? 0xFF : count); // Cap at 255
		}
		else if (it.isSplash() || it.isFluidContainer()) {
			addByte(fluidMap[count & 7]);
		}
	}

	void addItem(const Item* item) {
		if (!item) return;
		const ItemType& it = Item::items[item->getID()];
		if (!canAdd(it.stackable || it.isSplash() || it.isFluidContainer() ? 3 : 2)) return;
		add<uint16_t>(it.clientId);
		if (it.stackable) {
			addByte(std::min<uint16_t>(0xFF, item->getItemCount()));
		}
		else if (it.isSplash() || it.isFluidContainer()) {
			addByte(fluidMap[item->getFluidType() & 7]);
		}
	}

	void addItemId(uint16_t itemId) {
		if (!canAdd(2)) return;
		add<uint16_t>(Item::items[itemId].clientId);
	}

	// Batch method for multiple items (AI optimization)
	void addItems(const std::vector<std::pair<uint16_t, uint8_t>>& items) {
		size_t requiredSize = items.size() * 3; // Worst case: 2 bytes ID + 1 byte count
		if (!canAdd(requiredSize)) return;
		for (const auto& [id, count] : items) {
			addItem(id, count);
		}
	}

	// Creature state serialization for AI updates
	void addCreatureState(const Creature* creature) {
		if (!creature || !canAdd(8)) return; // 5 bytes pos + 1 byte dir + 2 bytes health
		addPosition(creature->getPosition());
		addByte(static_cast<uint8_t>(creature->getDirection()));
		add<uint16_t>(std::min<uint32_t>(0xFFFF, creature->getHealth()));
	}

	// Inline small methods
	inline uint8_t getByte() const {
		return canRead(1) ? buffer[info.position++] : 0;
	}

	inline void addByte(uint8_t value) {
		if (canAdd(1)) buffer[info.position++] = value, info.length++;
	}

	template<typename T>
	T get() const {
		if (!canRead(sizeof(T))) return T();
		T v;
		std::memcpy(&v, buffer.get() + info.position, sizeof(T));
		info.position += sizeof(T);
		return v;
	}

	template<typename T>
	void add(T value) {
		if (!canAdd(sizeof(T))) return;
		std::memcpy(buffer.get() + info.position, &value, sizeof(T));
		info.position += sizeof(T);
		info.length += sizeof(T);
	}

	// Error state
	bool hasError() const { return errorState; }
	void reset() { info.position = 0; info.length = 0; errorState = false; }

private:
	bool canRead(size_t n) const {
		return info.position + n <= info.length && info.position + n <= MAX_BUFFER_SIZE;
	}

	bool canAdd(size_t n) {
		if (info.position + n > MAX_BUFFER_SIZE) {
			errorState = true;
			return false;
		}
		if (info.position + n > capacity) {
			capacity = std::max(capacity * 2, info.position + n);
			std::unique_ptr<uint8_t[]> newBuffer(new uint8_t[capacity]);
			std::memcpy(newBuffer.get(), buffer.get(), info.length);
			buffer = std::move(newBuffer);
		}
		return true;
	}

	std::unique_ptr<uint8_t[]> buffer;
	size_t capacity = MAX_BUFFER_SIZE;
	MsgInfo info;
	bool errorState = false;
};
