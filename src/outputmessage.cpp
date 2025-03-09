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
#include "outputmessage.h"
#include "protocol.h"
#include "lockfree.h"
#include "scheduler.h"
#include <algorithm>
#include <iostream>

extern Scheduler g_scheduler;

namespace {

	const uint16_t OUTPUTMESSAGE_FREE_LIST_CAPACITY = 4096; // Increased from 2048 for higher load
	const std::chrono::milliseconds OUTPUTMESSAGE_AUTOSEND_DELAY{ 10 };
	const size_t MAX_PROTOCOLS = 10000; // Arbitrary limit to prevent unbounded growth

	void sendAll(std::vector<Protocol_ptr>& bufferedProtocols); // Pass by reference to avoid copying

	void scheduleSendAll(std::vector<Protocol_ptr>& bufferedProtocols) {
		g_scheduler.addEvent(createSchedulerTask(OUTPUTMESSAGE_AUTOSEND_DELAY.count(),
			[&bufferedProtocols]() { sendAll(bufferedProtocols); }));
	}

	void sendAll(std::vector<Protocol_ptr>& bufferedProtocols) {
		// Dispatcher thread
		if (bufferedProtocols.empty()) return;

		// Process in batches to improve cache locality
		constexpr size_t BATCH_SIZE = 64;
		for (size_t i = 0; i < bufferedProtocols.size(); i += BATCH_SIZE) {
			size_t end = std::min(i + BATCH_SIZE, bufferedProtocols.size());
			for (size_t j = i; j < end; ++j) {
				auto& protocol = bufferedProtocols[j];
				if (!protocol) {
#ifdef _DEBUG
					std::cerr << "OutputMessagePool::sendAll: Null protocol detected" << std::endl;
#endif
					continue;
				}
				auto msg = protocol->getCurrentBuffer();
				if (msg) {
					protocol->send(std::move(msg));
				}
			}
		}

		// Only reschedule if there are still protocols with pending messages
		bool hasPending = false;
		for (const auto& protocol : bufferedProtocols) {
			if (protocol && protocol->getCurrentBuffer()) {
				hasPending = true;
				break;
			}
		}
		if (hasPending) {
			scheduleSendAll(bufferedProtocols);
		}
	}

}

class OutputMessagePool {
public:
	OutputMessagePool() {
		bufferedProtocols.reserve(128); // Pre-reserve for typical server load
	}

	void addProtocolToAutosend(Protocol_ptr protocol) {
		// Dispatcher thread
		if (!protocol) {
#ifdef _DEBUG
			std::cerr << "OutputMessagePool::addProtocolToAutosend: Null protocol" << std::endl;
#endif
			return;
		}

		if (bufferedProtocols.size() >= MAX_PROTOCOLS) {
#ifdef _DEBUG
			std::cerr << "OutputMessagePool::addProtocolToAutosend: Protocol limit reached (" << MAX_PROTOCOLS << ")" << std::endl;
#endif
			return;
		}

		if (bufferedProtocols.empty()) {
			scheduleSendAll(bufferedProtocols);
		}
		bufferedProtocols.push_back(std::move(protocol)); // Move to avoid copying
	}

	void removeProtocolFromAutosend(const Protocol_ptr& protocol) {
		// Dispatcher thread
		if (!protocol) return;

		auto it = std::find(bufferedProtocols.begin(), bufferedProtocols.end(), protocol);
		if (it != bufferedProtocols.end()) {
			std::swap(*it, bufferedProtocols.back());
			bufferedProtocols.pop_back();
		}
		else {
#ifdef _DEBUG
			std::cerr << "OutputMessagePool::removeProtocolFromAutosend: Protocol not found" << std::endl;
#endif
		}
	}

	OutputMessage_ptr getOutputMessage() {
		return std::allocate_shared<OutputMessage>(LockfreePoolingAllocator<void, OUTPUTMESSAGE_FREE_LIST_CAPACITY>());
	}

	// AI-specific: Prioritize sending for critical updates (e.g., creature AI actions)
	void addProtocolToPrioritySend(Protocol_ptr protocol) {
		if (!protocol) return;
		auto msg = protocol->getCurrentBuffer();
		if (msg) {
			protocol->send(std::move(msg)); // Immediate send for priority
		}
		addProtocolToAutosend(std::move(protocol)); // Then add to regular autosend
	}

	// Batch creature updates for AI efficiency
	void batchCreatureUpdates(const std::vector<Protocol_ptr>& protocols) {
		if (bufferedProtocols.size() + protocols.size() > MAX_PROTOCOLS) {
#ifdef _DEBUG
			std::cerr << "OutputMessagePool::batchCreatureUpdates: Exceeds protocol limit" << std::endl;
#endif
			return;
		}
		if (bufferedProtocols.empty()) {
			scheduleSendAll(bufferedProtocols);
		}
		bufferedProtocols.insert(bufferedProtocols.end(), protocols.begin(), protocols.end());
	}

private:
	std::vector<Protocol_ptr> bufferedProtocols;
};

OutputMessagePool g_outputMessagePool; // Assuming singleton usage
