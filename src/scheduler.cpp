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
#include "scheduler.h"
#include <boost/asio/post.hpp>
#include <memory>
#include <atomic>
#include <iostream>

extern Dispatcher g_dispatcher; // Assuming this is defined elsewhere

class Scheduler {
public:
	Scheduler() {
		eventIdTimerMap.reserve(1024); // Pre-reserve for typical load
	}

	uint32_t addEvent(SchedulerTask* task) {
		if (!task) {
#ifdef _DEBUG
			std::cerr << "Scheduler::addEvent: Null task provided" << std::endl;
#endif
			return 0;
		}

		if (task->getEventId() == 0) {
			task->setEventId(++lastEventId);
		}

		boost::asio::post(io_context, [this, task]() {
			if (getState() == THREAD_STATE_TERMINATED) {
				delete task;
				return;
			}

			auto eventId = task->getEventId();
			auto [it, inserted] = eventIdTimerMap.emplace(eventId, boost::asio::steady_timer{ io_context });
			if (!inserted) {
#ifdef _DEBUG
				std::cerr << "Scheduler::addEvent: Duplicate event ID " << eventId << std::endl;
#endif
				delete task;
				return;
			}

			auto& timer = it->second;
			timer.expires_after(std::chrono::milliseconds(task->getDelay()));
			timer.async_wait([this, task](const boost::system::error_code& error) {
				auto it = eventIdTimerMap.find(task->getEventId());
				if (it != eventIdTimerMap.end()) {
					eventIdTimerMap.erase(it); // Only erase if still present
				}

				if (error == boost::asio::error::operation_aborted || getState() == THREAD_STATE_TERMINATED) {
					delete task;
					return;
				}

				g_dispatcher.addTask(task);
				});
			});

		return task->getEventId();
	}

	bool stopEvent(uint32_t eventId) {
		if (eventId == 0) return false;

		bool success = false;
		boost::asio::post(io_context, [this, eventId, &success]() {
			auto it = eventIdTimerMap.find(eventId);
			if (it != eventIdTimerMap.end()) {
				it->second.cancel();
				success = true;
			}
			else {
#ifdef _DEBUG
				std::cerr << "Scheduler::stopEvent: Event ID " << eventId << " not found" << std::endl;
#endif
			}
			});
		return success; // Note: This is optimistic; actual success may depend on async completion
	}

	void shutdown() {
		state.store(THREAD_STATE_TERMINATED, std::memory_order_release);
		boost::asio::post(io_context, [this]() {
			for (auto& [eventId, timer] : eventIdTimerMap) {
				timer.cancel();
			}
			eventIdTimerMap.clear(); // Ensure all timers are cleaned up
			io_context.stop();
			});
	}

	// AI-specific: Add prioritized event
	uint32_t addPriorityEvent(SchedulerTask* task) {
		task->setPriority(true); // Assuming SchedulerTask has a priority flag
		return addEvent(task);
	}

	// AI-specific: Batch scheduling for creature updates
	std::vector<uint32_t> addBatchEvents(std::vector<SchedulerTask*> tasks) {
		std::vector<uint32_t> eventIds;
		eventIds.reserve(tasks.size());

		boost::asio::post(io_context, [this, tasks = std::move(tasks)]() {
			for (auto* task : tasks) {
				if (!task) continue;

				if (task->getEventId() == 0) {
					task->setEventId(++lastEventId);
				}

				auto eventId = task->getEventId();
				auto& timer = eventIdTimerMap.emplace(eventId, boost::asio::steady_timer{ io_context }).first->second;
				timer.expires_after(std::chrono::milliseconds(task->getDelay()));
				timer.async_wait([this, task](const boost::system::error_code& error) {
					auto it = eventIdTimerMap.find(task->getEventId());
					if (it != eventIdTimerMap.end()) {
						eventIdTimerMap.erase(it);
					}

					if (error == boost::asio::error::operation_aborted || getState() == THREAD_STATE_TERMINATED) {
						delete task;
						return;
					}

					g_dispatcher.addTask(task);
					});
			}
			});

		for (const auto* task : tasks) {
			if (task) eventIds.push_back(task->getEventId());
		}
		return eventIds;
	}

	inline ThreadState getState() const { return state.load(std::memory_order_acquire); }
	void setState(ThreadState newState) { state.store(newState, std::memory_order_release); }

private:
	boost::asio::io_context io_context;
	std::unordered_map<uint32_t, boost::asio::steady_timer> eventIdTimerMap;
	std::atomic<uint32_t> lastEventId{ 0 };
	std::atomic<ThreadState> state{ THREAD_STATE_RUNNING };
};

Scheduler g_scheduler; // Assuming singleton usage

SchedulerTask* createSchedulerTask(uint32_t delay, TaskFunc&& f) {
	return new SchedulerTask(delay, std::move(f));
}
