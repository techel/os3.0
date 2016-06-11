#pragma once
#include "common.hpp"

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// physical memory management
/////////////////////////////////////////////////////////////////////////////////////////////////////

namespace PhysicalMemoryManagement
{
	void Initialize();
	size_t getGranularity();
	void *Allocate(void *regionstart, size_t regionsize, size_t numbytes);
	void *Allocate(size_t siz);
	bool Acquire(const void *addr, size_t siz);
	void Release(const void *addr, size_t size);
	void getUsage(size_t *mtotal, size_t *mfree);
};

