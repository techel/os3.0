#include "pmmgr.hpp"

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// physical memory management
/////////////////////////////////////////////////////////////////////////////////////////////////////

static std::Spinlock BitmapLock;
constexpr size_t PageSize = 1024 * 4; //4KiB pages
constexpr size_t MaxPages = 1024 * 1024;
constexpr size_t UpperPagesStart = (1024 * 1024 * 16) / PageSize;
constexpr size_t InvalidPage = 0xFFFFFFF;
constexpr size_t BitmapSize = 1024 * 128; // 4GiB/4KiB/8 = 128KiB
static uint8_t *BitmapStart = nullptr;
static size_t NumTotalPages = 0;
static size_t NumFreePages = 0;
constexpr size_t AddressToPage(const void *addr)
{
	return (intptr_t)addr / PageSize;
}
constexpr size_t SizeToPages(size_t n)
{
	return (n + PageSize - 1) / PageSize;
}

static void setPageUsed(size_t page)
{
	BitmapStart[page / 8] |= (1 << (page % 8));
}
static void setPageFree(size_t page)
{
	BitmapStart[page / 8] &= ~(1 << (page % 8));
}
static bool isPageFree(size_t page)
{
	return (BitmapStart[page / 8] & (1 << (page % 8))) == 0;
}

namespace PhysicalMemoryManagement
{
	void Initialize()
	{
		new(&BitmapLock) std::Spinlock;

		BitmapStart = (uint8_t*)((uintptr_t)&FileEnd+BootArguments::Modules::getModulesSize()+0x4000); //place after stack
		Logging::FormatLog("initialize physical memory bitmap at %p", BitmapStart);

		std::fill_n(BitmapStart, BitmapSize, 0xFF); //set all reserved

		for(size_t i = 0; i < BootArguments::MemoryMap::getNumEntries(); ++i)
		{
			const auto &e = BootArguments::MemoryMap::getEntry(i);
			if(e.Type == e.Type_Usable)
			{
				size_t startPage = (e.Base + PageSize - 1) / PageSize;
				size_t numPages = e.Size / PageSize;

				for(size_t p = 0; p < numPages; ++p)
				{
					if(!isPageFree(startPage + p))
					{
						setPageFree(startPage + p);
						NumTotalPages++;
					}
				}
			}
		}

		NumFreePages = NumTotalPages;
		Logging::FormatLog("total memory size: %uKiB (%uMiB)", NumTotalPages*PageSize/1024, NumTotalPages*PageSize/1024/1024);
	}
	size_t getGranularity()
	{
		return PageSize;
	}
	static size_t AllocatePages(size_t regionstart, size_t regionsize, size_t numpages)
	{
		std::Lock guard(BitmapLock);

		size_t contiguousFound = 0, contiguousStart = 0;
		for(size_t i = regionstart; i < regionstart + regionsize; ++i)
		{
			if(isPageFree(i))
			{
				if(contiguousFound == 0)
					contiguousStart = i;
				contiguousFound++;
				if(contiguousFound >= numpages)
				{
					for(size_t p = 0; p < numpages; ++p)
						setPageUsed(contiguousStart + p);
					return contiguousStart;
				}
			}
			else
				contiguousFound = 0;
		}
		return InvalidPage;
	}
	void *Allocate(void *regionstart, size_t regionsize, size_t numbytes)
	{
		size_t p = AllocatePages(AddressToPage(regionstart), SizeToPages(regionsize), SizeToPages(numbytes));
		if(p == InvalidPage)
			return nullptr;
		return (void*)(p * PageSize);
	}
	void *Allocate(size_t numbytes)
	{
		size_t p = AllocatePages(UpperPagesStart, MaxPages - UpperPagesStart, SizeToPages(numbytes));
		if(p == InvalidPage)
			return nullptr;
		return (void*)(p * PageSize);
	}
	bool Acquire(const void *addr, size_t siz)
	{
		std::Lock guard(BitmapLock);
		size_t startPage = AddressToPage(addr);
		size_t numPages = SizeToPages(siz);
		for(size_t p = 0; p < numPages; ++p)
		{
			if(!isPageFree(startPage + p))
				return false;
		}
		for(size_t p = 0; p < numPages; ++p)
			setPageUsed(startPage + p);
		NumFreePages -= numPages;
		return true;
	}
	void Release(const void *addr, size_t siz)
	{
		std::Lock guard(BitmapLock);
		size_t startPage = AddressToPage(addr);
		size_t numPages = SizeToPages(siz);
		for(size_t p = 0; p < numPages; ++p)
			setPageFree(startPage + p);
		NumFreePages += numPages;
	}
	void getUsage(size_t *mtotal, size_t *mfree)
	{
		if(mtotal)
			*mtotal = NumTotalPages*PageSize;
		if(mfree)
			*mfree = NumFreePages*PageSize;
	}
}
