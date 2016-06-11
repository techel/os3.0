#pragma once
#include <stdint.h>
#include <stddef.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// common stuff
/////////////////////////////////////////////////////////////////////////////////////////////////////

extern "C"
{
	extern uintptr_t FileStart;
	extern uintptr_t FileEnd;
	void LogTextCdecl(const char *string);
	void EnterSpinlockCdecl(uint32_t *v);
	void LeaveSpinlockCdecl(uint32_t *v);
}

inline void *operator new(size_t sz, void *here) { return here; }

namespace BootArguments
{
	namespace Modules
	{
		#pragma pack(push, 1)
		struct Entry
		{
			char Type[8];
			uint32_t Location, Size;
		};
		#pragma pack(pop)

		size_t getNumModules();
		size_t getModulesSize();
		const Entry &getEntry(size_t idx);
	}

	namespace MemoryMap
	{
		#pragma pack(push, 1)
		struct Entry
		{
			uint32_t Base, Size, Type;
			enum { Type_Usable=0, Type_Unusable=1 };
		};
		#pragma pack(pop)

		size_t getNumEntries();
		const Entry &getEntry(size_t idx);
	}
};

namespace Logging
{
	void Log(const char *test);
	void FormatLog(const char *format, ...);
}

namespace std
{
	template<class T>
	void swap(T &t1, T &t2)
	{
		T tmp(t1);
		t1 = t2;
		t2 = tmp;
	}
	size_t to_string(unsigned int n, char *dest, unsigned int base = 10);

	template<class I, class T>
	void fill_n(I iterbegin, size_t size, const T &value)
	{
		for(size_t i = 0; i < size; ++i)
			*iterbegin++ = value;
	}

	class Spinlock
	{
	private:
		uint32_t Locked;
	public:
		Spinlock() : Locked(0) {}
		void Enter() { EnterSpinlockCdecl(&Locked); }
		void Leave() { LeaveSpinlockCdecl(&Locked); }
	};

	class Lock
	{
	private:
		Spinlock &Lck;
	public:
		Lock(Spinlock &l) : Lck(l) { l.Enter(); }
		~Lock() { Lck.Leave(); }
	};
}


