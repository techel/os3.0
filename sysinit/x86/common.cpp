#include "common.hpp"
#include <stdarg.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// common stuff
/////////////////////////////////////////////////////////////////////////////////////////////////////

namespace BootArguments
{
	namespace Modules
	{
		size_t getNumModules()
		{
			return (size_t)*(uint32_t*)(0xD000);
		}
		size_t getModulesSize()
		{
			return (size_t)*(uint32_t*)(0xD004);
		}
		const Entry &getEntry(size_t idx)
		{
			return ((Entry*)0xD008)[idx];
		}
	}
	namespace MemoryMap
	{
		size_t getNumEntries()
		{
			return (size_t)*(uint32_t*)(0xE000);
		}
		const Entry &getEntry(size_t idx)
		{
			return ((Entry*)0xE004)[idx];
		}
	}
};

namespace Logging
{
	void Log(const char *test)
	{
		LogTextCdecl(test);
	}
	void FormatLog(const char *format, ...)
	{
		va_list params;
		va_start(params, format);
		char txt[256];
		char *dst = txt;

		while(*format != '\0')
		{
			if(format[0] == '%' && format[1] != '%' && format[1] != '\0')
			{
				if(format[1] == 'b')
					dst += std::to_string(va_arg(params, unsigned int), dst, 2);
				if(format[1] == 'u')
					dst += std::to_string(va_arg(params, unsigned int), dst, 10);
				if(format[1] == 'x')
					dst += std::to_string(va_arg(params, unsigned int), dst, 16);
				if(format[1] == 'p')
				{
					dst[0] = '0'; dst[1] = 'x';
					dst += 2;
					dst += std::to_string(va_arg(params, uintptr_t), dst, 16);
				}
				format += 2;
			}
			else
			{
				*dst++ = *format++;
			}
		}
		*dst = '\0';
		Log(txt);
	}
}

size_t std::to_string(unsigned int n, char *dest, unsigned int base)
{
	char *beg = dest;
	size_t digits = 0;
	do
	{
		*dest++ = "0123456789ABCDEF"[n % base];
		n /= base;
		digits++;
	}while(n != 0);
	for(size_t i = 0; i < digits / 2; ++i)
		std::swap(beg[i], beg[digits - i - 1]);
	*dest = '\0';
	return digits;
}
