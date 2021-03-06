#include "common.hpp"
#include "pmmgr.hpp"

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// main system initialization
/////////////////////////////////////////////////////////////////////////////////////////////////////

extern "C" void InitializeSystem()
{
	PhysicalMemoryManagement::Initialize();
	Logging::FormatLog("%p %p", PhysicalMemoryManagement::Allocate(5000), PhysicalMemoryManagement::Allocate(5000));
}
