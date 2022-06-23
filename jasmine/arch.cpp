#include "jasmine/arch.h"

#if defined(LIBCORE_LINUX) && defined(LIBCORE_AMD64)
#include "jasmine/arch/amd64.h"
const Target& DEFAULT_TARGET = TargetProps<AMD64LinuxTarget>::INSTANCE;
#endif