# Platform properties

OS := $(shell $(DETECT) os)
ARCH := $(shell $(DETECT) arch)
WORDSIZE := $(shell $(DETECT) wordsize)

OS_UPPER := $(shell echo $(OS) | tr a-z A-Z)
ARCH_UPPER := $(shell echo $(ARCH) | tr a-z A-Z)
DEFINES =

CC := $(shell $(DETECT) cc)
CXX := $(shell $(DETECT) cxx)
TOOLCHAIN := $(shell $(DETECT) toolchain)

ifeq ($(RT_FREESTANDING), 1)
FREESTANDING := 1
endif

ifeq ($(TOOLCHAIN), msvc)
else
	# clang or gcc
	ASM_FLAGS := -masm=intel -fPIC
	CXX_FLAGS := $(INCLUDE_DIRS:%=-I%) -std=c++20 -fPIC \
		-Wall -Werror -Wno-unused -Wno-array-bounds \
		-fno-exceptions -fno-rtti -ffunction-sections -fdata-sections -fno-omit-frame-pointer \
		-DRT_GCC_COMPATIBLE=1 -DRT_$(OS_UPPER)=1 -DRT_$(ARCH_UPPER)=1 -DRT_$(WORDSIZE)=1
	DEFINES += RT_GCC_COMPATIBLE
	FREESTANDING_FLAGS := -DRT_FREESTANDING=1 -nostdlib -nostdlib++ -nodefaultlibs -fno-builtin -ffreestanding -fno-builtin-bcmp -fno-use-cxa-atexit -fno-threadsafe-statics
	ifeq ($(RT_FREESTANDING), 1)
		CXX_FLAGS += $(FREESTANDING_FLAGS)
	else
		CXX_FLAGS += -DRT_LIBC_COMPATIBLE=1
	endif
	DEBUG_CXX_FLAGS := -O0 -g3 -Ibin/debug #-fsanitize=undefined,address -fno-sanitize=function -DRT_ASAN=1
	DEBUG_LINK_FLAGS := -Wl,--gc-sections -z noexecstack
	RELEASE_CXX_FLAGS := -O3 -g -ffast-math -Ibin/release -DRELEASE
	RELEASE_LINK_FLAGS := -Wl,--gc-sections -z noexecstack
	CXX_EMIT_DEPFILE_DEBUG = $(CXX) $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) -E -o
	CXX_EMIT_DEPFILE_RELEASE = $(CXX) $(CXX_FLAGS) $(RELEASE_CXX_FLAGS) -E -o
	EMIT_DEPFILE_TO := -MMD -MF
	CXX_COMPILE_ASM = $(CXX) $(ASM_FLAGS) -c -o
	CXX_COMPILE_DEBUG = $(CXX) $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) -c -o
	CXX_COMPILE_RELEASE = $(CXX) $(CXX_FLAGS) $(RELEASE_CXX_FLAGS) -c -o
	CXX_LINK_SHARED_DEBUG = $(CXX) $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) $(DEBUG_LINK_FLAGS) -shared -o
	CXX_LINK_SHARED_RELEASE = $(CXX) $(CXX_FLAGS) $(RELEASE_CXX_FLAGS) $(RELEASE_LINK_FLAGS)  -shared -o
	CXX_LINK_EXECUTABLE_DEBUG = $(CXX) $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) $(DEBUG_LINK_FLAGS) -o
	CXX_LINK_EXECUTABLE_RELEASE = $(CXX) $(CXX_FLAGS) $(RELEASE_CXX_FLAGS) $(RELEASE_LINK_FLAGS) -o
endif
