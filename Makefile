.PHONY: clean

CORE_SRCS := $(wildcard core/*.cpp)
LIB_SRCS := $(wildcard lib/*.cpp)
CLOVER_SRCS := $(wildcard clover/*.cpp)
JASMINE_SRCS := $(wildcard jasmine/*.cpp) $(wildcard jasmine/arch/*.cpp)
SOLVER_SRCS := $(wildcard solver/*.cpp)

CORE_OBJS := $(CORE_SRCS:.cpp=.o)
LIB_OBJS := $(LIB_SRCS:.cpp=.o)
CLOVER_OBJS := $(CLOVER_SRCS:.cpp=.o)
JASMINE_OBJS := $(JASMINE_SRCS:.cpp=.o)
SOLVER_OBJS := $(SOLVER_SRCS:.cpp=.o)

CXX := clang++
CXXFLAGS := -std=c++11 -Wall -Wno-unused -Wno-return-type-c-linkage -DINCLUDE_UTF8_LOOKUP_TABLE -nodefaultlibs -Os -fno-rtti -ffunction-sections -fno-exceptions -nostdlib -std=c++11 -I.
LDFLAGS := -O3 -flto
ASM := as
ASMFLAGS := 

ifeq ($(OS),Windows_NT)
	ARCHSTR := amd64
else
	ARCHSTR := $(shell uname -m | tr [:upper:] [:lower:])
	ARCHSTR := $(ARCHSTR:x86_64=amd64)
	ARCHSTR := $(ARCHSTR:i*86=x86)
	ARCHSTR := $(ARCHSTR:aarch64=arm64)
	ARCHSTR := $(ARCHSTR:arm=arm64)
endif

ifeq ($(OS),Windows_NT)
	OSSTR := windows
else
	OSSTR := $(shell uname | tr [:upper:] [:lower:])
endif

ifeq ($(OSSTR), darwin)
	LDFLAGS += -Wl,-static
endif

CORE_ASMS := core/native/sys_$(OSSTR)_$(ARCHSTR).s
CORE_NATIVE_OBJS := $(CORE_ASMS:.s=.o)

main: release
release: clover jasmine libcore libcclover solver
debug: clover-debug jasmine-debug libcore-debug libcclover-debug solver-debug

clover-debug: CXXFLAGS += -O0 -g3
jasmine-debug: CXXFLAGS += -O0 -g3
libcore-debug: CXXFLAGS += -O0 -g3
libcclover-debug: CXXFLAGS += -O0 -g3
solver-debug: CXXFLAGS += -O0 -g3

bin/libcore.a: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS)
	mkdir -p bin
	ar crs $@ $^

bin/libcclover.a: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) cclover.o
	mkdir -p bin
	ar crs $@ $^

bin/clover: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) $(CLOVER_OBJS) $(JASMINE_OBJS) bin/libcclover.a clover_main.cpp
	mkdir -p bin
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

bin/jasmine: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) $(JASMINE_OBJS) jasmine_main.cpp
	mkdir -p bin
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

bin/solver: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) $(SOLVER_OBJS) solver_main.cpp
	mkdir -p bin
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

%.o: %.s
	$(ASM) $(ASMFLAGS) $< -o $@

%.o: %.cpp %.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

clover-debug: bin/clover
jasmine-debug: bin/jasmine
libcore-debug: bin/libcore.a
libcclover-debug: bin/libcclover.a
solver-debug: bin/solver

clover: bin/clover
jasmine: bin/jasmine
libcore: bin/libcore.a
libcclover: bin/libcclover.a
solver: bin/solver

clean:
	rm -f $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) $(CLOVER_OBJS) $(JASMINE_OBJS) bin/libcore.a bin/libcclover.a bin/clover bin/jasmine