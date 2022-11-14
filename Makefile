.PHONY: clean test

########################################
#                                      #
# Shared flags for compilers and such. #
#                                      #
########################################

CXX := clang++
CXXFLAGS := -std=c++17 -Wall -Wno-char-subscripts -Wno-unused -Wno-return-type-c-linkage -DINCLUDE_UTF8_LOOKUP_TABLE -nodefaultlibs -fno-rtti -ffunction-sections -fno-omit-frame-pointer -fno-exceptions -nostdlib -I. -Wno-inaccessible-base -masm=intel
LDFLAGS := -Wl,--gc-sections
ASM := as
ASMFLAGS := 

#######################
#                     #
# Platform detection. #
#                     #
#######################

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

###############################
#                             #
# Sources and object listing. #
#                             #
###############################

CORE_SRCS := $(wildcard core/*.cpp)
LIB_SRCS := $(wildcard lib/*.cpp)
CLOVER_SRCS := $(wildcard clover/*.cpp)
BASIL_SRCS := $(wildcard basil/*.cpp)
JASMINE_SRCS := $(wildcard jasmine/*.cpp) $(wildcard jasmine/arch/*.cpp)

CORE_OBJS := $(CORE_SRCS:.cpp=.o)
LIB_OBJS := $(LIB_SRCS:.cpp=.o)
CLOVER_OBJS := $(CLOVER_SRCS:.cpp=.o)
BASIL_OBJS := $(BASIL_SRCS:.cpp=.o)
JASMINE_OBJS := $(JASMINE_SRCS:.cpp=.o)

CORE_ASMS := core/native/sys_$(OSSTR)_$(ARCHSTR).s
CORE_NATIVE_OBJS := $(CORE_ASMS:.s=.o)

####################
#                  #
# Product targets. #
#                  #
####################

main: release
release: basil-release clover-release jasmine-release libcore-release libcclover-release
debug: basil-debug clover-debug jasmine-debug libcore-debug libcclover-debug

basil-release: CXXFLAGS += -O3
clover-release: CXXFLAGS += -O3
jasmine-release: CXXFLAGS += -O3
libcore-release: CXXFLAGS += -O3
libcclover-release: CXXFLAGS += -O3
gctest-release: CXXFLAGS += -O3 -g3

basil-debug: CXXFLAGS += -O0 -g3
clover-debug: CXXFLAGS += -O0 -g3
jasmine-debug: CXXFLAGS += -O0 -g3
libcore-debug: CXXFLAGS += -O0 -g3
libcclover-debug: CXXFLAGS += -O0 -g3
gctest-debug: CXXFLAGS += -O0 -g3

bin/libcore.a: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS)
	mkdir -p bin
	ar crs $@ $^

bin/libcclover.a: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) cclover.o
	mkdir -p bin
	ar crs $@ $^

bin/basil: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) $(BASIL_OBJS) basil_main.cpp
	mkdir -p bin
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

bin/clover: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) $(CLOVER_OBJS) bin/libcclover.a clover_main.cpp
	mkdir -p bin
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

bin/jasmine: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) $(JASMINE_OBJS) jasmine_main.cpp
	mkdir -p bin
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

bin/gctest: $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) gctest_main.cpp
	mkdir -p bin
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

basil-release: bin/basil
clover-release: bin/clover
jasmine-release: bin/jasmine
libcore-release: bin/libcore.a
libcclover-release: bin/libcclover.a
gctest-release: bin/gctest

basil-debug: bin/basil
clover-debug: bin/clover
jasmine-debug: bin/jasmine
libcore-debug: bin/libcore.a
libcclover-debug: bin/libcclover.a
gctest-debug: bin/gctest

###################
#                 #
# Shared targets. #
#                 #
###################

%.o: %.s
	$(ASM) $(ASMFLAGS) $< -o $@

%.o: %.cpp %.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

#############################
#                           #
# Test sources and objects. #
#                           #
#############################

CORE_TEST_SRCS := $(wildcard core/test/*.cpp)
LIB_TEST_SRCS := $(wildcard lib/test/*.cpp)
CLOVER_TEST_SRCS := $(wildcard clover/test/*.cpp)
JASMINE_TEST_SRCS := $(wildcard jasmine/test/*.cpp) $(wildcard jasmine/arch/test/*.cpp)

CORE_TEST_OBJS := $(CORE_TEST_SRCS:.cpp=.o)
LIB_TEST_OBJS := $(LIB_TEST_SRCS:.cpp=.o)
CLOVER_TEST_OBJS := $(CLOVER_TEST_SRCS:.cpp=.o)
JASMINE_TEST_OBJS := $(JASMINE_TEST_SRCS:.cpp=.o)

#################
#               #
# Test targets. #
#               #
#################

bin/test/core-tests: $(CORE_TEST_OBJS) $(CORE_OBJS)
	mkdir -p bin
	touch $@
	cat $(CORE_TEST_OBJS) > $@.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $@.cpp -o $@
	rm -f $@.cpp

EXAMPLE_SRCS := $(wildcard example/*.cl)
STD_SRCS := $(wildcard std/*.cl)

EXAMPLE_PRODUCTS := $(EXAMPLE_SRCS:.cl=.c) $(EXAMPLE_SRCS:.cl=.h)
STD_PRODUCTS := $(STD_SRCS:.cl=.c) $(STD_SRCS:.cl=.h)

clean:
	rm -f $(CORE_OBJS) $(CORE_NATIVE_OBJS) $(LIB_OBJS) $(BASIL_OBJS) $(CLOVER_OBJS) $(JASMINE_OBJS) $(EXAMPLE_PRODUCTS) $(STD_PRODUCTS) bin/libcore.a cclover.o bin/libcclover.a bin/clover bin/jasmine bin/solver