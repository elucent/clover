# Sources

RT_SRC_ROOT := $(RT_PATH)

RT_CXX_HEADERS := $(wildcard $(RT_SRC_ROOT)/*.h) $(wildcard $(RT_SRC_ROOT)/common/*.h) $(wildcard $(RT_SRC_ROOT)/$(OS)/*.h) $(wildcard $(RT_SRC_ROOT)/$(ARCH)/*.h) $(wildcard $(RT_SRC_ROOT)/$(OS)_$(ARCH)/*.h)
RT_CXX_SOURCES := $(wildcard $(RT_SRC_ROOT)/*.cpp) $(wildcard $(RT_SRC_ROOT)/common/*.cpp) $(wildcard $(RT_SRC_ROOT)/$(OS)/*.cpp) $(wildcard $(RT_SRC_ROOT)/$(ARCH)/*.cpp) $(wildcard $(RT_SRC_ROOT)/$(OS)_$(ARCH)/*.cpp)
RT_ASM_SOURCES := $(wildcard $(RT_SRC_ROOT)/$(ARCH)/*.s) $(wildcard $(RT_SRC_ROOT)/$(OS)_$(ARCH)/*.s)

RT_DEBUG_OBJS := \
	$(RT_CXX_SOURCES:%.cpp=$(DEBUG_BUILD)/%.o) \
	$(RT_ASM_SOURCES:%.s=$(DEBUG_BUILD)/%.o)
RT_RELEASE_OBJS := \
	$(RT_CXX_SOURCES:%.cpp=$(RELEASE_BUILD)/%.o) \
	$(RT_ASM_SOURCES:%.s=$(RELEASE_BUILD)/%.o)
RT_DEBUG_HEADERS := $(RT_CXX_HEADERS:%.h=$(DEBUG_BUILD)/include/%.h)
RT_RELEASE_HEADERS := $(RT_CXX_HEADERS:%.h=$(RELEASE_BUILD)/include/%.h)
RT_DEBUG_DEPFILES := $(RT_CXX_SOURCES:%.cpp=$(DEBUG_BUILD)/%.d)
RT_RELEASE_DEPFILES := $(RT_CXX_SOURCES:%.cpp=$(RELEASE_BUILD)/%.d)

ifeq ($(RT_LIBC_COMPATIBLE), 1)
RT_ENTRY_CXX_HEADERS := $(wildcard $(RT_SRC_ROOT)/entry/libc/*.h)
RT_ENTRY_CXX_SOURCES := $(wildcard $(RT_SRC_ROOT)/entry/libc/*.cpp)
RT_ENTRY_ASM_SOURCES := $(wildcard $(RT_SRC_ROOT)/entry/libc/*.s)
else
RT_ENTRY_CXX_HEADERS := $(wildcard $(RT_SRC_ROOT)/entry/$(OS)_$(ARCH)/*.h)
RT_ENTRY_CXX_SOURCES := $(wildcard $(RT_SRC_ROOT)/entry/$(OS)_$(ARCH)/*.cpp)
RT_ENTRY_ASM_SOURCES := $(wildcard $(RT_SRC_ROOT)/entry/$(OS)_$(ARCH)/*.s)
endif

RT_ENTRY_DEBUG_OBJS := \
	$(RT_ENTRY_CXX_SOURCES:%.cpp=$(DEBUG_BUILD)/%.o) \
	$(RT_ENTRY_ASM_SOURCES:%.s=$(DEBUG_BUILD)/%.o)
RT_ENTRY_RELEASE_OBJS := \
	$(RT_ENTRY_CXX_SOURCES:%.cpp=$(RELEASE_BUILD)/%.o) \
	$(RT_ENTRY_ASM_SOURCES:%.s=$(RELEASE_BUILD)/%.o)
RT_ENTRY_DEBUG_HEADERS := $(RT_ENTRY_CXX_HEADERS:%.h=$(DEBUG_BUILD)/include/%.h)
RT_ENTRY_RELEASE_HEADERS := $(RT_ENTRY_XX_HEADERS:%.h=$(RELEASE_BUILD)/include/%.h)
RT_ENTRY_DEBUG_DEPFILES := $(RT_ENTRY_CXX_SOURCES:%.cpp=$(DEBUG_BUILD)/%.d)
RT_ENTRY_RELEASE_DEPFILES := $(RT_ENTRY_CXX_SOURCES:%.cpp=$(RELEASE_BUILD)/%.d)

RT_DIRS_DEBUG := \
	$(DEBUG_BUILD)/$(RT_SRC_ROOT)/common \
	$(DEBUG_BUILD)/$(RT_SRC_ROOT)/$(OS) \
	$(DEBUG_BUILD)/$(RT_SRC_ROOT)/$(ARCH) \
	$(DEBUG_BUILD)/$(RT_SRC_ROOT)/$(OS)_$(ARCH) \
	$(DEBUG_BUILD)/$(RT_SRC_ROOT)/entry/libc \
	$(DEBUG_BUILD)/$(RT_SRC_ROOT)/entry/$(OS)_$(ARCH) \
	$(DEBUG_BUILD)/include/$(RT_SRC_ROOT)/common \
	$(DEBUG_BUILD)/include/$(RT_SRC_ROOT)/$(OS) \
	$(DEBUG_BUILD)/include/$(RT_SRC_ROOT)/$(ARCH) \
	$(DEBUG_BUILD)/include/$(RT_SRC_ROOT)/$(OS)_$(ARCH) \
	$(DEBUG_BUILD)/include/$(RT_SRC_ROOT)/entry/libc \
	$(DEBUG_BUILD)/include/$(RT_SRC_ROOT)/entry/$(OS)_$(ARCH)
RT_DIRS_RELEASE := \
	$(RELEASE_BUILD)/$(RT_SRC_ROOT)/common \
	$(RELEASE_BUILD)/$(RT_SRC_ROOT)/$(OS) \
	$(RELEASE_BUILD)/$(RT_SRC_ROOT)/$(ARCH) \
	$(RELEASE_BUILD)/$(RT_SRC_ROOT)/$(OS)_$(ARCH) \
	$(RELEASE_BUILD)/$(RT_SRC_ROOT)/entry/libc \
	$(RELEASE_BUILD)/$(RT_SRC_ROOT)/entry/$(OS)_$(ARCH) \
	$(RELEASE_BUILD)/include/$(RT_SRC_ROOT)/common \
	$(RELEASE_BUILD)/include/$(RT_SRC_ROOT)/$(OS) \
	$(RELEASE_BUILD)/include/$(RT_SRC_ROOT)/$(ARCH) \
	$(RELEASE_BUILD)/include/$(RT_SRC_ROOT)/$(OS)_$(ARCH) \
	$(RELEASE_BUILD)/include/$(RT_SRC_ROOT)/entry/libc \
	$(RELEASE_BUILD)/include/$(RT_SRC_ROOT)/entry/$(OS)_$(ARCH)

$(DEBUG_BUILD)/include/%.h: %.h
	cp $< $@

$(RELEASE_BUILD)/include/%.h: %.h
	cp $< $@
