# Sources

ASM_SRC_ROOT := $(ASM_PATH)

ASM_CXX_HEADERS := $(wildcard $(ASM_SRC_ROOT)/*.h) $(wildcard $(ASM_SRC_ROOT)/arch/*.h)
ASM_CXX_SOURCES := $(wildcard $(ASM_SRC_ROOT)/*.cpp) $(wildcard $(ASM_SRC_ROOT)/arch/*.cpp)

ASM_DEBUG_OBJS := \
	$(ASM_CXX_SOURCES:%.cpp=$(DEBUG_BUILD)/%.o)
ASM_RELEASE_OBJS := \
	$(ASM_CXX_SOURCES:%.cpp=$(RELEASE_BUILD)/%.o)
ASM_DEBUG_HEADERS := $(ASM_CXX_HEADERS:%.h=$(DEBUG_BUILD)/include/%.h)
ASM_RELEASE_HEADERS := $(ASM_CXX_HEADERS:%.h=$(RELEASE_BUILD)/include/%.h)
ASM_DEBUG_DEPFILES := $(ASM_CXX_SOURCES:%.cpp=$(DEBUG_BUILD)/%.d)
ASM_RELEASE_DEPFILES := $(ASM_CXX_SOURCES:%.cpp=$(RELEASE_BUILD)/%.d)

ASM_TEST_SOURCES := $(wildcard $(ASM_SRC_ROOT)/test/*.cpp) $(wildcard $(ASM_SRC_ROOT)/test/$(OS)/*.cpp) $(wildcard $(ASM_SRC_ROOT)/test/$(ARCH)/*.cpp) $(wildcard $(ASM_SRC_ROOT)/test/$(OS)_$(ARCH)/*.cpp)
ASM_TEST_DEBUG_OBJS := $(ASM_TEST_SOURCES:%.cpp=$(DEBUG_BUILD)/%.o)
ASM_TEST_RELEASE_OBJS := $(ASM_TEST_SOURCES:%.cpp=$(RELEASE_BUILD)/%.o)

ASM_DIRS_DEBUG := \
	$(DEBUG_BUILD)/$(ASM_PATH)/test/$(OS) \
	$(DEBUG_BUILD)/$(ASM_PATH)/test/$(ARCH) \
	$(DEBUG_BUILD)/$(ASM_PATH)/test/$(OS)_$(ARCH) \
	$(DEBUG_BUILD)/$(ASM_PATH)/arch \
	$(DEBUG_BUILD)/include/$(ASM_PATH)/arch
ASM_DIRS_RELEASE := \
	$(RELEASE_BUILD)/$(ASM_PATH)/test/$(OS) \
	$(RELEASE_BUILD)/$(ASM_PATH)/test/$(ARCH) \
	$(RELEASE_BUILD)/$(ASM_PATH)/test/$(OS)_$(ARCH) \
	$(RELEASE_BUILD)/$(ASM_PATH)/arch \
	$(RELEASE_BUILD)/include/$(ASM_PATH)/arch

$(DEBUG_BUILD)/include/%.h: %.h
	cp $< $@

$(RELEASE_BUILD)/include/%.h: %.h
	cp $< $@
