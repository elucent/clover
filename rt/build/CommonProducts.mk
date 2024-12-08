$(BUILD_CONFIG):
	mkdir -p bin
	$(DETECT) write $(BUILD_CONFIG)

DEBUG_DEPFILE := $(DEBUG_BUILD)/deps.d
RELEASE_DEPFILE := $(RELEASE_BUILD)/deps.d
DEBUG_DEPFILE_INCLUDE := $(filter bin/debug/deps.d,$(wildcard bin/debug/deps.d*))
RELEASE_DEPFILE_INCLUDE := $(filter bin/release/deps.d,$(wildcard bin/release/deps.d*))

$(DEBUG_BUILD)/%.o: %.s $(DEBUG_MANIFEST)
	$(CXX_COMPILE_ASM) $@ $<

$(DEBUG_BUILD)/%.o: %.cpp $(DEBUG_MANIFEST)
	$(CXX_COMPILE_DEBUG) $@ $< $(EMIT_DEPFILE_TO) $(@:.o=.d)

$(RELEASE_BUILD)/%.o: %.s $(RELEASE_MANIFEST)
	$(CXX_COMPILE_ASM) $@ $<

$(RELEASE_BUILD)/%.o: %.cpp $(RELEASE_MANIFEST)
	$(CXX_COMPILE_RELEASE) $@ $< $(EMIT_DEPFILE_TO) $(@:.o=.d)
