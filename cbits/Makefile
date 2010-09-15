cbits: $(BUILD_DIR)/Makefile
	$(MAKE) -C $(BUILD_DIR)

$(BUILD_DIR)/Makefile: hsqml-cbits.pro
	mkdir -p $(BUILD_DIR)
	CBITS_DIR=$$PWD && cd $(BUILD_DIR) && qmake $$CBITS_DIR/hsqml-cbits.pro
