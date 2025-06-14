BUILD_PATH=_build
INDEX_PATH=$(BUILD_PATH)/test_index


build:
	dune build

int_test: build
	find . \( -path "./.git" -o -path "./$(BUILD_PATH)"  \) -prune -o -type d -o -exec ./$(BUILD_PATH)/install/default/bin/hit add -p $(INDEX_PATH) {} \;

clean:
	dune clean
