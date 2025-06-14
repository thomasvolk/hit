BUILD_PATH=_build
INDEX_PATH=$(BUILD_PATH)/test_index
HIT=$(BUILD_PATH)/install/default/bin/hit


build:
	dune build

int_test: build
	find . \( -path "./.git" -o -path "./$(BUILD_PATH)"  \) -prune -o -type d -o -exec $(HIT) add -p $(INDEX_PATH) {} \;
	$(HIT) search -p $(INDEX_PATH) install dune

clean:
	dune clean
