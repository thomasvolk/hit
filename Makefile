BUILD_PATH=_build
INDEX_PATH=$(BUILD_PATH)/test_index
HIT=$(BUILD_PATH)/install/default/bin/hit

all: unit_test integration_test

unit_test:
	dune test

build:
	dune build

integration_test: build
	$(HIT) add -p $(INDEX_PATH) Makefile
	$(HIT) add -p $(INDEX_PATH) dune-project
	$(HIT) add -p $(INDEX_PATH) README.md
	$(HIT) add -p $(INDEX_PATH) LICENSE
	$(HIT) import -p $(INDEX_PATH) -e ml lib
	$(HIT) search -d -p $(INDEX_PATH) install sexp

clean:
	dune clean
