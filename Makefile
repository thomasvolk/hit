BUILD_PATH=_build
INDEX_PATH=$(BUILD_PATH)/test_index
HIT=$(BUILD_PATH)/install/default/bin/hit

all: unit_test integration_test

unit_test:
	dune test

build:
	dune build

integration_test: build
	$(HIT) init -l -d $(INDEX_PATH)
	$(HIT) add -l -d $(INDEX_PATH) Makefile
	$(HIT) add -l -d $(INDEX_PATH) dune-project
	$(HIT) add -l -d $(INDEX_PATH) README.md
	$(HIT) add -l -d $(INDEX_PATH) LICENSE
	$(HIT) import -l -d $(INDEX_PATH) -t ml lib
	$(HIT) search -l -m -c 3 -d $(INDEX_PATH) install sexp

clean:
	dune clean
