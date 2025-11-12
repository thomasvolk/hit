BUILD_PATH=_build
INDEX_PATH=$(BUILD_PATH)/test_index
HIT=$(BUILD_PATH)/install/default/bin/hit
LOG_LEVEL=error

all: unit_test build

test: unit_test integration_test

unit_test:
	dune test

build:
	dune build

integration_test: build
	$(HIT) init -l $(LOG_LEVEL) -d $(INDEX_PATH)
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) Makefile
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) dune-project
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) README.org
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) LICENSE
	$(HIT) import -l $(LOG_LEVEL) -d $(INDEX_PATH) -t ml lib
	@echo "--- search for: install or sexp"
	$(HIT) search -l $(LOG_LEVEL) -m -c 3 -d $(INDEX_PATH) install sexp
	@echo "--- search for: content and license"
	$(HIT) query -l $(LOG_LEVEL) -m -c 3 -d $(INDEX_PATH) '(and (eq content) (eq license))'
	@echo "--- search for: content or license"
	$(HIT) query -l $(LOG_LEVEL) -m -c 3 -d $(INDEX_PATH) '(or (eq content) (eq license))'
	@echo "--- search for: hit"
	$(HIT) search -l $(LOG_LEVEL) -m -c 3 -d $(INDEX_PATH) hit
	@echo "--- search for: hit (IDs only)"
	IDS=$$($(HIT) search -l $(LOG_LEVEL) -m -c 3 -d $(INDEX_PATH) hit | awk '{print $$1}'); \
	echo "IDs found: $$IDS"; \
	for id in $$IDS; do \
	  echo "Delete document with ID: $$id"; \
	  $(HIT) delete -l $(LOG_LEVEL) -d $(INDEX_PATH) $$id; \
	done
	@echo "--- search for: hit"
	$(HIT) search -l $(LOG_LEVEL) -m -c 3 -d $(INDEX_PATH) hit





format:
	dune fmt

clean:
	dune clean
	rm -rf my_index
