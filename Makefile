BUILD_PATH=_build
INDEX_PATH=$(BUILD_PATH)/test_index
BENCHMARK_INDEX_PATH=$(BUILD_PATH)/test_index_benchmark
HIT=$(BUILD_PATH)/install/default/bin/hit
LOG_LEVEL=info

all: integration_test doc-test benchmark

test:
	dune test

build:
	dune build

doc:
	dune build @doc

install: test build
	dune install

format:
	dune fmt

clean:
	dune clean
	rm -rf my_index

doc-test:
	./org-babel-tangle.sh README.org
	utop example.index-api.utop

benchmark: build
	$(HIT) benchmark -f -d $(BENCHMARK_INDEX_PATH)

integration_test: build test
	$(HIT) init -l $(LOG_LEVEL) -d $(INDEX_PATH)
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) Makefile
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) dune-project
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) README.org
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) LICENSE
	$(HIT) import -l $(LOG_LEVEL) -d $(INDEX_PATH) -t ml lib
	@echo "--- collect garbage"
	$(HIT) gc -l $(LOG_LEVEL) -d $(INDEX_PATH)
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
	@echo "--- collect garbage"
	mkdir -p $(INDEX_PATH)/doc/00/00/00/00/000000000000000000000000
	touch $(INDEX_PATH)/doc/00/00/00/00/000000000000000000000000/content
	$(HIT) gc -l $(LOG_LEVEL) -d $(INDEX_PATH)
	@echo "--- search for: install or sexp"
	$(HIT) search -l $(LOG_LEVEL) -m -c 3 -d $(INDEX_PATH) install sexp
