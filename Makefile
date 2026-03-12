BUILD_PATH=_build
INDEX_PATH=$(BUILD_PATH)/test_index
BENCHMARK_INDEX_PATH=$(BUILD_PATH)/test_index_benchmark
HIT=$(BUILD_PATH)/install/default/bin/hit
HIT_BENCHMARK=$(BUILD_PATH)/install/default/bin/hit-benchmark
LOG_LEVEL=info

all: integration_test doc-test benchmark-quick

unittest:
	dune test

build:
	dune build

doc:
	dune build @doc

install: unittest build
	dune install

format:
	dune fmt

clean:
	dune clean
	rm -rf my_index*
	rm -rf hit_benchmark_index*
	rm -f example.index-api.utop

doc-test: install
	./org-babel-tangle.sh README.org
	utop example.index-api.utop

benchmark: build
	$(HIT_BENCHMARK)

benchmark-quick: build
	$(HIT_BENCHMARK) -quota 1s

integration_test: build unittest
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) Makefile
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) dune-project
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) README.org
	$(HIT) add -l $(LOG_LEVEL) -d $(INDEX_PATH) LICENSE
	@echo "--- search for: content and license"
	$(HIT) query -l $(LOG_LEVEL) -d $(INDEX_PATH) '(and (eq content) (eq license))'
	@echo "--- search for: content or license"
	$(HIT) query -l $(LOG_LEVEL) -d $(INDEX_PATH) '(or (eq content) (eq license))'
	@echo "--- search for: hit"
	echo "IDs found: $$IDS"; \
	for id in $$IDS; do \
	  echo "Delete document with ID: $$id"; \
	  $(HIT) delete -l $(LOG_LEVEL) -d $(INDEX_PATH) $$id; \
	done
