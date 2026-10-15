SCRIPTS := $(basename $(wildcard test_*.py))
TOOLS := test_gen_goldens test_no_regression test_cluster_failures
SWEEPS := $(filter-out $(TOOLS),$(SCRIPTS))

.PHONY: build all format check $(SCRIPTS)

build:
	dune build --root ../..

all: build
	@for s in $(SWEEPS); do echo "== $$s"; python3 $$s.py; done
	@echo "== test_cluster_failures"; python3 test_cluster_failures.py

format:
	ruff format .

check:
	ruff format --check .
	ruff check .

$(SCRIPTS): build
	python3 $@.py $(ARGS)
