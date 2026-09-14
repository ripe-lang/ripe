SCRIPTS := $(basename $(wildcard test_*.py))
TOOLS := test_gen_goldens test_no_regression
SWEEPS := $(filter-out $(TOOLS),$(SCRIPTS))

.PHONY: build all format check $(SCRIPTS)

build:
	dune build --root ../..

all: build
	@for s in $(SWEEPS); do echo "== $$s"; python3 $$s.py; done

format:
	ruff format .

check:
	ruff format --check .
	ruff check .

$(SCRIPTS): build
	python3 $@.py $(ARGS)
