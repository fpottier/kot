# [make test] runs several tests in parallel.
# These tests do not terminate; they run forever.

.PHONY: test
test:
	@ make clean
	@ dune build @all
	@ parallel \
	  --tag --line-buffer \
	  make -C {} random_nodep \
	  ::: \
	  Buffer8 \
	  Deque \
	  DequeWF \
	  DequeBa \
	  DequeBa \
	  DequeBa \
	  DequeBa \
	  DequeBa \

# Disabled tests:
#	  Buffer3 \
#	  NCDeque \
#	  LBuffer \

# [make scenarios] assumes that [make test] has been run and that some crash
# scenarios have been found by [DequeBa]. It shows a list of these scenarios.

WC := $(shell if command -v gwc >/dev/null ; then echo gwc ; else echo wc ; fi)

.PHONY: scenarios
scenarios:
	@ $(WC) -l DequeBa/output/crashes/* --total=never | awk '{$$1=$$1};1' | sort
# awk suppresses leading spaces (of which gwc prints a variable number!)

# [make scenario] assumes that [make test] has been run and that some crash
# scenarios have been found by [DequeBa]. It shows the shortest such scenario.

.PHONY: scenario
scenario:
	@ more $$(make scenarios | head -1 | tr -s ' ' | cut -d ' ' -f 2)
# tr compresses multiple spaces into one, so cut works as desired

.PHONY: clean
clean:
	@ git clean -fdX
