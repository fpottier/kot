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

.PHONY: scenarios
scenarios:
	@ wc -l DequeBa/output/crashes/* | sort

# [make scenario] assumes that [make test] has been run and that some crash
# scenarios have been found by [DequeBa]. It shows the shortest such scenario.

.PHONY: scenario
scenario:
	@ more $$(wc -l DequeBa/output/crashes/* | sort | head -1 | tr -s ' ' | cut -d' ' -f 3)


.PHONY: clean
clean:
	@ git clean -fdX
