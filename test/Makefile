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
	  Buffer3 \
	  NCDeque \
	  Buffer8 \
	  Deque \

.PHONY: clean
clean:
	@ git clean -fdX
