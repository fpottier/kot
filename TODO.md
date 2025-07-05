# To Do

* Make `Catdeque` a functor over `Buffer`,
  and instantiate it with bounded buffers of capacity at most 8.
  Test `Buffer8` independently.

* If we are interested in making the code more efficient,
  then we should avoid using sequences of `B.push` and `B.pop`
  and introduce more operations on buffers.

* We could potentially distinguish several distinct types of buffers
  (prefix, middle, suffix), each of which is subject to its own size
  constraints; e.g., a middle buffer has 0 or 2 elements. This would
  allow a slight efficiency gain.

* Instead of an option on a reference,
  we could use an option with a mutable field,
  saving an indirection.

* A thread-safe version of the library requires an atomic field.
  Benchmark.
