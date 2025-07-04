# To Do

* Make `Catdeque` a functor over `Buffer`,
  and instantiate it with bounded buffers of capacity at most 8.
  Test `Buffer8` independently.

* Instead of an option on a reference,
  we could use an option with a mutable field,
  saving an indirection.

* A thread-safe version of the library requires an atomic field.
  Benchmark.
