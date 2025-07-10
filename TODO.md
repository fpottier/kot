# To Do

* Make `Deque` a functor over `Buffer`,
  and instantiate it with bounded buffers of capacity at most 8.

* To reduce allocation and make the code more efficient,
  we should avoid using sequences of small operations on buffers
  (`B.push`, `B.pop`, `B.inject`, `B.eject`)
  and introduce composite operations on buffers.

* We could potentially distinguish several distinct types of buffers
  (prefix, middle, suffix), each of which is subject to its own size
  constraints; e.g., a middle buffer has 0 or 2 elements. This would
  allow a slight efficiency gain.

* Instead of an option on a reference,
  we could use an option with a mutable field,
  saving an indirection.

* Test concurrent accesses, *without* an atomic field.

* Extend the data structures with a `length` field so that
  `length` runs in constant time.

* Test `map`, `fold_left`, `fold_right`, etc.

* Experiment with array-based versions of `Buffer`.
  E.g., immutable arrays (simplest option).
  Or, mutable chunks in the style of `Sek`.
