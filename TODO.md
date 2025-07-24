# To Do

## Performance

* To reduce allocation and make the code more efficient,
  we should avoid using sequences of small operations on buffers
  (`B.push`, `B.pop`, `B.inject`, `B.eject`)
  and introduce composite operations on buffers.

* Once the above point has been addressed,
  perform benchmarks,
  using various implementations of buffers.
  Compare `Buffer8` versus `LinkedBuffer`.

* Experiment with array-based versions of `Buffer`.
  E.g., immutable arrays (simplest option).
  Or, mutable chunks in the style of `Sek`.

* Instead of an option on a reference,
  we could use an option with a mutable field,
  saving an indirection.

## Cleanup

* Make `Deque` a functor over `Buffer`.

## Tests

* In each buffer and deque data structure,
  test `map`, `fold_left`, `fold_right`.

* Test concurrent accesses, *without* an atomic field.
