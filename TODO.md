# To Do

## Performance

* To reduce allocation and make the code more efficient,
  we should avoid using sequences of small operations on buffers
  (`B.push`, `B.pop`, `B.inject`, `B.eject`)
  and introduce composite operations on buffers.
  See `pop2` and `eject2` in `Buffer3.mli`.

* Once the above point has been addressed,
  perform benchmarks,
  using various implementations of buffers.
  Compare `Buffer8` versus `LinkedBuffer`.
  In `LinkedBuffer`, a `length` field could be used
  to obtain constant-time `length`.

* Experiment with array-based versions of `Buffer`.
  E.g., immutable arrays (simplest option).
  Or, mutable chunks in the style of `Sek`.

* Instead of an option on a reference,
  we could use an option with a mutable field,
  saving an indirection.

## Cleanup

* Share the test code for the three buffer implementations.

* Make `Deque` a functor over `Buffer`.

* Add comments in every signature.

* Document.

## Tests

* Use the scenarios in `attic/scenarios`
  as repeatable test cases.

* In each buffer and deque data structure,
  test `map`, `fold_left`, `fold_right`.

* Test concurrent accesses, *without* an atomic field.

## Future features

* Add a `length` field to the data structure
  so that the operation `length` has constant time complexity.

* Offer random access functions (`get`, `set`)
  with logarithmic time complexity.
