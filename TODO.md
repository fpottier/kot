# To Do

## Performance

* Some operations on buffers (`push`, `inject`, `pop`, `eject`, etc.)
  are applied to buffers whose size is known. Specialize them?

* Conversely, it is possible that some of the specialized operations
  that we have introduced are used only in areas of the code that are
  very seldom executed. If so, remove them.

* Instead of an option on a reference,
  we could use an option with a mutable field,
  saving an indirection.

## Cleanup

* Share the test code for the three buffer implementations.

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
