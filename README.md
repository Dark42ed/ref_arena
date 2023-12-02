# ref_arena

A no_std (alloc) arena that acts similarly to a `Slab<Rc<T>>` but with
better performance and less features.

This arena stores reference counts with the objects in
a continuous buffer, which decreases the need for
multiple small memory allocations. References can
be cloned and act like normal `Rc`s. When all references
to an object is dropped, the space is made available
for a new object, and if the arena is dropped, the
underlying buffer may also be dropped.

This arena features constant time inserts, derefs,
and drops while allocating less often, decreasing
memory fragmentation, having increased performance
(depending on the allocator), and potentially using
less memory.

RefArena does not support Weak references and probably
will not in the indefinite future.

This library uses a decent amount of unsafe code, and is
partially tested with miri. It should be safe since the code
isn't too complex, but beware of bugs.

## Example

```rust
use RefArena::{RefArena, RcRef};

let mut arena: RefArena<i32> = RefArena::new();

// Create a reference
let reference: RcRef<i32> = arena.insert(5);

// Deref to get the inner value
let inner = *reference;
assert_eq!(inner, 5);

// We can create clones of the reference just like an Rc!
let clone = reference.clone();
assert_eq!(*clone, 5);

// References can be held even after the arena is dropped.
drop(arena);
assert_eq!(*reference, 5);

// Objects (and internal buffers) are dropped when
// all references are dropped.
drop(reference);
drop(clone);
```

## Benchmarks

Note: These benchmarks are very specific and
cater to the creation of lots of small objects.

Additionally these benchmarks may vary
system-to-system and allocator-to-allocator.

Allocating 10k `Rc`s:
```
std::rc::Rc   allocate 10_000  247.59 μs
RefArena      allocate 10_000  48.57 μs

~5x speedup
```

Dereferencing 10k `Rc`s:
```
std::rc::Rc   deref 10_000     4.97 μs
RefArena      deref 10_000     4.86 μs

no speedup
```

Dereferencing should be about the same within both since
it's a simple pointer dereference. RcRef may have double pointer
indirection which will be looked into depending on how costly it is.

Dropping 10k `Rc`s:
```
std::rc::Rc   drop 10_000      134.35 μs
RefArena      drop 10_000      29.06 μs

~4.62x speedup
```

Reallocating 10k `Rc`s:
```
RefArena      realloc 10_000   45.62 μs
```

In this case 10k RcRefs were allocated and dropped, and we measured
the time it took to put 10k objects back onto the arena.
(Compare to allocate)

## Comparison to `rc_arena`

[`rc_arena`](https://github.com/ebfull/rc_arena) is similar to `ref_arena`
in that they are arenas that return reference counted objects.
Both contain inner buffers that hold contiguous lists of objects.

The main difference between the two is that rc_arena does not
individually count objects. When all references of an object are
dropped in ref_arena, the inner object is dropped and the space
is made available for a new insertion (similar to `slab` and
`stable-vec`), whereas in rc_arena the space is never made available again.

rc_arena is useful if you have a determinite amount of objects
that need to be reference counted, where ref_arena is useful
when you frequently create and drop objects.

Note this comparison might not be 100% correct as it's just
what I could tell from looking at the code and documentation.
Additionally this crate was not made with rc_arena in mind.


License: MIT
