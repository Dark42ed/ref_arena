# rc_arena

An arena that acts similarly to a `Slab<Rc<T>>` but with
better performance and less features.

This arena features constant time inserts, derefs,
and drops while allocating less often, decreasing
memory fragmentation, having increased performance,
and (potentially) using less memory.

RcArena does not support Weak references and probably
will not in the indefinite future.

This library uses a decent amount of unsafe code, and is
mostly tested with miri. It should be safe since the code
isn't too complex, but beware of bugs.

## Example

```rust
use rc_arena::{RcArena, RcRef};

let mut arena: RcArena<i32> = RcArena::new();

// Create a reference
let reference: RcRef<i32> = arena.insert(5);

// Deref to get the inner value
let inner = *reference;
assert_eq!(inner, 5);

// References can be held even after the arena is dropped.
drop(arena);

assert_eq!(*reference, 5);

// Objects (and internal buffers) are dropped when
// all references are dropped.
drop(reference);
```

## Benchmarks

Note: These benchmarks are very specific and
cater to the creation of lots of small objects.

Additionally these benchmarks may vary
system-to-system and allocator-to-allocator.

Allocating 10k `Rc`s:
```rust
std::rc::Rc   allocate 10_000  247.59 μs
RcArena       allocate 10_000  48.57 μs

~5x speedup
```

Dereferencing 10k `Rc`s:
```rust
std::rc::Rc   deref 10_000     4.97 μs
RcArena       deref 10_000     4.86 μs

no speedup
```

Dereferencing should be about the same within both since
it's a simple pointer dereference. RcRef may have double pointer
indirection which will be looked into depending on how costly it is.

Dropping 10k `Rc`s:
```rust
std::rc::Rc   drop 10_000      134.35 μs
RcArena       drop 10_000      29.06 μs

~4.62x speedup
```

