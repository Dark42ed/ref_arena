#![no_std]
//! A `no_std` (alloc) arena that acts similarly to a `Slab<Rc<T>>` but with
//! better performance and less features.
//!
//! This arena stores reference counts with the objects in
//! a continuous buffer, which decreases the need for
//! multiple small memory allocations. References can
//! be cloned and act like normal `Rc`s. When all references
//! to an object is dropped, the space is made available
//! for a new object, and if the arena is dropped, the
//! underlying buffer may also be dropped.
//!
//! This arena features constant time inserts, derefs,
//! and drops while allocating less often, decreasing
//! memory fragmentation, having increased performance
//! (depending on the allocator), and potentially using
//! less memory.
//!
//! `RefArena` does not support Weak references and probably
//! will not in the indefinite future.
//!
//! This library uses a decent amount of unsafe code, and is
//! partially tested with miri. It should be safe since the code
//! isn't too complex, but beware of bugs.
//!
//! # Example
//!
//! ```
//! use ref_arena::{RefArena, RcRef};
//!
//! let mut arena: RefArena<i32> = RefArena::new();
//!
//! // Create a reference
//! let reference: RcRef<i32> = arena.insert(5);
//!
//! // Deref to get the inner value
//! let inner = *reference;
//! assert_eq!(inner, 5);
//!
//! // We can create clones of the reference just like an Rc!
//! let clone = reference.clone();
//! assert_eq!(*clone, 5);
//!
//! // References can be held even after the arena is dropped.
//! drop(arena);
//! assert_eq!(*reference, 5);
//!
//! // Objects (and internal buffers) are dropped when
//! // all references are dropped.
//! drop(reference);
//! drop(clone);
//! ```
//!
//! # Benchmarks
//!
//! Note: These benchmarks are very specific and
//! cater to the creation of lots of small objects.
//!
//! Additionally these benchmarks may vary
//! system-to-system and allocator-to-allocator.
//!
//! Allocating 10k `Rc`s:
//! ```text
//! std::rc::Rc   allocate 10_000  261.15 μs
//! RefArena      allocate 10_000  49.20 μs
//!
//! ~5x speedup
//! ```
//!
//! Dereferencing 10k `Rc`s:
//! ```text
//! std::rc::Rc   deref 10_000     6.43 μs
//! RefArena      deref 10_000     3.77 μs
//!
//! 2x speedup, most likely due to cache hits.
//! ```
//!
//! Dereferencing should be about the same within both since
//! it's a simple pointer dereference.
//!
//! Dropping 10k `Rc`s:
//! ```text
//! std::rc::Rc   drop 10_000      147.57 μs
//! RefArena      drop 10_000      22.79 μs
//!
//! ~6.47x speedup
//! ```
//!
//! Reallocating 10k `Rc`s:
//! ```text
//! RefArena      realloc 10_000   42.65 μs
//! ```
//!
//! In this case 10k `RcRef`s were allocated and dropped, and we measured
//! the time it took to put 10k objects back onto the arena.
//! (Compare to allocate)
//!
//! # Comparison to `rc_arena`
//!
//! [`rc_arena`](https://github.com/ebfull/rc_arena) is similar to `ref_arena`
//! in that they are arenas that return reference counted objects.
//! Both contain inner buffers that hold contiguous lists of objects.
//!
//! The main difference between the two is that `rc_arena` does not
//! individually count objects. When all references of an object are
//! dropped in `ref_arena`, the inner object is dropped and the space
//! is made available for a new insertion (similar to `slab` and
//! `stable-vec`), whereas in `rc_arena` the space is never made available again.
//!
//! `rc_arena` is useful if you have a determinite amount of objects
//! that need to be reference counted, where `ref_arena` is useful
//! when you frequently create and drop objects.
//!
//! Note this comparison might not be 100% correct as it's just
//! what I could tell from looking at the code and documentation.
//! Additionally this crate was not made with `rc_arena` in mind.
//!

extern crate alloc;

use alloc::{
    boxed::Box,
    rc::{Rc, Weak},
    vec::Vec,
};
use core::{
    cell::{Cell, UnsafeCell},
    fmt::{Debug, Display},
    mem::MaybeUninit,
    ptr::NonNull,
};
use core::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops,
};

// Starts at 8 and doubles every time.
fn get_buffer_size(buffer_num: u32) -> usize {
    2_usize.pow(3 + buffer_num)
}

struct InnerArena<T> {
    // Keep weak reference to vacant so RcRefs can push
    // when dropped. Weak since if the RefArena is dropped
    // we won't be allocating any more.
    vacant: Weak<UnsafeCell<Vec<(usize, usize)>>>,
    // The index of the buffer in RefArena.inner
    // Used for inserting dropped indexes into vacant.
    buffer_index: usize,
    // Wish I could get rid of the arena since InnerArena is
    // already allocated in an Rc.
    arena: Box<[UnsafeCell<MaybeUninit<RcItem<T>>>]>,
}

/// An arena that holds reference counted values.
///
/// A `RefArena` essentially acts as a `Slab<Rc<T>>` but
/// is much more faster and memory efficient since reference
/// counting is stored inline with the objects rather than
/// in separate allocations.
///
/// # Internals
///
/// Internally the objects are stored in buffers that get
/// bigger as you store more objects, similar to a `Vec`. The
/// key different here is that the `Vec`s are never reallocated,
/// which allows stable references to be held for the [`RcRef`]s
///
/// Buffers start at 8 objects and exponentially increase by
/// powers of two depending on how many objects are in the arena.
///
/// # Notes
///
/// Objects can't be removed or indexed once they are in the arena.
/// Instead, they are dropped when all references are dropped, similar
/// to an Rc, and are accessed through the reference.
///
/// # Example
///
/// ```
/// use ref_arena::{RefArena, RcRef};
///
/// let mut arena: RefArena<i32> = RefArena::new();
///
/// // Create a reference
/// let reference: RcRef<i32> = arena.insert(5);
///
/// // Deref to get the inner value
/// let inner = *reference;
/// assert_eq!(inner, 5);
///
/// // References can be held even after the arena is dropped.
/// drop(arena);
///
/// assert_eq!(*reference, 5);
///
/// // Objects (and internal buffers) are dropped when
/// // all references are dropped.
/// drop(reference);
/// ```
pub struct RefArena<T> {
    // Use MaybeUninit and track Someness with references
    // (0 references == None)
    // Using Option requires T to have clone in some cases.
    inner: Vec<Rc<InnerArena<T>>>,
    // Stores indexes for indexes previously used by an RcRef,
    // but since dropped and "removed".
    vacant: Rc<UnsafeCell<Vec<(usize, usize)>>>,
    // The "length" of the last allocated buffer.
    last_len: usize,
}

impl<T> RefArena<T> {
    /// Creates a new `RefArena`
    pub fn new() -> RefArena<T> {
        RefArena {
            inner: Vec::new(),
            vacant: Rc::new(UnsafeCell::new(Vec::new())),
            last_len: 0,
        }
    }

    #[allow(clippy::uninit_vec)]
    fn allocate_new_buffer(&mut self) {
        let size = get_buffer_size(self.inner.len() as u32);

        // Can never be false unless it overflows usize, at which point we're screwed anyways
        debug_assert!(size > 0);

        let mut v: Vec<UnsafeCell<MaybeUninit<RcItem<T>>>> = Vec::with_capacity(size);
        unsafe {
            // SAFETY: It's MaybeUninit
            v.set_len(size);
        }

        let inner = Rc::new(InnerArena {
            vacant: Rc::downgrade(&self.vacant),
            buffer_index: self.inner.len(),
            arena: v.into_boxed_slice(),
        });

        self.inner.push(inner);
    }

    /// Inserts an item `T` into the `RefArena`, and returns an [`RcRef`] to it.
    ///
    /// # Example
    /// ```
    /// use ref_arena::{RefArena, RcRef};
    ///
    /// let mut arena: RefArena<i32> = RefArena::new();
    ///
    /// let rc: RcRef<i32> = arena.insert(5);
    /// assert_eq!(*rc, 5);
    /// ```
    pub fn insert(&mut self, item: T) -> RcRef<T> {
        let (buffer_index, index) = match unsafe { &mut *self.vacant.get() }.pop() {
            // Found vacant spot!
            Some(vacant) => vacant,

            // No vacant spots found
            None => {
                match self.inner.last() {
                    Some(last) => {
                        // We have a buffer allocated
                        if self.last_len == last.arena.len() {
                            // Buffer is full, allocate a new one.
                            self.allocate_new_buffer();
                            self.last_len = 0;
                        }
                        // Insert into buffer now.
                        self.last_len += 1;

                        (self.inner.len() - 1, self.last_len - 1)
                    }
                    None => {
                        // Allocate initial buffer
                        self.allocate_new_buffer();
                        self.last_len += 1;

                        (0, 0)
                    }
                }
            }
        };

        let buffer = unsafe { self.inner.get_unchecked(buffer_index) };
        unsafe {
            Rc::increment_strong_count(Rc::as_ptr(buffer));
            let cell = buffer.arena.get_unchecked(index);
            (*cell.get()).write(RcItem {
                value: item,
                ref_count: Cell::new(1),
            });
        }

        RcRef {
            buffer: Rc::as_ptr(buffer),
            // Don't pass the UnsafeCell, just a mutable to the MaybeUninit.
            // We can do this since the RefArena won't access the UnsafeCell again
            // until it is deallocated.
            ptr: unsafe { NonNull::new_unchecked(buffer.arena.get_unchecked(index).get()) },
        }
    }
}

impl<T> Default for RefArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct RcItem<T> {
    value: T,
    ref_count: Cell<usize>,
}

/// A reference to an item within an [`RefArena`].
///
/// This reference has no lifetime and acts like a normal [`Rc`].
/// Internally this Rc is an index within a bigger buffer
/// in the `RefArena`.
///
/// Note that keeping an Rc held after the owning arena has
/// been dropped will cause the holding buffer to stay alive.
/// The holding buffer is an array of `T`'s, so keeping the
/// buffer alive means that lots of memory (depending on how
/// many objects are in the holding buffer) will not be freed
/// until the last `RcRef` in the buffer has been dropped.

pub struct RcRef<T> {
    buffer: *const InnerArena<T>,
    ptr: NonNull<MaybeUninit<RcItem<T>>>,
}

impl<T> RcRef<T> {
    /// Gets the reference count.
    pub fn ref_count(&self) -> usize {
        self.get_inner().ref_count.get()
    }

    fn get_inner(&self) -> &RcItem<T> {
        // Ptr is valid while inner buffer is valid, held by Rc.
        unsafe { self.ptr.as_ref().assume_init_ref() }
    }

    /// Gets the inner data and refcount immutably
    /// Safe since it gives an immut ref.
    fn get_data(&self) -> &T {
        &self.get_inner().value
    }
}

impl<T> Clone for RcRef<T> {
    #[inline]
    fn clone(&self) -> Self {
        // Should be safe since refcount is private and not mutably borrowed elsewhere
        let inner = self.get_inner();
        let count = inner.ref_count.get();
        inner.ref_count.set(count + 1);

        RcRef {
            buffer: self.buffer,
            ptr: self.ptr,
        }
    }
}

impl<T> ops::Drop for RcRef<T> {
    fn drop(&mut self) {
        // Data is unborrowed while dropping
        unsafe {
            let inner = self.get_inner();
            let new_count = inner.ref_count.get() - 1;
            inner.ref_count.set(new_count);

            if new_count == 0 {
                // No other rcs are alive
                (*self.ptr.as_ptr()).assume_init_drop();
                let buffer = &*self.buffer;
                if buffer.vacant.strong_count() != 0 {
                    // Calculate index from ptr offset, so Deref impl doesn't
                    // calculate offset and just derefs ptr directly.
                    let index = self
                        .ptr
                        .as_ptr()
                        .cast::<UnsafeCell<_>>()
                        .offset_from(buffer.arena.as_ptr());
                    let vacant = &mut *(*buffer.vacant.as_ptr()).get();
                    vacant.push((buffer.buffer_index, index as usize));
                }
                Rc::decrement_strong_count(self.buffer);
            }
        }
    }
}

impl<T> ops::Deref for RcRef<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        // Data is initted on creation
        self.get_data()
    }
}

impl<U, T: AsRef<U>> AsRef<U> for RcRef<T> {
    #[inline]
    fn as_ref(&self) -> &U {
        (**self).as_ref()
    }
}

impl<T: Debug> Debug for RcRef<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&**self, f)
    }
}

impl<T: Display> Display for RcRef<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&**self, f)
    }
}

impl<T: PartialEq> PartialEq for RcRef<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(&**other)
    }
}

impl<T: Eq> Eq for RcRef<T> {}

impl<T: PartialOrd> PartialOrd for RcRef<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: Ord> Ord for RcRef<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: Hash> Hash for RcRef<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

#[cfg(test)]
mod test {
    use crate::{get_buffer_size, RefArena};
    use alloc::vec::Vec;

    #[test]
    fn initial_buffer_size() {
        assert_eq!(get_buffer_size(0), 8);
    }

    #[test]
    fn live_after() {
        let mut arena = RefArena::new();
        let rc = arena.insert(5);
        drop(arena);
        assert_eq!(*rc, 5);
        // assert!(rc.buffer.vacant.upgrade().is_none());
    }

    #[test]
    fn buffer_allocate() {
        // first buffer size
        let to_allocate = get_buffer_size(0) + 1;

        let mut arena = RefArena::new();
        let mut rcs = Vec::new();

        for i in 0..to_allocate {
            rcs.push(arena.insert(i));
        }

        assert_eq!(arena.inner.len(), 2);
    }

    #[test]
    fn ref_clone() {
        let mut arena = RefArena::new();

        let rc = arena.insert(5);
        let rcclone = rc.clone();

        assert_eq!(*rc, 5);
        assert_eq!(*rcclone, 5);
        assert_eq!(rc.ref_count(), 2);
        drop(rc);
        assert_eq!(*rcclone, 5);
        assert_eq!(rcclone.ref_count(), 1);
        assert_eq!(unsafe { &*arena.vacant.get() }.len(), 0);
        drop(rcclone);
        assert_eq!(unsafe { &*arena.vacant.get() }.len(), 1);
    }

    #[test]
    fn cell_check() {
        let mut arena = RefArena::new();

        let rc = arena.insert(5);
        let r: &i32 = &rc;
        // Shouldn't cause UB, check with miri.
        let count = rc.ref_count();

        assert_eq!(*r, 5);
        assert_eq!(count, 1);
    }

    #[test]
    fn test_zst() {
        struct Z;

        let mut arena = RefArena::new();
        let r = arena.insert(Z);
        let _ = r.clone();
    }
}
