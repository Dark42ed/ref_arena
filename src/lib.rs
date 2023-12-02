#![no_std]
//! An `no_std` (alloc) arena that acts similarly to a `Slab<Rc<T>>` but with
//! better performance and less features.
//!
//! This arena features constant time inserts, derefs,
//! and drops while allocating less often, decreasing
//! memory fragmentation, having increased performance
//! (depending on the allocator), and potentially using 
//! less memory.
//!
//! RcArena does not support Weak references and probably
//! will not in the indefinite future.
//!
//! This library uses a decent amount of unsafe code, and is
//! partially tested with miri. It should be safe since the code
//! isn't too complex, but beware of bugs.
//!
//! # Example
//!
//! ```
//! use rc_arena::{RcArena, RcRef};
//!
//! let mut arena: RcArena<i32> = RcArena::new();
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
//! std::rc::Rc   allocate 10_000  247.59 μs
//! RcArena       allocate 10_000  48.57 μs
//!
//! ~5x speedup
//! ```
//!
//! Dereferencing 10k `Rc`s:
//! ```text
//! std::rc::Rc   deref 10_000     4.97 μs
//! RcArena       deref 10_000     4.86 μs
//!
//! no speedup
//! ```
//!
//! Dereferencing should be about the same within both since
//! it's a simple pointer dereference. RcRef may have double pointer
//! indirection which will be looked into depending on how costly it is.
//!
//! Dropping 10k `Rc`s:
//! ```text
//! std::rc::Rc   drop 10_000      134.35 μs
//! RcArena       drop 10_000      29.06 μs
//!
//! ~4.62x speedup
//! ```

extern crate alloc;

use alloc::{
    boxed::Box,
    rc::{Rc, Weak},
    vec::Vec,
};
use core::hash::Hash;
use core::{
    cell::UnsafeCell,
    fmt::{Debug, Display},
    mem::MaybeUninit,
};

// Starts at 8 and doubles every time.
fn get_buffer_size(buffer_num: u32) -> usize {
    2_usize.pow(3 + buffer_num)
}

struct InnerArena<T> {
    // Keep weak reference to vacant so RcRefs can push
    // when dropped. Weak since if the RcArena is dropped
    // we won't be allocating any more.
    vacant: Weak<UnsafeCell<Vec<(usize, usize)>>>,
    // The index of the buffer in RcArena.inner
    // Used for inserting dropped indexes into vacant.
    buffer_index: usize,
    // Wish I could get rid of the arena since InnerArena is
    // already allocated in an Rc.
    arena: Box<[(UnsafeCell<MaybeUninit<T>>, UnsafeCell<usize>)]>,
}

/// An arena that holds reference counted values.
///
/// An RcArena essentially acts as a `Slab<Rc<T>>` but
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
/// Currently the stable references are not used due to limitations
/// with storing unsized objects within `Rc`s, but may be used in the
/// future for further performance improvements. Regardless, these
/// stable references have another benefit which is that existing memory
/// is never reallocated, instead more is allocated on top.
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
/// use rc_arena::{RcArena, RcRef};
///
/// let mut arena: RcArena<i32> = RcArena::new();
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
pub struct RcArena<T> {
    // Use MaybeUninit and track Someness with references
    // (0 references == None)
    // Using Option requires T to have clone in some cases.
    inner: Vec<Rc<InnerArena<T>>>,
    // Stores indexes for indexes previously used by an RcRef,
    // but since dropped and "removed".
    vacant: Rc<UnsafeCell<Vec<(usize, usize)>>>,
    // The "length" of the last allocated buffer. We use this
    // so we don't need to populate vacant when we allocate.
    last_len: usize,
}

impl<T> RcArena<T> {
    /// Creates a new `RcArena`
    pub fn new() -> RcArena<T> {
        RcArena {
            inner: Vec::new(),
            vacant: Rc::new(UnsafeCell::new(Vec::new())),
            last_len: 0,
        }
    }

    fn allocate_new_buffer(&mut self) {
        let size = get_buffer_size(self.inner.len() as u32);

        let mut v = Vec::with_capacity(size);
        v.resize_with(size, || {
            (UnsafeCell::new(MaybeUninit::uninit()), UnsafeCell::new(0))
        });

        let inner = InnerArena {
            vacant: Rc::downgrade(&self.vacant),
            buffer_index: self.inner.len(),
            arena: v.into_boxed_slice(),
        };

        self.inner.push(Rc::new(inner));
    }

    /// Inserts an item `T` into the `RcArena`, and returns an [`RcRef`] to it.
    ///
    /// # Example
    /// ```
    /// use rc_arena::{RcArena, RcRef};
    ///
    /// let mut arena: RcArena<i32> = RcArena::new();
    ///
    /// let rc: RcRef<i32> = arena.insert(5);
    /// assert_eq!(*rc, 5);
    /// ```
    pub fn insert(&mut self, item: T) -> RcRef<T> {
        let last_idx = self.inner.len().saturating_sub(1);
        let last = self.inner.get(last_idx);

        let (buffer_index, index) = match unsafe { &mut *self.vacant.get() }.pop() {
            // Found vacant spot!
            Some(vacant) => vacant,

            // No vacant spots found
            None => {
                match last {
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

        let buffer = &self.inner[buffer_index];
        unsafe {
            let cell = &buffer.arena[index];
            // Refcount is 1 (the ref we are about to return)
            (*cell.1.get()) = 1;
            (*cell.0.get()).write(item);
        }

        RcRef {
            buffer: buffer.clone(),
            index,
        }
    }
}

impl<T> Default for RcArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// A reference to an item within a [`RcArena`].
///
/// This reference has no lifetime and acts like a normal [`Rc`].
/// Internally this Rc is an index within a bigger buffer
/// in the RcArena.
///
/// Note that keeping an Rc held after the owning arena has
/// been dropped will cause the holding buffer to stay alive.
/// The holding buffer is an array of `T`'s, so keeping the
/// buffer alive means that lots of memory (depending on how
/// many object were in the arena) will not be freed until
/// the last RcRef in the buffer has been dropped.
///
///
pub struct RcRef<T> {
    buffer: Rc<InnerArena<T>>,
    index: usize,
}

impl<T> RcRef<T> {
    /// Gets the reference count.
    pub fn ref_count(&self) -> usize {
        unsafe { *self.get_inner().1.get() }
    }

    unsafe fn get_inner(&self) -> &(UnsafeCell<MaybeUninit<T>>, UnsafeCell<usize>) {
        unsafe { self.buffer.arena.get_unchecked(self.index) }
    }

    /// Gets the inner data and refcount immutably
    /// Safe since it gives an immut ref.
    fn get_data(&self) -> &MaybeUninit<T> {
        // Index is within buffer len, checked on creation
        unsafe { &*self.buffer.arena.get_unchecked(self.index).0.get() }
    }
}

impl<T> Clone for RcRef<T> {
    fn clone(&self) -> Self {
        // Should be safe since refcount is private and not mutably borrowed elsewhere
        let count = self.ref_count();
        // Refcount is initted on creation.
        // Increment refcount
        unsafe {
            (*self.get_inner().1.get()) = count + 1;
        }

        RcRef {
            buffer: self.buffer.clone(),
            index: self.index,
        }
    }
}

impl<T> core::ops::Drop for RcRef<T> {
    fn drop(&mut self) {
        // Data is unborrowed while dropping
        unsafe {
            let inner = self.get_inner();
            let refcount = &mut *inner.1.get();
            *refcount -= 1;

            if *refcount == 0 {
                // No other rcs are alive
                let data = &mut *inner.0.get();
                data.assume_init_drop();
                if let Some(vacant) = self.buffer.vacant.upgrade() {
                    (*vacant.get()).push((self.buffer.buffer_index, self.index));
                }
            }
        }
    }
}

impl<T> core::ops::Deref for RcRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Data is initted on creation
        unsafe { self.get_data().assume_init_ref() }
    }
}

impl<U, T: AsRef<U>> AsRef<U> for RcRef<T> {
    fn as_ref(&self) -> &U {
        (**self).as_ref()
    }
}

impl<T: Debug> Debug for RcRef<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(&**self, f)
    }
}

impl<T: Display> Display for RcRef<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Display::fmt(&**self, f)
    }
}

impl<T: PartialEq> PartialEq for RcRef<T> {
    fn eq(&self, other: &RcRef<T>) -> bool {
        (**self).eq(&**other)
    }
}

impl<T: Eq> Eq for RcRef<T> {}

impl<T: PartialOrd> PartialOrd for RcRef<T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: Ord> Ord for RcRef<T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: Hash> Hash for RcRef<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

#[cfg(test)]
mod test {
    use crate::{get_buffer_size, RcArena};
    use alloc::vec::Vec;

    #[test]
    fn initial_buffer_size() {
        assert_eq!(get_buffer_size(0), 8);
    }

    #[test]
    fn live_after() {
        let mut arena = RcArena::new();
        let rc = arena.insert(5);
        drop(arena);
        assert_eq!(*rc, 5);
        assert!(rc.buffer.vacant.upgrade().is_none());
    }

    #[test]
    fn buffer_allocate() {
        // first buffer size
        let to_allocate = get_buffer_size(0) + 1;

        let mut arena = RcArena::new();
        let mut rcs = Vec::new();

        for i in 0..to_allocate {
            rcs.push(arena.insert(i));
        }

        assert_eq!(arena.inner.len(), 2);
    }

    #[test]
    fn ref_clone() {
        let mut arena = RcArena::new();

        let rc = arena.insert(5);
        let rcclone = rc.clone();

        assert_eq!(*rc, 5);
        assert_eq!(*rcclone, 5);
        assert_eq!(rc.ref_count(), 2);
        drop(rc);
        assert_eq!(*rcclone, 5);
        assert_eq!(rcclone.ref_count(), 1);
        drop(rcclone);
    }
}
