#![no_std]

extern crate alloc;

use core::{cell::{UnsafeCell, RefCell}, mem::MaybeUninit};
use alloc::{vec::Vec, rc::{Rc, Weak}, boxed::Box};
use alloc::vec;


fn get_buffer_size(buffer_num: u32) -> usize {
    2_usize.pow(3 + buffer_num)
}

pub struct InnerArena<T> {
    vacant: Weak<UnsafeCell<Vec<(usize, usize)>>>,
    buffer_index: usize,
    arena: Box<[(UnsafeCell<MaybeUninit<T>>, UnsafeCell<usize>)]>,
}


pub struct RcArena<T> {
    // Use MaybeUninit and track Someness with references
    // (0 references == None)
    // Using Option requires T to have clone in some cases.
    inner: Vec<Rc<InnerArena<T>>>,
    vacant: Rc<UnsafeCell<Vec<(usize, usize)>>>,
    last_len: usize
}

impl<T> RcArena<T> {
    pub fn new() -> RcArena<T> {
        RcArena {
            inner: Vec::new(),
            vacant: Rc::new(UnsafeCell::new(Vec::new())),
            last_len: 0
        }
    }

    fn allocate_new_buffer(&mut self) {
        let size = get_buffer_size(self.inner.len() as u32);
        /*
        let rc = Rc::<[MaybeUninit<(T, usize)>]>::new_uninit_slice(size);
        let transmuted = unsafe { core::mem::transmute::<_, Rc<[(UnsafeCell<MaybeUninit<T>>, UnsafeCell<MaybeUninit<usize>>)]>>(rc) };
        self.inner.push(
            transmuted
        );*/

        let mut v = Vec::with_capacity(size);
        v.resize_with(size, || (UnsafeCell::new(MaybeUninit::uninit()), UnsafeCell::new(0)));

        let inner = InnerArena {
            vacant: Rc::downgrade(&self.vacant),
            buffer_index: self.inner.len(),
            arena: v.into_boxed_slice()
        };

        self.inner.push(Rc::new(inner));
    }

    pub fn insert(&mut self, item: T) -> RcRef<T> {
        let last_idx = self.inner.len().saturating_sub(1);
        let last = self.inner.get(last_idx);

        let (buffer_index, index) = match unsafe { &mut *self.vacant.get() }.pop() {
            Some(vacant) => vacant,
            None => {
                match last {
                    Some(last) => {
                        if self.last_len == last.arena.len() {
                            // Buffer is full, allocate a new one.
                            self.allocate_new_buffer();
                            self.last_len = 0;
                        }
                        // About to be inserted to
                        self.last_len += 1;

                        (self.inner.len() - 1, self.last_len - 1)
                    },
                    None => {
                        // Allocate initial buffer
                        self.allocate_new_buffer();

                        // About to be inserted to
                        self.last_len += 1;

                        (0, 0)
                    }
                }
            }
        };

        let buffer = &self.inner[buffer_index];
        unsafe {
            let cell = &buffer.arena[index];
            (*cell.1.get()) = 1;
            (*cell.0.get()).write(item);            
        }

        RcRef {
            buffer: buffer.clone(),
            index,
        }
    }
}

pub struct RcRef<T> {
    buffer: Rc<InnerArena<T>>,
    index: usize,
}

impl<T> RcRef<T> {
    /// Gets the refcount.
    pub fn ref_count(&self) -> usize {
        unsafe { *self.get_inner().1.get() }
    }

    unsafe fn get_inner(&self) -> &(UnsafeCell<MaybeUninit<T>>, UnsafeCell<usize>) {
        unsafe { & (*self.buffer.arena.get_unchecked(self.index)) }
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
        unsafe { (*self.get_inner().1.get()) = count + 1; }

        RcRef {
            buffer: self.buffer.clone(),
            index: self.index,
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