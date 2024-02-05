use core::{cell::UnsafeCell, marker::PhantomData, mem::MaybeUninit, ops::Deref};

use alloc::{boxed::Box, vec::Vec};

use crate::{get_buffer_size, RcItem};

pub struct BoundedInnerArena<T> {
    // BoundedInnerArena is only used from const references.

    // We need to mutate RcItems when adding and dropping.
    arena: Box<[UnsafeCell<RcItem<T>>]>,
    buffer_index: usize,

    vacant: *const UnsafeCell<Vec<(usize, usize)>>
}

pub struct BoundedRefArena<T> {
    inner: Vec<Box<BoundedInnerArena<T>>>,
    vacant: Box<UnsafeCell<Vec<(usize, usize)>>>,
    last_len: usize,
}

impl<T> BoundedRefArena<T> {
    pub fn new() -> BoundedRefArena<T> {
        BoundedRefArena {
            inner: Vec::new(),
            vacant: Box::new(UnsafeCell::new(Vec::new())),
            last_len: 0
        }
    }

    fn allocate_new_buffer(&mut self) {
        let size = get_buffer_size(self.inner.len() as u32);

        let mut v = Vec::<UnsafeCell<RcItem<T>>>::with_capacity(size);
        v.resize_with(size, || UnsafeCell::new(MaybeUninit::uninit()));

        let inner = BoundedInnerArena {
            vacant: &(*self.vacant) as *const _,
            buffer_index: self.inner.len(),
            arena: v.into_boxed_slice(),
        };

        self.inner.push(Box::new(inner));
    }

    /// SAFETY: BoundedRcRef must be dropped before BoundedRefArena.
    pub unsafe fn insert(&mut self, item: T) -> BoundedRcRef<T> {
        // Insert must be &self since RcRef borrows it

        // Safety: vacant isn't used while it's being held.
        let (buffer_index, index) = match self.vacant.get_mut().pop() {
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

        unsafe {
            let buffer = self.inner.get_unchecked(buffer_index);
            let cell = buffer.arena.get_unchecked(index);
            (*cell.get()).write(
                (UnsafeCell::new(item), UnsafeCell::new(1))
            );

            BoundedRcRef {
                ptr: cell.get() as *const RcItem<T>,
                inner_arena: &**buffer as *const BoundedInnerArena<T>,
            }
        }
    }
}

pub struct BoundedRcRef<T> {
    ptr: *const RcItem<T>,
    inner_arena: *const BoundedInnerArena<T>,
}

impl<T> BoundedRcRef<T> {
    fn get_inner(&self) -> &(UnsafeCell<T>, UnsafeCell<usize>) {
        unsafe { (*self.ptr).assume_init_ref() }
    }

    pub fn ref_count(&self) -> usize {
        unsafe { *self.get_inner().1.get() }
    }

}

impl<T> Deref for BoundedRcRef<T> {
    type Target = T;
    
    fn deref(&self) -> &Self::Target {
        unsafe {
            &*(*self.ptr).assume_init_ref().0.get()
        }
    }
}

impl<T> Clone for BoundedRcRef<T> {
    fn clone(&self) -> Self {
        unsafe { 
            *self.get_inner().1.get() += 1; 
        }

        BoundedRcRef {
            ptr: self.ptr,
            inner_arena: self.inner_arena,
        }
    }
}

impl<T> Drop for BoundedRcRef<T> {
    fn drop(&mut self) {
        unsafe {
            let inner = self.get_inner();
    
            (*inner.1.get()) -= 1;

            if (*inner.1.get()) == 0 {
                // SAFETY: pretty sure its safe, not too sure what to prove
                core::ptr::drop_in_place(
                    (*self.ptr).assume_init_ref().0.get()
                );
                
                // ALIAS SAFETY: Vacant is dropped before end of scope.
                let vacant = &mut (*(*(*self.inner_arena).vacant).get());

                // Calculate index from ptr offset, so Deref impl doesn't
                // calculate offset and just derefs ptr directly.
                let index = self.ptr.offset_from((*self.inner_arena).arena.as_ptr().cast());
                vacant.push(((*self.inner_arena).buffer_index, index as usize));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ref_ptrs() {
        let mut arena = BoundedRefArena::new();

        let rc = unsafe { arena.insert(5) };

        assert_eq!(rc.ptr, arena.inner[0].arena[0].get().cast_const());
        assert_eq!(*rc, 5);
        assert_eq!(rc.ref_count(), 1);

        drop(rc);

        assert_eq!(arena.vacant.get_mut(), &[(0, 0)]);
    }

    #[test]
    fn insert() {
        // let arena = BoundedRefArena::new();

        // let first = arena.insert(5);

    }
}