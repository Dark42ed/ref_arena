use core::{cell::UnsafeCell, marker::PhantomData, mem::MaybeUninit, ops::Deref};

use alloc::{boxed::Box, vec::Vec};

use crate::{get_buffer_size, RcItem};

pub struct BoundedInnerArena<T> {
    arena: Box<[RcItem<T>]>,
    buffer_index: usize,
    vacant: *const UnsafeCell<Vec<(usize, usize)>>
}

pub struct BoundedRefArenaHolder<T> {
    inner: Vec<Box<BoundedInnerArena<T>>>,
    vacant: Box<UnsafeCell<Vec<(usize, usize)>>>,
    last_len: usize,
}

impl<T> BoundedRefArenaHolder<T> {
    pub fn new() -> BoundedRefArenaHolder<T> {
        BoundedRefArenaHolder {
            inner: Vec::new(),
            vacant: Box::new(UnsafeCell::new(Vec::new())),
            last_len: 0
        }
    }

    fn allocate_new_buffer(&mut self) {
        let size = get_buffer_size(self.inner.len() as u32);

        let mut v = Vec::<RcItem<T>>::with_capacity(size);
        v.resize_with(size, || MaybeUninit::uninit());

        let inner = BoundedInnerArena {
            vacant: &*self.vacant as *const _ as *mut _,
            buffer_index: self.inner.len(),
            arena: v.into_boxed_slice(),
        };

        self.inner.push(Box::new(inner));
    }

    pub fn insert(&mut self, item: T) -> BoundedRcRef<'_, T> {
        // Insert must be &self since RcRef borrows it

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

        unsafe {
            let buffer = self.inner.get_unchecked(buffer_index);
            let cell = buffer.arena.get_unchecked(index);
            cell.as_ptr().cast_mut().write(
                (UnsafeCell::new(item), UnsafeCell::new(1))
            );

            BoundedRcRef {
                ptr: cell as *const RcItem<T>,
                inner_arena: &**buffer as *const BoundedInnerArena<T>,
                _phantom: PhantomData
            }
        }
    }
}

pub struct BoundedRefArena<T> {
    inner: UnsafeCell<BoundedRefArenaHolder<T>>
}

impl<T> BoundedRefArena<T> {
    pub fn new() -> BoundedRefArena<T> {
        BoundedRefArena {
            inner: UnsafeCell::new(BoundedRefArenaHolder::new())
        }
    }

    pub fn insert<'this>(&'this self, item: T) -> BoundedRcRef<'this, T> {
        let rcref = unsafe { &mut *self.inner.get() }.insert(item);
        unsafe { core::mem::transmute::<_, BoundedRcRef<'this, T>>(rcref) }
    }
}

pub struct BoundedRcRef<'b, T> {
    ptr: *const RcItem<T>,
    inner_arena: *const BoundedInnerArena<T>,
    _phantom: PhantomData<&'b BoundedRefArena<T>>
}

impl<'b, T> BoundedRcRef<'b, T> {
    fn get_inner(&self) -> &(UnsafeCell<T>, UnsafeCell<usize>) {
        unsafe { (*self.ptr).assume_init_ref() }
    }

    pub fn ref_count(&self) -> usize {
        unsafe { *self.get_inner().1.get() }
    }

}

impl<'b, T> Deref for BoundedRcRef<'b, T> {
    type Target = T;
    
    fn deref(&self) -> &Self::Target {
        unsafe {
            &*(*self.ptr).assume_init_ref().0.get()
        }
    }
}

impl<'b, T> Clone for BoundedRcRef<'b, T> {
    fn clone(&self) -> Self {
        unsafe { 
            *self.get_inner().1.get() += 1; 
        }

        BoundedRcRef {
            ptr: self.ptr,
            inner_arena: self.inner_arena,
            _phantom: PhantomData
        }
    }
}

impl<'b, T> Drop for BoundedRcRef<'b, T> {
    fn drop(&mut self) {
        unsafe {
            let inner = self.get_inner();
    
            (*inner.1.get()) -= 1;

            if (*inner.1.get()) == 0 {
                // Safe to cast mut since its the only reference left.
                (*self.ptr.cast_mut()).assume_init_drop();
                
                let vacant = &mut (*(*(*self.inner_arena).vacant).get());
                // Calculate index from ptr offset, so Deref impl doesn't
                // calculate offset and just derefs ptr directly.
                let index = self.ptr.offset_from((*self.inner_arena).arena.as_ptr());
                vacant.push(((*self.inner_arena).buffer_index, index as usize));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ref_clone() {
        let arena = BoundedRefArena::new();

        let rc = arena.insert(5);
        let rcclone = rc.clone();

        assert_eq!(*rc, 5);
        assert_eq!(*rcclone, 5);
        assert_eq!(rc.ref_count(), 2);
        drop(rc);
        assert_eq!(*rcclone, 5);
        assert_eq!(rcclone.ref_count(), 1);
        assert_eq!(unsafe { (*(*arena.inner.get()).vacant.get()).len() }, 0);
        drop(rcclone);
        assert_eq!(unsafe { (*(*arena.inner.get()).vacant.get()).len() }, 1);
    }

    #[test]
    fn insert() {
        let arena = BoundedRefArena::new();

        let first = arena.insert(5);

    }
}