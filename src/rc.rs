
use std::ptr::NonNull;
use std::marker::PhantomData;
use std::alloc::Layout;
use std::cell::Cell;
use std::cmp::max;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::mem::ManuallyDrop;

// global collection of refcounts
thread_local! {
    static MAP: RefCell<HashMap<*const (), usize>>
        = RefCell::new(HashMap::new());
}

// A transmutable Rc type
#[repr(transparent)]
pub struct Rc<T: ?Sized> {
    ptr: NonNull<T>,
    phantom: PhantomData<T>,
}

impl<T: ?Sized> From<Box<T>> for Rc<T> {
    fn from(box_: Box<T>) -> Self {
        let ptr = NonNull::from(Box::leak(box_));
        if std::mem::size_of_val::<T>(unsafe { ptr.as_ref() }) > 0 {
            MAP.with(|map| {
                match map.borrow_mut().insert(ptr.as_ptr() as *const (), 1) {
                    Some(_) => unreachable!(),
                    None => {}
                }
            });
        }

        Rc{ptr: ptr, phantom: PhantomData}
    }
}

impl<T> Rc<T> {
    pub fn new(t: T) -> Self {
        Self::from(Box::new(t))
    }
}

impl<T: ?Sized> Clone for Rc<T> {
    fn clone(&self) -> Self {
        if std::mem::size_of_val::<T>(unsafe { self.ptr.as_ref() }) > 0 {
            MAP.with(|map| {
                match map.borrow_mut().entry(self.ptr.as_ptr() as *const ()) {
                    Entry::Occupied(mut entry) => {
                        let mut rc = *entry.get();
                        rc += 1;
                        *entry.get_mut() = rc;
                    }
                    Entry::Vacant(entry) => unreachable!(),
                }
            });
        }

        Rc{ptr: self.ptr, phantom: PhantomData}
    }
}

impl<T: ?Sized> Drop for Rc<T> {
    fn drop(&mut self) {
        let zero = if std::mem::size_of_val::<T>(unsafe { self.ptr.as_ref() }) > 0 {
            MAP.with(|map| {
                match map.borrow_mut().entry(self.ptr.as_ptr() as *const ()) {
                    Entry::Occupied(mut entry) => {
                        let mut rc = *entry.get();
                        rc -= 1;
                        if rc == 0 {
                            entry.remove();
                            true
                        } else {
                            *entry.get_mut() = rc;
                            false
                        }
                    }
                    Entry::Vacant(entry) => unreachable!(),
                }
            })
        } else {
            true
        };

        if zero {
            drop(unsafe { Box::from_raw(self.ptr.as_ptr()) });
        }
    }
}

impl<T: Clone> Rc<T> {
    pub fn try_unwrap(self) -> Result<T, Rc<T>> {
        let zero = if std::mem::size_of_val::<T>(unsafe { self.ptr.as_ref() }) > 0 {
            MAP.with(|map| {
                match map.borrow_mut().entry(self.ptr.as_ptr() as *const ()) {
                    Entry::Occupied(entry) => {
                        let mut rc = *entry.get();
                        rc -= 1;
                        if rc == 0 {
                            entry.remove();
                            true
                        } else {
                            false
                        }
                    }
                    Entry::Vacant(entry) => unreachable!(),
                }
            })
        } else {
            true
        };

        if zero {
            Ok(unsafe { std::ptr::read(ManuallyDrop::new(self).ptr.as_ptr()) })
        } else {
            Err(self)
        }
    }

    pub fn try_into(self) -> Option<T> {
        self.try_unwrap().ok()
    }

    pub fn into_clone(self) -> T {
        match self.try_unwrap() {
            Ok(t) => t,
            Err(self_) => self_.as_ref().clone(),
        }
    }
}



// to/from references
impl<T: ?Sized> AsRef<T> for Rc<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: ?Sized> Rc<T> {
    pub fn try_from(t: &T) -> Option<Rc<T>> {
        let found = MAP.with(|map| {
            match map.borrow_mut().entry(t as *const T as *const ()) {
                Entry::Occupied(mut entry) => {
                    let mut rc = *entry.get();
                    rc += 1;
                    *entry.get_mut() = rc;
                    true
                }
                Entry::Vacant(entry) => {
                    false
                }
            }
        });

        if found {
            Some(Rc{ptr: NonNull::from(t), phantom: PhantomData})
        } else {
            None
        }
    }
}

// other traits
impl<T: ?Sized> std::ops::Deref for Rc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: ?Sized> std::borrow::Borrow<T> for Rc<T> {
    fn borrow(&self) -> &T {
        self.as_ref()
    }
}

impl<T: ?Sized + std::fmt::Debug> std::fmt::Debug for Rc<T> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>
    ) -> Result<(), std::fmt::Error> {
        std::fmt::Debug::fmt(self.as_ref(), f)
    }

}

// other casts
impl<T> From<Vec<T>> for Rc<[T]> {
    fn from(v: Vec<T>) -> Self {
        Self::from(v.into_boxed_slice())
    }
}

// a marker trait for transmutation
pub unsafe trait Transmute<'a, T> {
    fn transmute(&'a self) -> &'a T {
        unsafe { &*(self as *const Self as *const T) }
    }
}

unsafe impl<'a, T: ?Sized> Transmute<'a, &'a T> for Rc<T> {}

