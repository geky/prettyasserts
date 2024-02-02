
use std::ptr::NonNull;
use std::marker::PhantomData;
use std::alloc::Layout;
use std::cell::Cell;
use std::cmp::max;
use std::mem::ManuallyDrop;
use std::mem::size_of;
use std::mem::align_of;
use std::mem::transmute;

struct RcBoxHeader<T: ?Sized> {
    rc: Cell<usize>,
    // we need this to preserve metadata for zero-sized types
    ptr: Option<NonNull<RcBox<T>>>,
}

struct RcBox<T: ?Sized>(RcBoxHeader<T>, T);

// A transmutable Rc type
#[repr(transparent)]
pub struct Rc<T: ?Sized> {
    ptr: NonNull<T>,
    phantom: PhantomData<T>,
}

impl<T: ?Sized> RcBoxHeader<T> {
    fn new() -> Self {
        RcBoxHeader{rc: Cell::new(1), ptr: None}
    }

    fn layout(t: &T) -> Layout {
        Layout::new::<RcBoxHeader<T>>()
            .extend(Layout::for_value(t))
            .unwrap()
            .0
    }

    fn offset(t: &T) -> usize {
        Layout::new::<RcBoxHeader<T>>()
            .extend(Layout::for_value(t))
            .unwrap()
            .1
    }
}

impl<T: ?Sized> Rc<T> {
    fn rcbox(&self) -> &RcBox<T> {
        unsafe {
            ((self.ptr.as_ptr() as *const u8).sub(
                RcBoxHeader::offset(self.ptr.as_ref())
            ) as *const RcBoxHeader<T>)
                .as_ref().unwrap()
                .ptr.unwrap().as_ref()
        }
    }

    fn rc(&self) -> &Cell<usize> {
        &self.rcbox().0.rc
    }

    fn from_rcbox(mut rcbox: Box<RcBox<T>>) -> Self {
        rcbox.0.rc = Cell::new(1);
        rcbox.0.ptr = Some(NonNull::from(rcbox.as_ref()));
        let ptr = NonNull::from(&Box::leak(rcbox).1);
        Rc{ptr: ptr, phantom: PhantomData}
    }

    unsafe fn into_rcbox(&self) -> Box<RcBox<T>> {
        Box::from_raw(self.rcbox() as *const RcBox<T> as *mut RcBox<T>)
    }
}

impl<T> Rc<T> {
    pub fn new(t: T) -> Self {
        let rcbox = Box::new(RcBox(RcBoxHeader::new(), t));
        Self::from_rcbox(rcbox)
    }
}

impl<T: ?Sized> Clone for Rc<T> {
    fn clone(&self) -> Self {
        let rc = self.rc().get();
        self.rc().set(rc + 1);

        Rc{ptr: self.ptr, phantom: PhantomData}
    }
}

impl<T: ?Sized> Drop for Rc<T> {
    fn drop(&mut self) {
        let rc = self.rc().get();
        if rc == 1 {
            //drop(unsafe { self.into_rcbox() });
        } else {
            self.rc().set(rc - 1);
        }
    }
}

impl<T: Clone> Rc<T> {
    pub fn try_unwrap(self) -> Result<T, Rc<T>> {
        let rc = self.rc().get();
        if rc == 1 {
            unsafe {
                let mut rcbox = transmute::<
                    Box<RcBox<T>>,
                    Box<RcBox<ManuallyDrop<T>>>
                >(
                    self.into_rcbox()
                );
                Ok(ManuallyDrop::take(&mut rcbox.1))
            }
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



// traits
impl<T: ?Sized> AsRef<T> for Rc<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

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

impl<T: ?Sized + Default> Default for Rc<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

// other casts
impl<T> Rc<[T]> {
    unsafe fn from_slice(src: &mut [ManuallyDrop<T>]) -> Rc<[T]> {
        let raw = std::alloc::alloc(RcBoxHeader::layout(src));
        let hdr = raw as *mut RcBoxHeader<[T]>;
        hdr.write(RcBoxHeader::new());

        let dst = raw.add(RcBoxHeader::offset(src)) as *mut T;
        for i in 0..src.len() {
            dst.write(ManuallyDrop::take(&mut src[i]));
        }

        let dst = std::slice::from_raw_parts_mut(dst, src.len());
        Rc{ptr: NonNull::from(dst), phantom: PhantomData}
    }
}

impl<T> From<Box<[T]>> for Rc<[T]> {
    fn from(b: Box<[T]>) -> Self {
        unsafe {
            let mut b = transmute::<Box<[T]>, Box<[ManuallyDrop<T>]>>(b);
            Self::from_slice(&mut b)
        }
    }
}

impl<T> From<Vec<T>> for Rc<[T]> {
    fn from(v: Vec<T>) -> Self {
        unsafe {
            let mut v = transmute::<Vec<T>, Vec<ManuallyDrop<T>>>(v);
            Self::from_slice(&mut v)
        }
    }
}

impl<T> From<&[T]> for Rc<[T]>
where
    T: Clone
{
    fn from(v: &[T]) -> Self {
        Self::from(Vec::from(v))
    }
}


// a marker trait for transmutation
pub unsafe trait Transmute<T>
where
    Self: Sized
{
    fn transmute(self) -> T {
        debug_assert!(size_of::<Self>() == size_of::<T>());
        debug_assert!(align_of::<Self>() == align_of::<T>());
        unsafe {
            std::ptr::read(
                &ManuallyDrop::new(self)
                    as *const ManuallyDrop<Self>
                    as *const T
            )
        }
    }
}

unsafe impl<'a, T> Transmute<&'a T> for Rc<T> {}

// basically a better borrow trait
pub trait Transborrow<'a, T> {
    fn borrow(&'a self) -> T;
}

impl<'a, S, T> Transborrow<'a, &'a T> for S
where
    S: Transmute<T>
{
    fn borrow(&'a self) -> &'a T {
        debug_assert!(size_of::<Self>() == size_of::<T>());
        debug_assert!(align_of::<Self>() == align_of::<T>());
        unsafe { &*(self as *const Self as *const T) }
    }
}

