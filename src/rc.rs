
use std::ptr::NonNull;
use std::marker::PhantomData;
use std::alloc::Layout;
use std::cell::Cell;
use std::cmp::max;
use std::mem::ManuallyDrop;
use std::mem::MaybeUninit;
use std::mem::size_of;
use std::mem::align_of;
use std::mem::transmute;

struct RcHeader<T: ?Sized> {
    rc: Cell<usize>,
    // we need this to preserve metadata for zero-sized types
    ptr: NonNull<T>,
}

// A transmutable Rc type
#[repr(transparent)]
pub struct Rc<T: ?Sized> {
    ptr: NonNull<T>,
    phantom: PhantomData<T>,
}

impl<T: ?Sized> RcHeader<T> {
    fn new(ptr: NonNull<T>) -> Self {
        RcHeader{rc: Cell::new(1), ptr: ptr}
    }

    fn layout(t: &T) -> Layout {
        Layout::new::<RcHeader<T>>()
            .extend(Layout::for_value(t))
            .unwrap()
            .0
    }

    fn offset(t: &T) -> usize {
        Layout::new::<RcHeader<T>>()
            .extend(Layout::for_value(t))
            .unwrap()
            .1
    }

    unsafe fn header(t: &T) -> &RcHeader<T> {
        ((t as *const T as *const u8)
            .sub(RcHeader::offset(t))
            as *const RcHeader<T>)
            .as_ref().unwrap()
    }

    fn alloc<'a, 'b>(t: &T) -> &'a mut MaybeUninit<T>
    where
        T: Sized
    {
        unsafe {
            let raw = std::alloc::alloc(RcHeader::layout(t));
            let header = raw as *mut RcHeader<T>;
            let data = raw.add(RcHeader::offset(t)) as *mut T;

            header.write(RcHeader::new(NonNull::new(data).unwrap()));
            &mut *(data as *mut MaybeUninit<T>)
        }
    }

    fn alloc_array<'a, 'b>(ts: &'a [ManuallyDrop<T>]) -> &'b mut [MaybeUninit<T>]
    where
        T: Sized
    {
        unsafe {
            let raw = std::alloc::alloc(RcHeader::layout(ts));
            let header = raw as *mut RcHeader<[T]>;
            let data = std::slice::from_raw_parts_mut(
                raw.add(RcHeader::offset(ts)) as *mut T,
                ts.len()
            ) as *mut [T];

            header.write(RcHeader::new(NonNull::new(data).unwrap()));
            &mut *(data as *mut [MaybeUninit<T>])
        }
    }

    unsafe fn dealloc(t: &mut ManuallyDrop<T>) {
        std::alloc::dealloc(
            RcHeader::header(t)
                as *const RcHeader<ManuallyDrop<T>>
                as *mut u8,
            RcHeader::layout(t)
        );
    }
}

impl<T: ?Sized> Rc<T> {
    fn header(&self) -> &RcHeader<T> {
        unsafe { RcHeader::header(self.ptr.as_ref()) }
    }

    fn manual(&mut self) -> &mut ManuallyDrop<T> {
        unsafe { &mut *(self.ptr.as_ptr() as *mut ManuallyDrop<T>) }
    }
}

impl<T> Rc<T> {
    pub fn new(t: T) -> Self {
        let rcbox = unsafe {
            let rcbox = RcHeader::alloc(&t);
            rcbox.write(t);
            rcbox.assume_init_ref()
        };

        Rc{ptr: NonNull::from(rcbox), phantom: PhantomData}
    }
}

impl<T: ?Sized> Clone for Rc<T> {
    fn clone(&self) -> Self {
        let rc = self.header().rc.get();
        self.header().rc.set(rc + 1);

        Rc{ptr: self.ptr, phantom: PhantomData}
    }
}

impl<T: ?Sized> Drop for Rc<T> {
    fn drop(&mut self) {
        let rc = self.header().rc.get();
        if rc == 1 {
            unsafe {
                let ptr = self.manual();
                ManuallyDrop::drop(ptr);
                RcHeader::dealloc(ptr);
            }
        } else {
            self.header().rc.set(rc - 1);
        }
    }
}

impl<T: Clone> Rc<T> {
    pub fn try_unwrap(mut self) -> Result<T, Rc<T>> {
        let rc = self.header().rc.get();
        if rc == 1 {
            unsafe {
                let ptr = self.manual();
                let t = ManuallyDrop::take(ptr);
                RcHeader::dealloc(ptr);
                Ok(t)
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
    unsafe fn from_array(src: &mut [ManuallyDrop<T>]) -> Rc<[T]> {
        let rcbox = unsafe {
            let rcbox = RcHeader::alloc_array(src);
            for i in 0..src.len() {
                rcbox[i].write(ManuallyDrop::take(&mut src[i]));
            }
            &*(rcbox as *const [MaybeUninit<T>] as *const [T])
        };

        Rc{ptr: NonNull::from(rcbox), phantom: PhantomData}
    }
}

impl<T> From<Box<[T]>> for Rc<[T]> {
    fn from(b: Box<[T]>) -> Self {
        unsafe {
            let mut b = transmute::<Box<[T]>, Box<[ManuallyDrop<T>]>>(b);
            Self::from_array(&mut b)
        }
    }
}

impl<T> From<Vec<T>> for Rc<[T]> {
    fn from(v: Vec<T>) -> Self {
        unsafe {
            let mut v = transmute::<Vec<T>, Vec<ManuallyDrop<T>>>(v);
            Self::from_array(&mut v)
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
            ManuallyDrop::take(
                &mut *(&mut ManuallyDrop::new(self)
                    as *mut ManuallyDrop<Self>
                    as *mut ManuallyDrop<T>)
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

