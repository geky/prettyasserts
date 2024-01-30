
use std::ops::Deref;
use std::ops::DerefMut;
use std::mem::transmute;
use std::cell::RefCell;
use std::borrow::BorrowMut;
use std::rc::Rc;


// we can't use Rust's Any because it's 'static
pub trait Swimmer {}
impl<T> Swimmer for T {}

pub struct Pool<'a>(RefCell<Vec<Box<dyn Swimmer + 'a>>>);

impl<'a> Pool<'a> {
    fn new() -> Self {
        Pool(RefCell::new(vec![]))
    }

    fn pool<T: Swimmer>(&self, t: T) -> &'a T {
        let mut o = self.0.borrow_mut();
        o.push(Box::new(t));
        let ref_ = o.last().unwrap().as_ref();
        unsafe { &*(ref_ as *const dyn Swimmer as *const T) }
    }
}

pub trait Swim {
    fn swim<'a>(self, o: &mut Pool<'a>) -> &'a Self;
}

impl<T: Swimmer> Swim for T {
    fn swim<'a>(self, o: &mut Pool<'a>) -> &'a Self {
        o.pool(self)
    }
}

// here's the actual type that's relatively safe to use
pub struct Pooled<T>(T, Rc<Pool<'static>>);

impl<T> Pooled<T> {
    pub fn new(t: T) -> Self {
        Pooled(t, Rc::new(Pool::new()))
    }

    pub fn from_fn<'a, F>(cb: F) -> Self
    where
        F: FnOnce(&mut Pool<'a>) -> T
    {
        Self::try_from_fn(|o| Ok::<_, ()>(cb(o))).unwrap()
    }

    pub fn try_from_fn<'a, F, E>(cb: F) -> Result<Self, E>
    where
        F: FnOnce(&mut Pool<'a>) -> Result<T, E>
    {
        let mut o = Pool::new();
        let t = cb(
            unsafe { transmute::<&mut Pool<'static>, &mut Pool<'a>>(&mut o) }
        )?;
        Ok(Pooled(t, Rc::new(o)))
    }
}

// extra stuff
impl<T: Default> Default for Pooled<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Pooled<T> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>
    ) -> Result<(), std::fmt::Error> {
        f.debug_tuple("Pooled")
            .field(&self.0)
            // omit swim
            .finish()
    }
}

impl<T: Clone> Clone for Pooled<T> {
    fn clone(&self) -> Self {
        Pooled(self.0.clone(), self.1.clone())
    }
}

impl<T> AsRef<T> for Pooled<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> AsMut<T> for Pooled<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T> Deref for Pooled<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Pooled<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// extra trait needed for tree transformations
pub trait Pfork<'a, M> {
    type Pforked;

    fn _pfork(
        &self,
        o: &mut Pool<'a>,
        // recursion makes the compiler explode if f is generic!
        cb: &mut dyn FnMut(&mut Pool<'a>, M) -> M
    ) -> Self::Pforked {
        self._try_pfork(o, &mut |o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    fn _try_pfork<E>(
        &self,
        o: &mut Pool<'a>,
        // recursion makes the compiler explode if f is generic!
        cb: &mut dyn FnMut(&mut Pool<'a>, M) -> Result<M, E>
    ) -> Result<Self::Pforked, E>;

    // convenience wrappers, but if recursion is used use the above or
    // else Rust will barf when the compiler itself overflows
    fn pfork<F>(
        &self,
        o: &mut Pool<'a>,
        // recursion makes the compiler explode if f is generic!
        mut cb: F
    ) -> Self::Pforked
    where
        F: FnMut(&mut Pool<'a>, M) -> M
    {
        self._pfork(o, &mut cb)
    }

    fn try_pfork<F, E>(
        &self,
        o: &mut Pool<'a>,
        // recursion makes the compiler explode if f is generic!
        mut cb: F
    ) -> Result<Self::Pforked, E>
    where
        F: FnMut(&mut Pool<'a>, M) -> Result<M, E>
    {
        self._try_pfork(o, &mut cb)
    }
}

impl<T> Pooled<T> {
    pub fn pfork<'a, M, F,>(&self, mut cb: F) -> Pooled<T::Pforked>
    where
        T: Pfork<'a, M>,
        F: FnMut(&mut Pool<'a>, M) -> M
    {
        self.try_pfork(|o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    pub fn try_pfork<'a, M, F, E>(&self, mut cb: F) -> Result<Pooled<T::Pforked>, E>
    where
        T: Pfork<'a, M>,
        F: FnMut(&mut Pool<'a>, M) -> Result<M, E>
    {
        // create a new swim
        let mut o = Pool::new();
        // map t
        let t = self.0._try_pfork(
            unsafe { transmute::<&mut Pool<'static>, &mut Pool<'a>>(&mut o) },
            &mut cb
        )?;
        Ok(Pooled(t, Rc::new(o)))
    }
}



