use std::ops::Deref;
use std::ops::DerefMut;


// we can't use Rust's Any because it's 'static
pub trait Swimmer {}
impl<T> Swimmer for T {}

pub struct Pool<'a>(Vec<Box<dyn Swimmer + 'a>>);

impl<'a> Pool<'a> {
    fn new() -> Self {
        Pool(vec![])
    }

    fn pool<T: Swimmer>(&mut self, t: T) -> &'a T {
        self.0.push(Box::new(t));
        let ref_ = self.0.last().unwrap().as_ref();
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
pub struct Pooled<'a, T>(T, Pool<'a>);

impl<'a, T> Pooled<'a, T> {
    pub fn new(t: T) -> Self {
        Pooled(t, Pool::new())
    }

    pub fn from_fn<F>(cb: F) -> Self
    where
        F: FnOnce(&mut Pool<'a>) -> T
    {
        let mut o = Pool::new();
        let t = cb(&mut o);
        Pooled(t, o)
    }

    pub fn try_from_fn<F, E>(cb: F) -> Result<Self, E>
    where
        F: FnOnce(&mut Pool<'a>) -> Result<T, E>
    {
        let mut o = Pool::new();
        let t = cb(&mut o)?;
        Ok(Pooled(t, o))
    }
}

// extra stuff
impl<T: Default> Default for Pooled<'_, T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Pooled<'_, T> {
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

impl<T> AsRef<T> for Pooled<'_, T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> AsMut<T> for Pooled<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T> Deref for Pooled<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Pooled<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// extra trait needed for tree transformations
pub trait Map<'a, I> {
    type Mapped;

    // recursion makes the compiler explode if f is generic!
    fn _try_map<E>(
        &self,
        o: &mut Pool<'a>,
        cb: &mut dyn FnMut(&mut Pool<'a>, I) -> Result<I, E>
    ) -> Result<Self::Mapped, E>;

    // convenience wrappers
    fn map<F>(
        &self,
        o: &mut Pool<'a>,
        mut cb: F
    ) -> Self::Mapped
    where
        F: FnMut(&mut Pool<'a>, I) -> I
    {
        self._try_map(o, &mut |o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    fn try_map<F, E>(
        &self,
        o: &mut Pool<'a>,
        mut cb: F
    ) -> Result<Self::Mapped, E>
    where
        F: FnMut(&mut Pool<'a>, I) -> Result<I, E>
    {
        self._try_map(o, &mut cb)
    }
}

impl<'a, T> Pooled<'a, T> {
    pub fn map<I, F>(&self, mut cb: F) -> Pooled<'a, T::Mapped>
    where
        T: Map<'a, I>,
        F: FnMut(&mut Pool<'a>, I) -> I
    {
        self.try_map(|o, t| Ok::<_, ()>(cb(o, t))).unwrap()
    }

    pub fn try_map<I, F, E>(&self, cb: F) -> Result<Pooled<'a, T::Mapped>, E>
    where
        T: Map<'a, I>,
        F: FnMut(&mut Pool<'a>, I) -> Result<I, E>
    {
        // create a new swim
        let mut o = Pool::new();
        // map t
        let t = self.0.try_map(&mut o, cb)?;
        Ok(Pooled(t, o))
    }
}



