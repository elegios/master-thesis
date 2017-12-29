module Types.Result where

ok :: ResultT e ()
ok = Data ()

data ResultT e a = Data a | Error e

instance Functor (ResultT e) where
  fmap f (Data a) = Data $ f a
  fmap _ (Error e) = Error e

instance Monoid e => Applicative (ResultT e) where
  pure = Data
  (Data f) <*> (Data a) = Data $ f a
  (Error e1) <*> (Error e2) = Error $ mappend e1 e2
  (Error e) <*> _ = Error e
  _ <*> (Error e) = Error e

instance Monoid e => Monad (ResultT e) where
  Data a >>= f = f a
  Error e >>= _ = Error e
  (>>) = (*>)
