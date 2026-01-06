{- ORMOLU_DISABLE -}
module Ident6 where

ident1 :: Int
ident1 = 42

data Ident2 = Foo Int | Bar

newtype Ident3 = Ident3 String

type Ident4 = Bool

class Ident5 a where
  method :: a -> Int

class (Ident7 a) => Ident8 a where
  function :: a -> Int

-- Macros defined with CPP extension
#define IDENT9(foo) foo * 2
#define IDENT10 x

class Ident5b a where
  method :: a -> Int

class Ident5 a => Ident8b a where
  function :: a -> Int
