{- ORMOLU_DISABLE -}
module Ident6 where

ident1 :: Int
ident1 = 42

data Ident2 = Foo Int | Bar

newtype Ident3 = Ident3 String

type Ident4 = Bool

class Ident5 a where
  method :: a -> Int

class (Ident7 b) => Ident8 b where
  function :: b -> Int

-- Macros defined with CPP extension
#define IDENT9(foo) foo * 2
#define IDENT10 x
