module Ident6 where -- Note that this file gets formatted with ormolu too (make fmt).

ident1 :: Int
ident1 = 42

data Ident2 = Foo Int | Bar

newtype Ident3 = Ident3 String

type Ident4 = Bool

class Ident5 a where
  method :: a -> Int

class (Ident7 b) => Ident8 b where
  function :: b -> Int
