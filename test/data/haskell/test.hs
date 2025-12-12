module TestModule where

ident1 :: Int
ident1 = 42

data Ident2 = Foo Int | Bar

newtype Ident3 = Ident3 String

type Ident4 = Bool

class Ident5 a where
  method :: a -> Int
