{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Type)
import GHC.TypeNats (Nat, type (-), type (<=))

data HList (xs :: [Type]) where
  HNil :: HList '[]
  (:&) :: x -> HList xs -> HList (x ': xs)

infixr 5 :&

type family At (n :: Nat) (xs :: [Type]) :: Type where
  At 0 (x ': xs) = x
  At n (x ': xs) = At (n - 1) xs

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

class HIndex (n :: Nat) (xs :: [Type]) where
  get :: HList xs -> At n xs
  set :: At n xs -> HList xs -> HList xs

instance {-# OVERLAPPING #-} HIndex 0 (x ': rest) where
  get (x :& _) = x
  set value (_ :& rest) = value :& rest

instance {-# OVERLAPPABLE #-}
  ( 1 <= n,
    HIndex (n - 1) rest,
    At n (x ': rest) ~ At (n - 1) rest
  ) =>
  HIndex n (x ': rest)
  where
  get (_ :& rest) = get @(n - 1) rest
  set value (x :& rest) = x :& set @(n - 1) value rest

append :: HList xs -> HList ys -> HList (Append xs ys)
append HNil right = right
append (x :& rest) right = x :& append rest right

add :: HList xs -> a -> HList (Append xs '[a])
add list value = append list (value :& HNil)

xs = "hello" :& 42 :& True :& HNil

ys = set @0 "world" xs

zs = set @1 100 ys

xs' = add (add (add HNil "hello") (42 :: Int)) True

zs' = add (set @1 100 xs') (3.14 :: Double)

main :: IO ()
main = do
  putStrLn (get @0 zs)
  print (get @1 zs)
  print (get @2 zs)
  print (get @3 zs')

-- These expressions do not compile:
-- invalidType = set @1 "hello" xs
-- invalidIndex = get @3 xs
