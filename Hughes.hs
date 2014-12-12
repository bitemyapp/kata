module Hughes where

import Data.Monoid

newtype Hughes a = Hughes ([a] -> [a])

runHughes :: Hughes a -> [a]
runHughes (Hughes k) = k []

mkHughes :: [a] -> Hughes a
mkHughes xs = Hughes (\ys -> mappend xs ys)

------------------------------------------------------------

consDumb :: a -> Hughes a -> Hughes a
consDumb a h = mkHughes (a : runHughes h)

cons :: a -> Hughes a -> Hughes a
cons a (Hughes h) = Hughes ((a:) . h)

------------------------------------------------------------

appendDumb :: Hughes a -> Hughes a -> Hughes a
appendDumb a b = mkHughes (runHughes a ++ runHughes b)

instance Monoid (Hughes a) where
  mempty = error "todo"
  mappend = error "todo"
  
------------------------------------------------------------

snocDumb :: Hughes a -> a -> Hughes a
snocDumb l a = mkHughes (runHughes l ++ [a])

snoc :: Hughes a -> a -> Hughes a
snoc = error "todo"
