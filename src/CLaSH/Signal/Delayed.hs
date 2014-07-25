{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module CLaSH.Signal.Delayed
  ( -- * Delay-annotated synchronous signals
    DSignal
  , dsignal
  , delay
  , feedback
    -- * Signal \<-\> DSignal conversion
  , fromSignal
  , toSignal
    -- * List \<-\> DSignal conversion
  , dsample
  , dsampleN
  , dfromList
  )
where

import Data.Coerce                (coerce)
import Data.Default               (Default(..))
import Control.Applicative        (Applicative (..), (<$>))
import GHC.TypeLits               (type (-))
import CLaSH.Promoted.Nat         (SNat,UNat(..),snatToInteger,toUNat)
import CLaSH.Sized.Vector         ((+>>), vlast, vcopyU)

import CLaSH.Signal.Implicit
import CLaSH.Signal.Types

dfromList :: [a] -> DSignal t a
dfromList = coerce . fromList

dsample :: DSignal t a -> [a]
dsample = sample . coerce

dsampleN :: Int -> DSignal t a -> [a]
dsampleN n = sampleN n . coerce

delay :: forall a n m . Default a
      => SNat m
      -> DSignal (n - m) a
      -> DSignal n a
delay m = coerce . delay' . coerce
  where
    delay' :: Signal a -> Signal a
    delay' s = case toUNat m of
                 UZero       -> s
                 u@(USucc _) -> let r = unpack (register (vcopyU u def) (pack (s +>> r)))
                                in  vlast r

feedback :: (DSignal (n - m - 1) a -> DSignal n a) -> DSignal (n - m - 1) a
feedback f = let (DSignal r) = f (DSignal r) in (DSignal r)

fromSignal :: Signal a -> DSignal 0 a
fromSignal = coerce

toSignal :: SNat m -> DSignal m a -> Signal (Maybe a)
toSignal m s = count (coerce s)
  where
    count s' = o
      where
        r      = register (snatToInteger m) r'
        (r',o) = unpack (cntr <$> r <*> s')

        cntr 0 v = (0,Just v)
        cntr k _ = (k-1,Nothing)
