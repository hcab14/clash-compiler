{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Promoted.Nat.TH where

import Language.Haskell.TH
import CLaSH.Promoted.Nat

-- $setup
-- >>> :set -XDataKinds
-- >>> let d1111 = snat :: SNat 1111
-- >>> let d1200 = snat :: SNat 1200
-- >>> let d1201 = snat :: SNat 1201
-- >>> let d1202 = snat :: SNat 1202

-- | Create an 'SNat' literal
--
-- > $(decLiteralD 1111)
--
-- >>> :t d1111
-- d1111 :: SNat 1111
decLiteralD :: Integer
            -> Q [Dec]
decLiteralD n = do
  let suffix  = if n < 0 then error ("Can't make negative SNat: " ++ show n) else show n
      valName = mkName $ 'd':suffix
  sig   <- sigD valName (appT (conT ''SNat) (litT (numTyLit n)))
  val   <- valD (varP valName) (normalB [| snat |]) []
  return [ sig, val ]

-- | Create a range of 'SNat' literals
--
-- > $(decLiteralsD 1200 1202)
--
-- >>> :t d1200
-- d1200 :: SNat 1200
-- >>> :t d1201
-- d1201 :: SNat 1201
-- >>> :t d1202
-- d1202 :: SNat 1202
decLiteralsD :: Integer
             -> Integer
             -> Q [Dec]
decLiteralsD from to =
    fmap concat $ sequence $ [ decLiteralD n | n <- [from..to] ]
