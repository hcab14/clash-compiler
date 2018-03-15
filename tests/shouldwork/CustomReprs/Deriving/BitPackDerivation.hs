module BitPackDerivation where

import Clash.Prelude
import Clash.Prelude.Testbench
import Clash.Annotations.BitRepresentation.Deriving

type SmallInt = Unsigned 2

data Train
  = Passenger
      SmallInt
      -- ^ Number of wagons
  | Freight
      SmallInt
      -- ^ Number of wagons
      SmallInt
      -- ^ Max weight
  | Maintenance
  | Toy

deriveAnnotation (simpleDerivator OneHot Overlap) [t| Train |]
deriveBitPack [t| Train |]

topEntity
  :: SystemClockReset
  => Signal System Train
  -> Signal System (BitVector 8)
topEntity trains = pack <$> trains
{-# NOINLINE topEntity #-}

testBench
  :: Signal System Bool
testBench = done'
  where
    testInput = stimuliGenerator $ Toy
                                :> Maintenance
                                :> Freight 2 3
                                :> Passenger 1
                                :> Nil

    expectedOutput :: SystemClockReset
                   => Signal System (BitVector 8) -> Signal System Bool
    expectedOutput = outputVerifier $ ($$(bLit "1000....") :: BitVector 8)
                                   :> ($$(bLit "0100....") :: BitVector 8)
                                   :> ($$(bLit "00101011") :: BitVector 8)
                                   :> ($$(bLit "000101..") :: BitVector 8)
                                   :> Nil
    done  = expectedOutput (topEntity testInput)
    done' = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
