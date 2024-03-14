{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Simple ( trivial1Prog
                       , trivial2Prog
                       , trivial3Prog
                       , trivial4Prog
                       , simpleVar1Prog
                       , simpleVar2Prog
                       , simpleVar3Prog
                       , simpleInitArray
                       , simpleNat
                       ) where

import Nightfall.Alphabet
import Nightfall.Lang.Syntax.Default
import Nightfall.Lang.Types
import Nightfall.Targets.Miden

-- * Simplest, most trivial program that adds to fixed numbers, no variable, no inputs (public or secret)

-- | That's the Haskell program to describe with the EDSL
{-
trivial1 :: Felt
trivial1 = 33 + 42
-}

-- | That's the EDSL version
trivial1Body :: Body asm ()
trivial1Body = do
    comment "Simply add fixed numbers 33 + 42, should output 75"
    ret $ add (lit 33) (lit 42)

trivial1Prog :: ZKProgram
trivial1Prog = ZKProgram { pName = "trivial 1"
                          , pBody = trivial1Body
                          , pPublicInputs = []
                          , pSecretInputs = Left emptySecretInputs
                          }

-- * Almost as simple a program, we just have one nested level

-- | That's the Haskell program to describe with the EDSL
{-
trivial2 :: Felt
trivial2 = 29 + 156 + 14 
-}

-- | That's the EDSL version
trivial2Body :: Body asm ()
trivial2Body = do
    comment "Simply add three numbers, 29 + 156 + 14, should output 199"
    ret $ add (lit 29) (add (lit 156) (lit 14))

trivial2Prog :: ZKProgram
trivial2Prog = ZKProgram { pName = "trivial 2"
                          , pBody = trivial2Body
                          , pPublicInputs = []
                          , pSecretInputs = Left emptySecretInputs
                          }


-- * This time we have a multiplication and an addition, we need to make sure orders of evaluation is correct

-- | That's the Haskell program to describe with the EDSL
{-
trivial3 :: Felt
trivial3 = 1238 * (345 + 78)
-}

-- | That's the EDSL version
trivial3Body :: Body asm ()
trivial3Body = do
    comment "Perform an addition followed by a multipication, 1238 * (345 + 78), should output 523674"
    ret $ mul (lit 1238) (add (lit 345) (lit 78))

trivial3Prog :: ZKProgram
trivial3Prog = ZKProgram { pName = "trivial 3"
                          , pBody = trivial3Body
                          , pPublicInputs = []
                          , pSecretInputs = Left emptySecretInputs
                          }

-- * Another simple program that uses the Num instance t omake it easier to write and make sure it works

-- | That's the Haskell program to describe with the EDSL
{-
trivial4 :: Felt
trivial4 = 52 * (11 - 1)
-}

-- | That's the EDSL version
trivial4Body :: Body asm ()
trivial4Body = do
    comment "Performs 52 * (11 - 1), written with Num instance, should output 520"
    ret $ 52 * (11 - 1)

trivial4Prog :: ZKProgram
trivial4Prog = ZKProgram { pName = "trivial 4"
                          , pBody = trivial4Body
                          , pPublicInputs = []
                          , pSecretInputs = Left emptySecretInputs
                          }

-- * Simple program using a variable to store a value

-- Haskell program
{-
simpleVar1 :: Felt
simpleVar1 = let a = 999
             in a + 1
-}


-- EDSL version
simpleVar1Body :: Body asm ()
simpleVar1Body = do
    comment "Simple addition, but with a variable storing a value"
    comment "a = 999"
    comment "a + 1. It should return 1000"
    a <- declare "a" (lit 999)
    ret $ add (get a) (lit 1)

simpleVar1Prog :: ZKProgram
simpleVar1Prog = ZKProgram { pName = "simple var 1"
                          , pBody = simpleVar1Body
                          , pPublicInputs = []
                          , pSecretInputs = Left emptySecretInputs
                          }


-- * Same program as before, but uses more variables and make a subtraction. Makes sure values are fetched correctly

-- Haskell program
{-
simpleVar2 :: Felt
simpleVar2 = let a = 888
                 b = 222
                 diff = a - b
             in diff
-}


-- EDSL version
simpleVar2Body :: Body asm ()
simpleVar2Body = do
    comment "Simple subtraction, but uses three variables"
    comment "a = 888, b = 222"
    comment "c = a - b. Return c"
    comment "It should return 666"
    a <- declare "a" 888
    b <- declare "b" 222
    c <- declare "c" $ get a - get b
    ret $ get c

simpleVar2Prog :: ZKProgram
simpleVar2Prog = ZKProgram { pName = "simple var 2"
                          , pBody = simpleVar2Body
                          , pPublicInputs = []
                          , pSecretInputs = Left emptySecretInputs
                          }

-- * Simple program that overwrite the values of a variable several times

simpleVar3Body :: Body asm ()
simpleVar3Body = do
    comment "Rewrite on the same variable several times"
    comment "a = 10, b = 20, a = 50, a + b. Should return 70"
    a <- declare "a" 10
    b <- declare "b" 20
    set a 50
    ret $ get a + get b

simpleVar3Prog :: ZKProgram
simpleVar3Prog = mkSimpleProgram "simple var 3" simpleVar3Body

simpleInitArray :: ZKProgram
simpleInitArray = mkSimpleProgram "simple initArray" $ do
    initArray "arr" [42, 0, 13, 885, 4, 193, 193]
    comment "oldArrAt3 = arr[3]"
    oldArrAt3 <- declare "oldArrAt3" $ getAt "arr" 3
    setAt "arr" 3 1
    comment "arr[1] + oldArrAt3 + arr[3] + arr[4] = 0 + 885 + 1 + 4 = 890"
    ret $ getAt "arr" 1 + get oldArrAt3 + getAt "arr" 3 + getAt "arr" 4

simpleNat :: ZKProgram
simpleNat = mkSimpleProgram "simple nat" $ do
    initArray "fakeArr" []
    _ <- declareOf Nat "n" . lit $ 5 + 2^!64 * 7 + 2^!128 * 3
    comment "n[0] + n[1] = 5 + 3 = 8"
    let getLimbAt i = getAt "fakeArr" (lit $ dynamicMemoryHead + i + 1)
    ret $ getLimbAt 0 + getLimbAt 1
