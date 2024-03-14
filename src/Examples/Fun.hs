{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Examples.Fun ( collatzFixedProg
                    , collatzPrivProg
                    ) where

import Nightfall.Lang.Types
import Nightfall.Lang.Syntax.Default

-- * The Collatz Sequence, start from 10

-- Haskell version
{-
collatzFixed :: Felt
collatzFixed = collatz' 10
    where collataz' :: Felt -> Felt
          collatz 1 = 1
          collatz n | odd n = collatz (3 * n + 1)
                    | otherwise = collatz (n `div` 2)
-}

-- | DSL version
collatzFixedStmts :: Body asm ()
collatzFixedStmts = do
    comment "Compute the Collatz sequence, starting from a fixed position: 10 and returns the length of the sequence."
    comment "It should return 7"
    emptyLine
    start <- declare "start" 10
    len <- declare "len" 1
    n <- declare "n" $ get start
    while (get n `gt` 1) $ do
        set len $ get len + 1
        ifElse (isOdd $ get n)
            (set n $ get n * 3 + 1)
            (set n $ get n `div'` 2)
    ret $ get len

collatzFixedProg :: ZKProgram
collatzFixedProg = mkSimpleProgram "Fixed Collatz (10)" collatzFixedStmts

-- * The Collatz Sequence, but the starting number comes from the private inputs

-- Haskell version
{-
collatzPriv :: Felt -> Felt
collatzPriv n = collatz' n
    where collataz' :: Felt -> Felt
          collatz 1 = 1
          collatz n | odd n = collatz (3 * n + 1)
                    | otherwise = collatz (n `div` 2)
-}

-- | DSL version
collatzPrivStmts :: Body asm ()
collatzPrivStmts = do
    comment "Compute the Collatz sequence, starting position taken from secret input"
    comment "It returns the length of the sequence"
    emptyLine
    start <- declare "start" nextSecret
    n <- declare "n" $ get start
    len <- declare "len" 1
    while (get n `gt` 1) $ do
        mut len $ \x -> add x 1
        ifElse (isOdd $ get n)
            (mut n $ \x -> x * 3 + 1)
            (mut n $ \x -> x `div'` 2)
    ret $ get len

collatzPrivProg :: ZKProgram
collatzPrivProg = mkZKProgram "collatz private" collatzPrivStmts [] "src/Examples/collatz_secrets.inputs"
