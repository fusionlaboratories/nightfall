module Examples.Inputs ( simpleSecretProg
                       ) where

import Nightfall.Lang.Types

-- | Simple program that outputs 10x the secret input

-- Haskell version
{-
simpleSecret :: Felt -> Felt
simpleSecret secret = 10 * secret
-}

-- DSL version
simpleSecretStmts :: Body asm ()
simpleSecretStmts = do
    comment "Simple program that outputs 10x the secret input"
    emptyLine
    ret $ 10 * nextSecret

simpleSecretProg :: ZKProgram
simpleSecretProg = mkZKProgram "simple secret" simpleSecretStmts [] "src/Examples/simple_secrets.inputs"
