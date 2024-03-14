{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Evaluation

import Examples.Cond
import Examples.Fun
import Examples.Inputs
import Examples.Loops
import Examples.Simple

import Nightfall.Lang.Internal.Types
import Nightfall.Lang.Types
import Nightfall.MASM
import Nightfall.MASM.Miden
import Nightfall.MASM.Types
import Nightfall.Targets.Miden

import Control.Monad.State
import Data.Aeson (eitherDecodeFileStrict, (.:))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as Lazy
import Data.Char
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.String
import Data.Word
import System.FilePath
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

-- TODO: handle @advice_map@ too.
replaceSecretInputsFileWithItsContents :: Module -> Module
replaceSecretInputsFileWithItsContents modul = modul
    { _moduleSecretInputs = do
        path <- _moduleSecretInputs modul
        let adviceStack = either error id $ do
                obj <- unsafePerformIO $ eitherDecodeFileStrict path
                parseEither (.: "advice_stack") obj
        Left $ SecretInputs (map read adviceStack) mempty
    }

goldenVsStringColored :: TestName -> FilePath -> IO Lazy.ByteString -> TestTree
goldenVsStringColored name = goldenVsStringDiff name diff where
    diff old new =
        -- Using @diff@ for sensible and readable output.
        ["diff", "-u", "--color=always", old, new]

test_examplesGolden :: TestTree
test_examplesGolden =
    testGroup "golden examples"
        [ goldenExample trivial1Prog
        , goldenExample trivial2Prog
        , goldenExample trivial3Prog
        , goldenExample trivial4Prog
        , goldenExample simpleVar1Prog
        , goldenExample simpleVar2Prog
        , goldenExample simpleVar3Prog
        , goldenExample simpleInitArray
        , goldenExample sumTo10Prog
        , goldenExample simpleSecretProg
        , goldenExample collatzFixedProg
        , goldenExample collatzPrivProg
        , goldenExample simpleIfProg
        , goldenExample ifVarProg
        , goldenExample simpleInfProg
        , goldenExample simpleNat
        ]
  where
    goldenExample prog =
        let context = defaultContext
                { _config = defaultConfig
                    { _cgfTraceVariablesDecl = True
                    , _cfgTraceVariablesUsage = True
                    }
                }
            (masm, _) = runState (transpileZKProgram prog) context
            name = filter isAlphaNum $ pName prog
            path = "test" </> "golden" </> name
        in testGroup name $
            [ goldenVsStringColored "program" (path ++ ".masm") $
                pure . fromString $
                    ppMASM masm
            , goldenVsStringColored "result" (path ++ ".mres") $
                fromString . either id show <$>
                    runMiden DontKeep Nothing masm
            ] ++
            -- Just to test that 'runMiden' correctly handles secret inputs provided via a list of
            -- 'Felt's directly, not just via an inputs file.
            [ goldenVsStringColored "result via list" (path ++ ".mres") $
                fromString . either id show <$>
                    runMiden DontKeep Nothing (replaceSecretInputsFileWithItsContents masm)
            | Right _ <- [_moduleSecretInputs masm]
            ]

-- Test cases taken from https://github.com/0xPolygonMiden/examples/blob/df0eeeeac29eab0e5e3d5cbe06921e372a189039/examples/advice_provider.masm
test_midenWordToHexKey :: TestTree
test_midenWordToHexKey =
    testCase "midenWordToHexKey" $ do
        midenWordToHexKey @String (MidenWord 268435456 0 0 0) @?=
            "0000001000000000000000000000000000000000000000000000000000000000"
        midenWordToHexKey @String (MidenWord 0 216172782113783808 51 0) @?=
            "0000000000000000000000000000000333000000000000000000000000000000"

unsafeFeltsToMidenWords :: [Felt] -> [MidenWord]
unsafeFeltsToMidenWords = map unsafeFeltsToMidenWord . chunksOf 4

test_displaySecretInputs :: TestTree
test_displaySecretInputs =
    goldenVsStringColored "displaySecretInputs" ("test" </> "golden" </> "secret.inputs") $
        pure . fromString . displaySecretInputs $ SecretInputs
            (42 : take 20 [0 ..] ++ take 20 [fromIntegral (maxBound :: Word32) ..])
            (Map.fromList
                [ ( midenWordToHexKey $ MidenWord 0 216172782113783808 51 0
                  , unsafeFeltsToMidenWords $ take 4 [216172782113783809 ..]
                  )
                , ( midenWordToHexKey $ MidenWord 1 2 3 4
                  , unsafeFeltsToMidenWords [36, 35 .. 21]
                  )
                , ( midenWordToHexKey $ MidenWord 5 6 7 8
                  , []
                  )
                ])

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ test_examplesGolden
    , test_midenWordToHexKey
    , test_evaluation
    , test_displaySecretInputs
    ]
