module Main where

import Examples.Simple
import Examples.Cond
import Examples.Loops
import Examples.Fun
import Examples.Inputs

import Nightfall.Alphabet
import Nightfall.MASM
import Nightfall.Targets.Miden

import Control.Monad.State
import Data.Map as Map
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
    let allProgs = Map.fromList [ ("trivial1", trivial1Prog)
                                , ("trivial2", trivial2Prog)
                                , ("trivial3", trivial3Prog)
                                , ("trivial4", trivial4Prog)
                                , ("simpleVar1", simpleVar1Prog)
                                , ("simpleVar2", simpleVar2Prog)
                                , ("simpleVar3", simpleVar3Prog)
                                , ("simpleIf", simpleIfProg)
                                , ("ifVar", ifVarProg)
                                , ("simpleInf", simpleInfProg)
                                , ("sumTo10", sumTo10Prog)
                                , ("collatzFixed", collatzFixedProg)
                                , ("collatzPriv", collatzPrivProg)
                                , ("simpleSecret", simpleSecretProg)
                                ]

    args <- getArgs

    -- For now, trivial: argument is the name of the example to run
    when (length args < 1) $ do
        exampleName <- getProgName
        let str = "Usage: " ++ exampleName ++ " <example> [filepath] where <example> is one of:\n"
            examples = unlines . Prelude.map ("    "++) . Map.keys $ allProgs
        error $ str ++ examples ++ "\n and [filepath] (optional) is path to write the MASM, otherwise stdout"

    masm <- case Map.lookup (head args) allProgs of
        Nothing -> do
            let str = "Example program \"" ++ head args ++ "\" not found. Available ones are:\n"
                examples = unlines . Prelude.map ("    "++) . Map.keys $ allProgs
            error $ str ++ examples
        Just prog -> do
            let cfg = defaultConfig { _cgfTraceVariablesDecl = True
                                    , _cfgTraceVariablesUsage = True
                                    }
                context = defaultContext { _config = cfg }
            let (midenProg, _) = runState (transpileZKProgram prog) context
            return midenProg

    -- Check if the user provided a path to write the program
    let io = if (length args >= 2)
        then do
            let fp = args !! 1
            writeFile fp (ppMASM masm)
            putStrLn $ "Miden program written in \"" ++ fp ++ "\""
        else do
            putStrLn . ppMASM $ masm

    io
