module Nightfall.MASM.Miden where

import Nightfall.Alphabet
import Nightfall.Lang.Internal.Types
import Nightfall.Lang.Types
import Nightfall.MASM
import Nightfall.MASM.Types

import qualified Data.Map.Strict as Map
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

data KeepFile = Keep FilePath | DontKeep
  deriving Show

whenKeep :: KeepFile -> (FilePath -> IO a) -> IO (Maybe a)
whenKeep k f = case k of
  DontKeep -> return Nothing
  Keep fp  -> Just <$> f fp

-- TODO: use @prettyprinter@?
displaySecretInputs :: SecretInputs -> String
displaySecretInputs (SecretInputs advStack advMap) = unlines $ concat
    [ -- TODO: wire the public inputs in properly.
      [ "{ \"operand_stack\": []"
      ]
    , [ ", \"advice_stack\": " ++ show (map show advStack)
      | not $ null advStack
      ]
    , if Map.null advMap
        then []
        else concat
            [ [", \"advice_map\":"]
            , zipWith
                (\fmt strKV -> replicate 4 ' ' ++ fmt ++ " " ++ strKV)
                ("{" : repeat ",")
                (map (uncurry displayKV) $ Map.toList advMap)
            , ["    }"]
            ]
    , [ "}"
      ]
    ]
  where
    displayKV hash wrds = show hash ++ ": " ++ show (concatMap midenWordToFelts wrds)

runMiden :: KeepFile -> Maybe Word32 -> Module -> IO (Either String [Felt])
runMiden keep mayNumOutputs m =
    withSystemTempFile "nightfall-testfile-XXX.masm" $ \masmPath masmHandle ->
    withSystemTempFile "nightfall-testfile-XXX.inputs" $ \inputsPath inputsHandle -> do
        hPutStrLn masmHandle $ ppMASM m
        hClose masmHandle
        -- TODO: implement the file keeping logic for the inputs file too?
        _ <- whenKeep keep $ \masmSavePath -> copyFile masmPath masmSavePath
        let numOutputsPrefix =
                maybe [] (\numOutputs -> ["--num-outputs", show numOutputs]) mayNumOutputs
        args <- mappend numOutputsPrefix <$> do
            let inputsFileOrList = _moduleSecretInputs m
            inputsIfAny <- case inputsFileOrList of
                Left secretInputs -> do
                    hPutStrLn inputsHandle $ displaySecretInputs secretInputs
                    hClose inputsHandle
                    pure ["--input", inputsPath]
                Right inputsOrigPath -> do
                    copyFile inputsOrigPath inputsPath
                    pure ["--input", inputsPath]
            pure $ ["--assembly", masmPath] ++ inputsIfAny
        (ex, midenout, midenerr) <- readProcessWithExitCode "miden" ("run" : args) ""
        case ex of
            ExitSuccess -> do
                case mapMaybe (stripPrefix "Output: " >=> readMaybe) $ lines midenout of
                    [stack] -> return $ Right stack
                    _ -> return . Left . concat $ concat
                        [ [ "Unexpected miden run output." ]
                        , [ "\n\nstdout:\n" ++ midenout | not $ null midenout]
                        , [ "\n\nstderr:\n" ++ midenerr | not $ null midenerr]
                        ]
            ExitFailure e -> return . Left . concat $ concat
                 [ [ "Miden run failed." ]
                 , [ "\nThe error code was " ++ show e ]
                 , [ "\n\nstdout:\n" ++ midenout | not $ null midenout]
                 , [ "\n\nstderr:\n" ++ midenerr | not $ null midenerr]
                 ]

runMidenProve :: Module -> IO (FilePath, FilePath, String)
runMidenProve m = do
    masmPath <- writeSystemTempFile "nightfall-testfile-XXX.masm" (ppMASM m)
    let outFile = masmPath <.> "out"
        proofFile = masmPath <.> "proof"
    (ex, midenout, midenerr) <-
      readProcessWithExitCode "miden"
        ["prove", "--assembly", masmPath, "-o", outFile, "-p", proofFile] ""
    -- putStrLn midenout
    -- putStrLn midenerr
    case ex of
        ExitSuccess ->
            case  map (takeWhile (/='.') . drop (length hashPrefix)) $ filter (hashPrefix `isPrefixOf`) (lines midenout) of
                [hash] -> return (outFile, proofFile, hash)
                _ -> error "couldn't determine hash from miden prove output"
        ExitFailure n -> error $ "miden prove failed: " ++ show (n, midenout, midenerr)

  where hashPrefix = "Proving program with hash "

runMidenVerify :: FilePath -> FilePath -> String -> IO (Maybe (ExitCode, String, String))
runMidenVerify out proof hash = do
    (ex, mout, merr) <- readProcessWithExitCode "miden"
      ["verify", "-p", proof, "-o", out, "-h", hash] ""
    -- putStrLn mout
    -- putStrLn merr
    case ex of
        ExitSuccess -> return Nothing
        ExitFailure _ -> return $ Just (ex, mout, merr)
