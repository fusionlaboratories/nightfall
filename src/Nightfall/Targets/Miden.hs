{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Nightfall.Targets.Miden ( Context(..)
                               , defaultContext
                               , Config(..)
                               , defaultConfig
                               , transpile
                               ) where

import Nightfall.Lang.Internal.Types as NFTypes
import Nightfall.MASM.Types as MASM
import Control.Monad.State
import Control.Monad.State.Lazy ( modify )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as Text
import Data.Word (Word64)
import Data.List ( singleton )
import Data.Coerce ( coerce )

-- | Index in Miden's global memory (accessed via mem_load, mem_store, etc.)
type MemIdx = Word64

-- | Transpilation configuration options
data Config = Config {
      cgfTraceVariablesDecl :: Bool     -- ^ Whether or not adding comments when declaring variables
    , cfgTraceVariablesUsage :: Bool    -- ^ Whether or not adding comments when using ("calling") variables
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { cgfTraceVariablesDecl = False
    , cfgTraceVariablesUsage = False
}

-- | Context for the transpilation, this is the state of the State Monad in which transpilation happens
data Context = Context
    { progName :: String             -- ^ Name of the program, for outputs logs, etc.
    , memPos :: MemIdx               -- ^ Next free indice in Miden's global memory
    , variables :: Map String MemIdx -- ^ Variables in the EDSL are stored in Miden's random access memory, we keep a map to match them
    , config :: Config               -- ^ Transpilation configuration options
    , nbCycles :: Integer            -- ^ Number of execution cycles the program will take upon execution (we take upper bounds)
    }

defaultContext :: Context
defaultContext = Context
    { progName = "<unnamed-program>"
    , memPos = 0
    , variables = Map.empty
    , config = defaultConfig
    , nbCycles = 0
    }

-- | Entry point: transpile a EDSL-described ZK program into a Miden Module
transpile :: ZKProgram -> State Context Module
transpile zkProg = do
    warning <- transpileStatement . coerce $ comment "This program was generated by Nightfall (https://github.com/qredo/nightfall), avoid editing by hand.\n"
    modify $ \ctx -> ctx { progName = pName zkProg }
    midenInstr <- concat <$> mapM (transpileStatement . coerce) (pStatements zkProg)
    return $ Module { moduleImports = [] -- No import for now (TODO)
                    , moduleProcs = Map.empty -- No procs either (TODO)
                    , moduleProg = Program (head warning : midenInstr)
                    }

transpileStatement :: Statement_ -> State Context [Instruction]
transpileStatement (NFTypes.Comment str) = return . singleton . MASM.Comment . Text.pack $ str
transpileStatement (IfElse cond ifBlock elseBlock) = do
    ifBlock' <- concat <$> mapM transpileStatement ifBlock
    elseBlock' <- concat <$> mapM transpileStatement elseBlock
    -- Not entirely sure, Miden talks about a "a small, but non-negligible overhead", but they don't say.
    -- I'm supposing it takes a pop/drop operation + a comparison
    addCycles (dropCycles + eqCycles')
    cond' <- transpileExpr cond
    return $ cond' <> [ MASM.If ifBlock' elseBlock' ]
transpileStatement (NFTypes.While cond body) = do
    body' <- concat <$> mapM transpileStatement body
    cond' <- transpileExpr cond
    return $ cond' <> [ MASM.While $ body' <> cond' ]
-- | Declaring a variable loads the expression into Miden's global memory, and we track the index in memory
transpileStatement (DeclVariable varname e) = do
    vars <- gets variables
    pos <- gets memPos
    cfg <- gets config

    -- Check if this variable hasn't already been declared
    when (Map.member varname vars) $ do
        error $ "Variable \"" ++ varname ++ "\" has already been declared!"
    
    -- Insert the record
    let vars' = Map.insert varname pos vars

    -- Update the context
    modify $ \s -> s { memPos = pos + 1, variables = vars' }

    -- Trace the variable declaration if configured
    let traceVar = [MASM.Comment $ "var " <> Text.pack varname | cgfTraceVariablesDecl cfg]

    -- Transpile the variable value
    e' <- transpileExpr e

    addCycles mem_storeCycles'

    -- Return instruction for the variable value and instruction to store the value in global memory
    return $ e' <> traceVar <> [ MASM.MemStore . Just . fromIntegral $ pos ]

    -- Add Miden's instructions to load the value in global memory
    -- return . singleton . MASM.MemStore . Just . fromIntegral $ pos

-- | Assigning a new value to a variable if just erasing the memory location
transpileStatement (AssignVar varname e) = do
    vars <- gets variables
    shouldTrace <- gets (cfgTraceVariablesUsage . config)

    -- Check that this variable exists (has been declared before)
    unless (Map.member varname vars) $ do
        error $ "Variable \"" ++ varname ++ "\" has not been declared before: can't assign value"
    
    -- Fetch the memory location for that variable
    let (Just pos) = Map.lookup varname vars -- safe to patter match Just since we checked with Map.member before

    -- Trace the variable usage if configured
        traceVar = [MASM.Comment $ "var " <> Text.pack varname | shouldTrace]
    
    e' <- transpileExpr e

    addCycles mem_storeCycles'
    return $ e' <> traceVar <> [ MASM.MemStore . Just . fromIntegral $ pos ]

-- | A (naked) function call is done by pushing the argument on the stack and caling the procedure name
transpileStatement (NakedCall fname args) = do
    args' <- concat <$> mapM transpileExpr args
    return $ args' <> [ MASM.Exec . Text.pack $ fname ]

transpileStatement (Return mE) = case mE of
    -- an empty return statement doesn't correspond to an action in Miden
    Nothing -> return []
    -- When we do have a value, we simply push it to the stack
    Just e -> transpileExpr e

transpileStatement EmptyLine = return . singleton $ MASM.EmptyL

transpileStatement _ = error "transpileStatement::TODO"


-- TODO: range check, etc.
transpileExpr :: Expr_ -> State Context [Instruction]
-- transpileExpr (Lit _) = error "Can't transpile standalone literal" -- should be simply push it to the stack??
-- transpileExpr (Bo _)  = error "Can't transpile standalone boolean"
-- | Literals are simply pushed onto the stack
transpileExpr (Lit felt) = do
    addCycles pushCycles
    return . singleton . Push . fromIntegral $ felt
transpileExpr (Bo bo) = do
    let felt = if bo then 1 else 0
    addCycles pushCycles
    return . singleton . Push $ felt

-- | Using a variable means we fetch the value from (global) memory and push it to the stack
transpileExpr (VarF varname) = do
    -- Fetch the memory location of that variable in memory, and push it to the stack
    vars <- gets variables
    case Map.lookup varname vars of
        Nothing -> error $ "Felt variable \"" ++ varname ++ "\" unknown (undeclared)"
        Just idx -> do
            shouldTrace <- gets (cfgTraceVariablesUsage . config)
            let traceVar = [MASM.Comment $ "var " <> Text.pack varname <> " (felt)" | shouldTrace]
            addCycles mem_loadCycles'
            return $ traceVar <> [MemLoad . Just . fromIntegral $ idx]
transpileExpr (VarB varname) = do
    -- Fetch the memory location of that variable in memory, and push it to the stack
    vars <- gets variables
    case Map.lookup varname vars of
        Nothing -> error $ "Boolean variable \"" ++ varname ++ "\" unknown (undeclared)"
        Just idx -> do
            shouldTrace <- gets (cfgTraceVariablesUsage . config)
            let traceVar = [MASM.Comment $ "var " <> Text.pack varname <> " (bool)" | shouldTrace]
            addCycles mem_loadCycles'
            return $ traceVar <> [MemLoad . Just . fromIntegral $ idx]

-- | Arithmetics operations are matched to their corresponding Miden operations
transpileExpr (NFTypes.Add e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    addCycles add_Cycles
    return $ e1s <> e2s <> [ MASM.Add Nothing ]
transpileExpr (NFTypes.Sub e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    addCycles subCycles
    return $ e1s <> e2s <> [ MASM.Sub Nothing ]
transpileExpr (NFTypes.Mul e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    addCycles mulCycles
    return $ e1s <> e2s <> [ MASM.Mul Nothing ]
transpileExpr (NFTypes.Div e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    addCycles divCycles
    return $ e1s <> e2s <> [ MASM.Div Nothing ]
transpileExpr (NFTypes.Mod e1 e2) = do
    error "No support for simple 'mod' function in Miden"
    -- e1s <- transpileExpr e1
    -- e2s <- transpileExpr e2
    -- return $ e1s <> e2s <> [ MASM.Mod? Nothing ]
transpileExpr (Equal e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    addCycles eqCycles
    return $ e1s <> e2s <> [ MASM.Eq Nothing ]
transpileExpr (Lower e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    addCycles ltCycles
    return $ e1s <> e2s <> [ MASM.Lt ]
transpileExpr (LowerEq e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    addCycles lteCycles
    return $ e1s <> e2s <> [ MASM.Lte ]
transpileExpr (Greater e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    addCycles gtCycles
    return $ e1s <> e2s <> [ MASM.Gt ]
transpileExpr (GreaterEq e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    addCycles gteCycles
    return $ e1s <> e2s <> [ MASM.Gte ]
transpileExpr (NFTypes.Not e) = do
    es <- transpileExpr e
    addCycles notCycles
    return $ es <> [ MASM.Not ]
transpileExpr (NFTypes.IsOdd e) = do
    es <- transpileExpr e
    return $ es <> [ MASM.IsOdd ]
transpileExpr NextSecret = return . singleton . MASM.AdvPush $ 1

transpileExpr _ = error "transpileExpr::TODO"

-- | Helper function to increment the number of VM cycles in the state
addCycles :: Integer -> State Context ()
addCycles n = modify $ \ctx -> ctx { nbCycles = n + nbCycles ctx }

-- ** Execution cycles as advertised by Miden VM's doc

pushCycles = 1
mem_loadCycles = 1
mem_loadCycles' = 2
mem_storeCycles = 2
mem_storeCycles' = 4
add_Cycles = 1
add_Cycles' = 2
subCycles = 2
subCycles' = 2
mulCycles = 1
mulCycles' = 2
divCycles = 2
divCycles' = 2
eqCycles = 1
eqCycles' = 2
ltCycles = 17
lteCycles = 18
gtCycles = 18
gteCycles = 19
notCycles = 1
dropCycles = 1