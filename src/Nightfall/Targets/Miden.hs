{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nightfall.Targets.Miden ( dynamicMemoryHead
                               , Context(..)
                               , defaultContext
                               , Config(..)
                               , defaultConfig
                               , VarInfo(..)
                               , onStack
                               , decorateM
                               , decorate
                               , deref
                               , derefw
                               , deref256
                               , storeNat
                               , loadNat
                               , toHashModule
                               , transpileZKProgram
                               , Transpile(..)
                               , transpileNoAsm
                               , cgfTraceVariablesDecl
                               , cfgTraceVariablesUsage
                               , varInfoType
                               , varInfoPos
                               , progName
                               , statMemPos
                               , mayDynMemPtr
                               , variables
                               , adviceMapExt
                               , importExt
                               , config
                               ) where

import Nightfall.Alphabet
import Nightfall.Lang.Internal.Types as NFTypes
import qualified Nightfall.Lang.Syntax.Default as Syntax
import Nightfall.Lang.Types
import Nightfall.MASM.Miden
import Nightfall.MASM.Types as MASM

import Control.Lens ((.=), (%=), (?=), use)
import Control.Monad.State
import System.IO.Unsafe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as SText
import qualified Data.Text as Strict (Text)

-- | Determines where the dynamic part of memory starts. We need to separate memory into static and
-- dynamic, because the former is more efficient but not always sufficient: it's best to allocate a
-- 'Felt' variable statically and directly load it onto the stack when necessary, but it's only
-- possible because we know how much space a 'Felt' value consumes in memory. A 'Natural' is
-- different: we don't know statically how much space an arbitrary computed at runtime natural is
-- going to consume, hence we statically allocate a pointer to a blob of dynamic memory and follow
-- the pointer at runtime, which is an additional indirection, but a necessary one.
--
-- This is just a broke man runtime memory management system with no ability to release memory.
-- Long-term, we should implement something proper.
dynamicMemoryHead :: Felt
dynamicMemoryHead = fromInteger $ 10 ^! 8

toStaticMemoryIndex :: Integer -> Maybe MemoryIndex
toStaticMemoryIndex = toMemoryIndexBelow $ unFelt dynamicMemoryHead

-- | The name of the pointer to free dynamic memory.
dynPtrName :: VarName
dynPtrName = "__dynPtr"

-- | Transpilation configuration options
data Config = Config {
      _cgfTraceVariablesDecl :: Bool   -- ^ Whether or not adding comments when declaring variables
    , _cfgTraceVariablesUsage :: Bool  -- ^ Whether or not adding comments when using ("calling") variables
} deriving (Eq, Show)

data VarInfo = VarInfo
    { _varInfoType :: VarType
    , _varInfoPos  :: MemoryIndex
    }

-- TODO: a Note about 'adviceMapExt' and 'importExt'.
-- | Context for the transpilation, this is the state of the State Monad in which transpilation happens
data Context = Context
    { _progName :: String                   -- ^ Name of the program, for outputs logs, etc.
    , _statMemPos :: MemoryIndex            -- ^ Next free indice in Miden's global memory
    , _mayDynMemPtr :: Maybe MemoryIndex
    , _variables :: Map String VarInfo      -- ^ Variables in the EDSL are stored in Miden's random
                                            -- access memory, we keep a map to match them
    , _adviceMapExt :: Map Strict.Text [MidenWord]  -- ^ Entries to add to the inputs file
    , _importExt :: Set Strict.Text         -- ^ Imports to add to the final program
    , _config :: Config                     -- ^ Transpilation configuration options
    }

$(foldMapA makeLenses [''Context, ''VarInfo, ''Config])

defaultConfig :: Config
defaultConfig = Config
    { _cgfTraceVariablesDecl = False
    , _cfgTraceVariablesUsage = False
    }

defaultContext :: Context
defaultContext = Context
    { _progName = "<unnamed-program>"
    , _statMemPos = case toMemoryIndex 0 of
        Nothing  -> error "Internal error: wrong index for initial memory position"
        Just pos -> pos
    , _mayDynMemPtr = Nothing
    , _variables = Map.empty
    , _adviceMapExt = Map.empty
    , _importExt = Set.empty
    , _config = defaultConfig
    }

-- | A class for things that can be transpiled.
class Transpile a where
    -- Constraints required to transpile @a@ without any assembly in it. Just for convenience, so
    -- that we don't need to instantiate @asm@ type variables with 'Void' manually at each call
    -- site.
    type NoAsm a :: Constraint
    transpile :: a -> State Context [Instruction]

transpileNoAsm :: (NoAsm a, Transpile a) => a -> State Context [Instruction]
transpileNoAsm = transpile

instance Transpile Void where
    type NoAsm Void = ()
    transpile = \case{}

instance Transpile a => Transpile [a] where
    type NoAsm [a] = NoAsm a
    transpile = foldMapA transpile

instance Transpile Instruction where
    type NoAsm Instruction = ()
    transpile = pure . pure

instance Transpile a => Transpile (State Context a) where
    type NoAsm (State Context a) = NoAsm a
    transpile a = a >>= transpile

instance Transpile asm => Transpile (Expr_ asm) where
    type NoAsm (Expr_ asm) = asm ~ Void
    transpile = transpileExpr

instance Transpile asm => Transpile (Expr asm a) where
    type NoAsm (Expr asm a) = asm ~ Void
    transpile = transpile . unExpr

instance Transpile asm => Transpile (Statement_ asm) where
    type NoAsm (Statement_ asm) = asm ~ Void
    transpile = transpileStatement

instance (Transpile asm, a ~ ()) => Transpile (Body asm a) where
    type NoAsm (Body asm a) = asm ~ Void
    transpile = transpile . runBody

toHashModule :: Word32 -> [MidenWord] -> Module
toHashModule numWords midenWords =
      Module
          ["std::mem"]  -- @std::mem@ provides @mem::pipe_words_to_memory@ used above.
          Map.empty
          (Program instrs)
          (Left $ SecretInputs (midenWords >>= midenWordToFelts) Map.empty)
  where
      instrs =
          [ Push [0, fromIntegral numWords]
            -- Stack: [numWords, 0, <zeros>]
          , Exec "mem::pipe_words_to_memory"
            -- Stack: [Hash, memPos', <zeros>]
          ]

-- | Compute the hash of the padded inputs by invoking the Miden executable with the inputs put on
-- @advice_stack@ and subsequently loaded into memory via @exec.mem::pipe_words_to_memory@,
-- which results in the top word on the stack being the hash in reversed form (so we
-- additionally reverse it to get the actual hash).
getHash :: Word32 -> [MidenWord] -> Either String [Felt]
getHash numWords midenWords =
    -- TODO: should transpilation work in IO instead or can we treat 'runMiden' as "morally-pure"?
    fmap reverse . unsafePerformIO $ runMiden DontKeep (Just 4) $ toHashModule numWords midenWords

toNumWords :: VarType -> Word32
toNumWords VarFelt              = 1
toNumWords VarBool              = 1
toNumWords (VarArrayOfFelt len) = len
toNumWords VarNat               = 1

-- | Allocate memory for a variable.
alloc :: VarName -> VarType -> State Context MemoryIndex
alloc var varType = do
    vars <- use variables
    if Map.member var vars
        then error $ "Variable '" ++ var ++ "' has already been declared!"
        else do
            varPos <- use statMemPos
            let numWords = toNumWords varType
                mayStatMemPos' = toStaticMemoryIndex $ unMemoryIndex varPos + fromIntegral numWords
            statMemPos .= case mayStatMemPos' of
                Nothing          -> error "Out of Miden's memory"
                Just statMemPos' -> statMemPos'
            variables .= Map.insert var (VarInfo varType varPos) vars
            pure varPos

-- | Compile
--
-- > repeat.count
-- >   body
--
-- to
--
-- > var counter = count
-- > while (counter /= 0)
-- >   body
-- >   counter = counter - 1
repeatDynamic :: VarName -> Expr asm Felt -> Body asm () -> Body asm ()
repeatDynamic counterName count body = do
    counter <- Syntax.declare counterName count
    while (not' $ Syntax.get counter `eq` 0) $ do
        body
        Syntax.set counter $ Syntax.get counter - 1

-- | Entry point: transpile a EDSL-described ZK program into a Miden Module
transpileZKProgram :: Transpile asm => ZKProgramAsm asm -> State Context Module
transpileZKProgram zkProg = do
    warning <- transpileNoAsm $
        comment "This program was generated by Nightfall (https://github.com/qredo/nightfall), avoid editing by hand.\n"
    progName .= pName zkProg
    midenInstr <- transpile $ pBody zkProg
    ctx <- get
    return $ Module { _moduleImports = Set.toList $ _importExt ctx
                    , _moduleProcs = Map.empty -- No procs either (TODO)
                    , _moduleProg = Program (warning ++ midenInstr)
                    , _moduleSecretInputs = case pSecretInputs zkProg of
                        Left inputs -> Left $ inputs
                            { _adviceMap =
                                Map.unionWithKey
                                    (\hash _ _ -> error $ concat
                                        [ "a mapping for "
                                        , SText.unpack hash
                                        , " occurs twice"
                                        ])
                                    (_adviceMapExt ctx)
                                    (_adviceMap inputs)
                            }
                        -- TODO: no, we're not supposed to ignore inputs that the program needs just
                        -- because we expect some of them to be provided via a file.
                        Right inputsFile -> Right inputsFile
                    }

transpileStatement :: Transpile asm => Statement_ asm -> State Context [Instruction]
transpileStatement (NFTypes.Comment str) = return . singleton . MASM.Comment . SText.pack $ str
transpileStatement (IfElse cond ifBlock elseBlock) = do
    ifBlock' <- foldMapA transpileStatement ifBlock
    elseBlock' <- foldMapA transpileStatement elseBlock
    -- Not entirely sure, Miden talks about a "a small, but non-negligible overhead", but they don't
    -- say. I'm supposing it takes a pop/drop operation + a comparison
    cond' <- transpileExpr cond
    return $ cond' <> [ MASM.If ifBlock' elseBlock' ]
transpileStatement (NFTypes.Repeat count body) = do
    body' <- foldMapA transpileStatement body
    return $ [ MASM.Repeat count body' ]
transpileStatement (NFTypes.While cond body) = do
    body' <- foldMapA transpileStatement body
    cond' <- transpileExpr cond
    return $ cond' <> [ MASM.While $ body' <> cond' ]

-- | Declaring a variable loads the expression into Miden's global memory, and we track the index in memory
transpileStatement (DeclVariable varType var e) = do
    varPos <- alloc var varType
    -- Trace the variable declaration if configured
    traceVar <- do
        traceDecl <- use $ config . cgfTraceVariablesDecl
        pure [MASM.Comment $ "var " <> SText.pack var | traceDecl]
    -- Transpile the variable value
    e' <- transpileExpr e
    -- Return instruction for the variable value and instruction to store the value in global memory
    return $ e' <> traceVar <> [ MASM.MemStore $ Just varPos ]

-- | Assigning a new value to a variable if just erasing the memory location
transpileStatement (AssignVar var e) = do
    vars <- use variables
    shouldTrace <- use $ config . cfgTraceVariablesUsage
    -- Fetch the memory location for that variable.
    case Map.lookup var vars of
        Nothing -> error $ concat
            [ "Variable \""
            , var
            , "\" has not been declared before: can't assign value"
            ]
        Just varInfo -> do
            -- Trace the variable usage if configured
            let traceVar = [MASM.Comment $ "var " <> SText.pack var | shouldTrace]
            e' <- transpileExpr e
            return $ e' <> traceVar <> [ MASM.MemStore . Just $ _varInfoPos varInfo ]

transpileStatement (SetAt arr i val) = do
    vars <- use variables
    case Map.lookup arr vars of
        Nothing -> error $ "variable \"" ++ arr ++ "\" unknown (undeclared)"
        Just varInfo -> do
            unless (isArrayOfFelt $ _varInfoType varInfo) $
                error $ "'" ++ arr ++ "' is used as an array, but it's not one"
            let arrPos = unMemoryIndex $ _varInfoPos varInfo
            shouldTrace <- use $ config . cfgTraceVariablesUsage
            let traceVar =
                    [ MASM.Comment $ "an element of '" <> SText.pack arr <> "' number"
                    | shouldTrace
                    ]
            valInstrs <- transpileExpr val
            iInstrs <- transpileExpr i
            return $ concat
                [ traceVar
                , valInstrs
                , iInstrs
                , -- Add the position of the array to @i@ to make the top felt on the stack peek at
                  -- @arr[i]@. Unless the array starts at the very beginning of memory, in which
                  -- case do nothing.
                  [MASM.Add . Just $ fromInteger arrPos | arrPos /= 0]
                -- TODO: add (optional) bounds checking.
                , [MemStore Nothing]
                ]

-- | A (naked) function call is done by pushing the argument on the stack and caling the procedure name
transpileStatement (NakedCall fname args) = do
    args' <- foldMapA transpileExpr args
    return $ args' <> [ MASM.Exec . SText.pack $ fname ]

transpileStatement (InitArray arr inputs) = do
    let numWords = genericLength inputs
        inputsAsWords = padFeltsAsMidenWords inputs
    arrPos <- alloc arr $ VarArrayOfFelt numWords
    let hash = either error id $ getHash numWords inputsAsWords
    -- @std::mem@ provides @mem::pipe_preimage_to_memory@ used below.
    importExt %= Set.insert "std::mem"
    adviceMapExt %= case feltsToMidenWord hash of
        Nothing       -> error $ "Panic: a hash is not a Word: " ++ show hash
        Just hashWord -> Map.insert
            (midenWordToHexKey hashWord)
            inputsAsWords

    traceVar <- do
        traceUsage <- use $ config . cfgTraceVariablesUsage
        pure [MASM.Comment $ "var " <> SText.pack arr | traceUsage]
    pure $ traceVar ++
        [ Push hash
          -- Stack: [Hash, <rest>]
        , -- Look @hash@ up in @advice_map@ and put the associated felts onto @advice_stack@.
          Adv "push_mapval"
          -- Stack: [Hash, <rest>]
        , Push [fromIntegral $ unMemoryIndex arrPos, fromIntegral numWords]
          -- Stack: [numWords, arrPos, Hash, <rest>]
        , -- Load the felts (@numWords@ of them, i.e. all that were put there by @adv.push_mapval@)
          -- from @advice_stack@ into memory at @arrPos@ and check that they hash to the same @hash@
          -- that we computed above via 'runMiden'.
          Exec "mem::pipe_preimage_to_memory"
          -- Stack: [memPos', <rest>]
        , -- Drop the memory position (right after the last element of the array) remaining on the
          -- stack.
          Drop
          -- Stack: [<rest>]
        ]

transpileStatement (Return e) = transpileExpr e

transpileStatement EmptyLine = return . singleton $ MASM.EmptyL

-- | Transpile an unary operation.
transpileUnOp :: UnOp -> [Instruction]
transpileUnOp NFTypes.Not   = [ MASM.Not   ]
transpileUnOp NFTypes.IsOdd = [ MASM.IsOdd ]

-- | An expression representing the top element of the stack.
onStack :: Expr (State Context [Instruction]) a
onStack = assembly $ pure []

-- | Decorate an expression with effectful computations returning pre- and post-assembly.
decorateM
    :: asm ~ State Context [Instruction]
    => asm -> Expr asm a -> asm -> Expr asm b
decorateM asmPre expr asmPost = assembly $ foldA [asmPre, transpile expr, asmPost]

-- | Decorate an expression with pre- and post-assembly.
decorate
    :: asm ~ State Context [Instruction]
    => [Instruction] -> Expr asm a -> [Instruction] -> Expr asm b
decorate asmPre expr asmPost = decorateM (pure asmPre) expr (pure asmPost)

-- | Dereference a Nightfall pointer to a 'Felt'
deref :: asm ~ State Context [Instruction] => Expr asm Felt -> Expr asm Felt
deref ptr = decorate [] ptr [MemLoad Nothing]

-- | Dereference a Nightfall pointer to a 'MidenWord'.
derefw :: asm ~ State Context [Instruction] => Expr asm Felt -> Expr asm MidenWord
derefw ptr = decorate [Padw] ptr [MemLoadw Nothing]

-- | Dereference a Nightfall pointer to a 'Word256'.
deref256 :: asm ~ State Context [Instruction] => Expr asm Felt -> Expr asm Word256
deref256 ptr = decorate [] (derefw ptr) [Padw]

-- TODO: @counter1@ and @counter2@ are ridiculous names. We should either having scoping or
-- shadowing or an ability to generate a fresh name out of the given text or all of that.
-- | Store a 'Natural' in free memory, given the number of 'Felt's to take off of the stack
-- (there are no limitations on what that number can be).
storeNat :: asm ~ State Context [Instruction] => Expr asm Felt -> Body asm ()
storeNat i = do
    let dynPtr = Syntax.Binding Syntax.Felt dynPtrName
    elPtr <- Syntax.declare "elPtr" $ Syntax.get dynPtr + i
    Syntax.set dynPtr $ Syntax.get elPtr + 1
    -- Store limbs one by one in memory starting with the last one (limbs are stored in big-endian
    -- on the stack and little-endian in memory).
    repeatDynamic "counter2" i $ do
        ret $ Syntax.get elPtr
        ret . assembly $ pure [MemStorew Nothing, Dropw]
        Syntax.set elPtr $ Syntax.get elPtr - 1
    -- Store the number of limbs.
    ret i
    ret $ Syntax.get elPtr
    ret . assembly $ pure [MemStore Nothing]
    -- Put the pointer to the natural onto the stack.
    ret $ Syntax.get elPtr

-- | Load a 'Natural' from memory onto the stack, taking the pointer to the natural from the stack.
loadNat :: Body (State Context [Instruction]) ()
loadNat = do
    natPtr <- Syntax.declare "natPtr2" onStack
    repeatDynamic "counter3" (deref $ Syntax.get natPtr) $ do
        Syntax.set natPtr $ Syntax.get natPtr + 1
        ret . derefw $ Syntax.get natPtr

-- | Transpile a binary operation.
transpileBinOp :: BinOp -> State Context [Instruction]
-- Arithmetics operations are matched to their corresponding Miden operations
transpileBinOp NFTypes.Add    = pure [MASM.Add Nothing]
transpileBinOp NFTypes.Sub    = pure [MASM.Sub Nothing]
transpileBinOp NFTypes.Mul    = pure [MASM.Mul Nothing]
transpileBinOp NFTypes.Div    = pure [MASM.Div Nothing]
transpileBinOp NFTypes.IDiv32 = pure [MASM.IDiv]
transpileBinOp NFTypes.IMax32 = pure [MASM.IMax]
transpileBinOp NFTypes.Add256 = do
    importExt %= Set.insert "std::math::u256"
    pure [MASM.IAdd256]
-- TODO: this should be a Nightfall function. As well as 'storeNat', 'loadNat' etc.
transpileBinOp NFTypes.AddNat = transpile $ do
    yPtr <- Syntax.declare "yPtr" onStack
    xPtr <- Syntax.declare "xPtr" onStack
    ySize <- Syntax.declare "ySize" . deref $ Syntax.get yPtr
    xSize <- Syntax.declare "xSize" . deref $ Syntax.get xPtr
    -- The size of the resulting natural initialized at @1@.
    zSize <- Syntax.declare "i" 1
    -- Store a 128-bit @carry@ variable tracking 128-bit overflows on the stack. After each loop
    -- iteration it's a Miden Word that is equal to either 0 or 1.
    ret . assembly $ pure [Padw]  -- @carry@
    repeatDynamic "counter1" (binOp IMax32 (Syntax.get xSize) (Syntax.get ySize)) $ do
        -- Pad the 128-bit @carry@ to 256 bit.
        ret . assembly $ pure [Padw]
        -- If @x@ still has limbs, then add the current one to @carry@.
        simpleIf (Syntax.get zSize `lte` Syntax.get xSize) $
           ret $ add256 (deref256 $ Syntax.get xPtr + Syntax.get zSize) onStack
        -- If @y@ still has limbs, then add the current one to @carry@.
        simpleIf (Syntax.get zSize `lte` Syntax.get ySize) $
           ret $ add256 (deref256 $ Syntax.get yPtr + Syntax.get zSize) onStack
        -- Increase the size of the resulting natural.
        Syntax.set zSize $ Syntax.get zSize + 1
    -- Drop the 3 zeroes of @carry@.
    ret . assembly $ pure [Drop, Drop, Drop]
    -- If what remains on the stack is 1
    ifElse onStack
        -- then we had an overflow and need to restore the zeroes, because they and 1 will be
        -- stored in memory.
        (ret . assembly $ pure [Push [1, 0, 0, 0]])
        -- Otherwise there was no overflow and the size of the result needs to be decreased.
        (Syntax.set zSize $ Syntax.get zSize - 1)
    -- Move all of the computed limbs from stack to memory.
    storeNat $ Syntax.get zSize
transpileBinOp Equal          = pure [MASM.Eq Nothing]
transpileBinOp Lower          = pure [MASM.Lt]
transpileBinOp LowerEq        = pure [MASM.Lte]
transpileBinOp Greater        = pure [MASM.Gt]
transpileBinOp GreaterEq      = pure [MASM.Gte]

transpileLiteral :: Literal -> State Context [Instruction]
transpileLiteral (LiteralFelt x) = return . singleton $ Push [x]
transpileLiteral (LiteralBool b) = return . singleton $ Push [if b then 1 else 0]
-- TODO: should be a Nightfall function too? Should be defined in terms of 'storeNat'?
transpileLiteral (LiteralNat nat) = do
    -- Instructions to initialize the pointer to the dynamic memory, if it's not already.
    instrsInitDynPtr <- use mayDynMemPtr >>= \case
        Just _  -> pure []
        Nothing -> do
            dynPtr <- use statMemPos
            mayDynMemPtr ?= dynPtr
            transpileNoAsm $ DeclVariable VarFelt dynPtrName . unExpr $ lit dynamicMemoryHead
    -- Instructions to get the value of the pointer to the dynamic memory.
    instrsGetDynPtr <- transpileNoAsm $ Var dynPtrName
    -- Instructions to increase the value of the pointer to the dynamic memory.
    instrsIncDynPtr <- transpileNoAsm . AssignVar dynPtrName $
        BinOp NFTypes.Add (Var dynPtrName) (Literal $ LiteralFelt 1)
    let natWords = naturalToMidenWords nat
        -- Instructions to store the size of the natural.
        instrsSize = concat
            [ [Push [fromIntegral $ length natWords]]
            , instrsGetDynPtr
            , [MemStore Nothing]
            , instrsIncDynPtr
            ]
        -- Instructions to store the limbs of the natural.
        instrsLimbs = do
            mword <- natWords
            -- TODO: this can be optimized, we don't really need to update 'dynPtr' in memory at
            -- each iteration.
            concat
                [ [Push $ midenWordToFelts mword]
                , instrsGetDynPtr
                , [MemStorew Nothing, Dropw]
                , instrsIncDynPtr
                ]
    pure $ instrsInitDynPtr ++ instrsGetDynPtr ++ instrsSize ++ instrsLimbs

-- TODO: range check, etc.
transpileExpr :: Transpile asm => Expr_ asm -> State Context [Instruction]
transpileExpr (Assembly asm) = transpile asm

-- Literals are simply pushed onto the stack
transpileExpr (Literal l) = transpileLiteral l

transpileExpr (Var varname) = do
    -- Fetch the memory location of that variable in memory, and push it to the stack
    vars <- use variables
    case Map.lookup varname vars of
        Nothing -> error $ "variable '" ++ varname ++ "' unknown (undeclared)"
        Just varInfo -> do
            shouldTrace <- use $ config . cfgTraceVariablesUsage
            let traceVar =
                    [ MASM.Comment . SText.pack $ fold
                        ["var ", varname, " (", ppVarType $ _varInfoType varInfo, ")"]
                    | shouldTrace
                    ]
            return $ traceVar <> [MemLoad . Just $ _varInfoPos varInfo]

transpileExpr (GetAt arr i) = do
    vars <- use variables
    case Map.lookup arr vars of
        Nothing -> error $ "Array variable \"" ++ arr ++ "\" unknown (undeclared)"
        Just varInfo -> do
            unless (isArrayOfFelt $ _varInfoType varInfo) $
                error $ "'" ++ arr ++ "' is used as an array, but it's not one"
            let arrPos = unMemoryIndex $ _varInfoPos varInfo
            shouldTrace <- use $ config . cfgTraceVariablesUsage
            let traceVar =
                    [ MASM.Comment $ "an element of '" <> SText.pack arr <> "' number"
                    | shouldTrace
                    ]
            iInstrs <- transpileExpr i
            return $ concat
                [ traceVar
                , iInstrs
                , -- Add the position of the array to @i@ to make the top felt on the stack peek at
                  -- @arr[i]@. Unless the array starts at the very beginning of memory, in which
                  -- case do nothing.
                  [MASM.Add . Just $ fromInteger arrPos | arrPos /= 0]
                -- TODO: add (optional) bounds checking.
                , [MemLoad Nothing]
                ]

transpileExpr (UnOp op e) = do
    es <- transpileExpr e
    return $ es <> transpileUnOp op
transpileExpr (BinOp op e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    ops <- transpileBinOp op
    return $ e1s <> e2s <> ops

transpileExpr NextSecret =
    case toStackIndex 1 of
        Nothing  -> error "Internal error: wrong index for 'AdvPush'"
        Just idx -> return . singleton . MASM.AdvPush $ idx

transpileExpr FCall{} = error "transpileExpr::TODO"
