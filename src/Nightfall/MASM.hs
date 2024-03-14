{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Nightfall.MASM where

import Nightfall.Alphabet
import Nightfall.MASM.Callgraph
import Nightfall.MASM.Types

import Control.Monad.Writer.Strict

import qualified Data.DList as DList
import Data.Text (pack, unpack)

comment :: Show a => a -> Instruction
comment = Comment . pack . show

accessibleStackDepth :: Int
accessibleStackDepth = 16

indent :: PpMASM a -> PpMASM a
indent = censor . fmap $ \s -> if null s then "" else "  " ++ s

ppMASM :: Module -> String
ppMASM = unlines . toList . execWriter . runPpMASM . ppModule

runPPMasm :: PpMASM a -> String
runPPMasm = unlines . toList . execWriter . runPpMASM

ppModule :: Module -> PpMASM ()
ppModule m = do
  -- TODO: pretty-print the inputs too, somehow? Even though we don't strictly need that, since
  -- 'runMiden' pretty-prints inputs itself anyway. Maybe at least as a comment in the code or
  -- or something?
  tell $ DList.fromList $ fmap (("use."++) . unpack) (_moduleImports m)
  traverse_ ppProc . sortProcs $ _moduleProcs m
  ppProgram (_moduleProg m)

ppProc :: (Text, Proc) -> PpMASM ()
ppProc (name, p) = do
  [ "proc." ++ unpack name ++ "." ++ show (_procNLocals p) ]
  indent $ traverse_ ppInstr (_procInstrs p)
  "end"

ppProgram :: Program -> PpMASM ()
ppProgram p = do
  "begin"
  indent $ traverse_ ppInstr (programInstrs p)
  "end"

ppInstr :: Instruction -> PpMASM ()
ppInstr (Exec pname) = [ "exec." ++ unpack pname ]
ppInstr (Adv name) = [ "adv." ++ unpack name ]
ppInstr (If {thenBranch, elseBranch}) = do
  "if.true"
  indent $ traverse_ ppInstr thenBranch
  unless (null elseBranch) $ do
    "else"
    indent $ traverse_ ppInstr elseBranch
  "end"
ppInstr (While body) = do
  "while.true"
  indent $ traverse_ ppInstr body
  "end"
ppInstr (Repeat count body) = do
  [ "repeat." ++ show count ]
  indent $ traverse_ ppInstr body
  "end"

ppInstr (LocStore n) = [ "loc_store." ++ show n ]
ppInstr (LocLoad n) = [ "loc_load." ++ show n ]

ppInstr (AdvPush n) = [ "adv_push." ++ show n ]
ppInstr (Push ns) = [ "push" ++ concatMap (\n -> '.' : show n) ns ]
ppInstr Padw = [ "padw" ]
ppInstr (Swap n) = [ "swap" ++ if unStackIndex n == 1 then "" else "." ++ show n ]
ppInstr Drop = "drop"
ppInstr Dropw = "dropw"
ppInstr CDrop = "cdrop"
ppInstr (Dup n) = [ "dup." ++ show n ]
ppInstr (MoveUp n) = [ "movup." ++ show n ]
ppInstr (MoveDown n) = [ "movdn." ++ show n ]
ppInstr TruncateStack = "exec.sys::truncate_stack"
ppInstr SDepth = "sdepth"
ppInstr (Eq Nothing) = "eq"
ppInstr (Eq (Just n)) = [ "eq." ++ show n ]
ppInstr (NEq Nothing) = "neq"
ppInstr (NEq (Just n)) = [ "neq." ++ show n ]
ppInstr Not = "not"
ppInstr Lt = "lt"
ppInstr Lte = "lte"
ppInstr Gt = "gt"
ppInstr Gte = "gte"
ppInstr IsOdd = "is_odd"
ppInstr (Add Nothing) = "add"
ppInstr (Add (Just n)) = [ "add." ++ show n ]
ppInstr (Sub Nothing) = "sub"
ppInstr (Sub (Just n)) = [ "sub." ++ show n ]
ppInstr (Mul Nothing) = "mul"
ppInstr (Mul (Just n)) = [ "mul." ++ show n ]
ppInstr (Div Nothing) = "div"
ppInstr (Div (Just n)) = [ "div." ++ show n ]

ppInstr Neg = "neg"
ppInstr IAdd = "u32wrapping_add"
ppInstr ISub = "u32wrapping_sub"
ppInstr IMul = "u32wrapping_mul"
ppInstr IDiv = "u32checked_div"
ppInstr IMod = "u32checked_mod"
ppInstr (IDivMod mk) = [ "u32checked_divmod" ++ maybe "" (\k -> "." ++ show k) mk ]
ppInstr IMax = "u32checked_max"
ppInstr IShL = "u32checked_shl"
ppInstr IShR = "u32checked_shr"
ppInstr (IEq mk) = [ "u32checked_eq" ++ maybe "" (\k -> "." ++ show k) mk ]
ppInstr INeq = "u32checked_neq"
ppInstr ILt = "u32checked_lt"
ppInstr IGt = "u32checked_gt"
ppInstr ILte = "u32checked_lte"
ppInstr IGte = "u32checked_gte"
ppInstr IAnd = "u32checked_and"
ppInstr IOr = "u32checked_or"
ppInstr IXor = "u32checked_xor"
ppInstr INot = "u32checked_not"
ppInstr IRotl = "u32checked_rotl"
ppInstr IRotr = "u32checked_rotr"
ppInstr IPopcnt = "u32checked_popcnt"

ppInstr (MemLoad mi) = [ "mem_load" ++ maybe "" (\i -> "." ++ show i) mi ]
ppInstr (MemLoadw mi) = [ "mem_loadw" ++ maybe "" (\i -> "." ++ show i) mi ]
ppInstr (MemStore mi) = [ "mem_store" ++ maybe "" (\i -> "." ++ show i) mi ]
ppInstr (MemStorew mi) = [ "mem_storew" ++ maybe "" (\i -> "." ++ show i) mi ]
ppInstr IAdd64 = "exec.u64::wrapping_add"
ppInstr ISub64 = "exec.u64::wrapping_sub"
ppInstr IMul64 = "exec.u64::wrapping_mul"
ppInstr IDiv64 = "exec.u64::checked_div"
ppInstr IMod64 = "exec.u64::checked_mod"
ppInstr IEq64 = "exec.u64::checked_eq"
ppInstr INeq64 = "exec.u64::checked_neq"
ppInstr IEqz64 = "exec.u64::checked_eqz"
ppInstr ILt64 = "exec.u64::checked_lt"
ppInstr IGt64 = "exec.u64::checked_gt"
ppInstr ILte64 = "exec.u64::checked_lte"
ppInstr IGte64 = "exec.u64::checked_gte"
ppInstr IShL64 = "exec.u64::unchecked_shl"
ppInstr IShR64 = "exec.u64::unchecked_shr"
ppInstr IOr64 = "exec.u64::checked_or"
ppInstr IAnd64 = "exec.u64::checked_and"
ppInstr IXor64 = "exec.u64::checked_xor"
ppInstr IRotl64 = "exec.u64::unchecked_rotl"
ppInstr IRotr64 = "exec.u64::unchecked_rotr"

ppInstr IAdd256 = "exec.u256::add_unsafe"

ppInstr Assert = "assert"
ppInstr AssertZ = "assertz"

ppInstr (Comment cmt) = [ "# " ++ unpack cmt ]
ppInstr EmptyL = ""
