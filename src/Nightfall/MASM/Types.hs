{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nightfall.MASM.Types
    ( module Nightfall.MASM.Types
    , StackIndex
    , unStackIndex
    , toStackIndex
    , unsafeToStackIndex
    , MemoryIndex
    , unMemoryIndex
    , toMemoryIndex
    , toMemoryIndexBelow
    , unsafeToMemoryIndex
    ) where

import Nightfall.Alphabet
import Nightfall.Lang.Types
import Nightfall.MASM.Integral

import Control.Monad.Writer.Strict
import qualified Data.DList as DList
import qualified GHC.Exts

type ProcName = Text

type AdvOpName = Text

type ModName = Text

data Module = Module
  { _moduleImports :: [ModName],
    _moduleProcs :: Map ProcName Proc,
    _moduleProg :: Program,
    -- Please keep in sync with 'ZKProgram'.
    _moduleSecretInputs :: Either SecretInputs FilePath
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

data Proc = Proc
  { _procNLocals :: Int,
    _procInstrs :: [Instruction]
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

newtype Program = Program {programInstrs :: [Instruction]}
  deriving (Eq, Ord, Show, Semigroup, Generic, Typeable)

data Instruction
  = Exec ProcName -- exec.foo
  | Adv AdvOpName -- adv.foo
  | If
      { -- if.true
        thenBranch :: [Instruction],
        elseBranch :: [Instruction]
      }
  | While [Instruction] -- while.true
  | Repeat Natural [Instruction] -- repeat.count
  | AdvPush (StackIndex 1 16)  -- adv_push.n
  | Push [Felt] -- push.a.b.c
  | Padw -- padw
  | Swap (StackIndex 1 15) -- swap[.i]
  | Drop -- drop
  | Dropw -- dropw
  | CDrop -- cdrop
  | Dup (StackIndex 0 15) -- dup.n
  | MoveUp (StackIndex 2 15) -- movup.n
  | MoveDown (StackIndex 2 15) -- movdn.n
  | TruncateStack -- exec.sys::truncate_stack
  | SDepth -- sdepth
  | Eq (Maybe Felt) -- eq[.n]
  | NEq (Maybe Felt) -- neq[.n]
  | Lt -- lt
  | Lte -- lte
  | Gt -- gt
  | Gte -- gte
  | Not -- not
  | IsOdd -- is_odd
  | LocStore MemoryIndex -- loc_store.i
  | LocLoad MemoryIndex -- loc_load.i
  | MemLoad (Maybe MemoryIndex) -- mem_load[.i]
  | MemLoadw (Maybe MemoryIndex) -- mem_loadw[.i]
  | MemStore (Maybe MemoryIndex) -- mem_store[.i]
  | MemStorew (Maybe MemoryIndex) -- mem_storew[.i]
  | Add (Maybe Felt) -- add[.n]
  | Sub (Maybe Felt) -- sub[.n]
  | Mul (Maybe Felt) -- mul[.n]
  | Div (Maybe Felt) -- div[.n]
  | Neg
  | IAdd -- u32checked_add
  | ISub -- "u32checked_sub"
  | IMul -- u32checked_mul
  | IDiv -- u32checked_div
  | IMod -- u32checked_mod
  | IDivMod (Maybe Word32) -- u32checked_divmod
  | IMax
  | IShL
  | IShR -- u32checked_{shl, shr}
  | IAnd
  | IOr
  | IXor
  | INot -- u32checked_{and, or, xor, not}
  | IEq (Maybe Word32)
  | INeq -- u32checked_{eq[.n], neq}
  | ILt
  | IGt
  | ILte
  | IGte -- u32checked_{lt[e], gt[e]}
  | IRotl
  | IRotr
  | IPopcnt -- u32checked_popcnt
  | -- "faked 64 bits" operations, u64::checked_{add,sub,mul}
    IAdd64
  | ISub64
  | IMul64
  | IDiv64
  | IMod64
  | IShL64
  | IShR64
  | IOr64
  | IAnd64
  | IXor64
  | IEq64
  | IEqz64
  | INeq64
  | ILt64
  | IGt64
  | ILte64
  | IGte64
  | IRotl64
  | IRotr64
  | IAdd256
  | Assert
  | AssertZ
  | Comment Text
  | EmptyL     -- ability to insert empty line for spacing (purely decorative and for easier inspection)
  deriving (Eq, Ord, Show, Generic, Typeable)

newtype PpMASM a = PpMASM {runPpMASM :: Writer (DList.DList String) a}
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance MonadWriter (DList.DList String) PpMASM

instance (a ~ ()) => IsString (PpMASM a) where
  fromString s = tell $ pure s

instance (a ~ ()) => GHC.Exts.IsList (PpMASM a) where
  type Item (PpMASM a) = String
  fromList = tell . DList.fromList
  toList = DList.toList . snd . runWriter . runPpMASM

$(foldMapA makeLenses [''Proc, ''Module])
