{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nightfall.MASM.Integral where

import Nightfall.Alphabet

-- | The type of indices into Miden's stack, each element is greater than or equal to @lower@ and
-- less than or equal to @upper@.
newtype StackIndex (lower :: Nat) (upper :: Nat) = StackIndex Word8
    deriving stock (Generic, Typeable)
    deriving newtype (Eq, Ord, Show)

-- | Convert a 'StackIndex' to the corresponding 'Integer'.
unStackIndex :: StackIndex lower upper -> Integer
unStackIndex = coerce (toInteger :: Word8 -> Integer)

-- See Note [Handling of integral types].
-- | Unwrap a 'StackIndex' to get the underlying 'Word8'. This is only unsafe in the sense that
-- performing operations over the resulting 'Word8' may result in overflow and hence usage of
-- 'unsafeStackIndex' is discouraged. Use 'unStackIndex' whenever possible instead.
unsafeStackIndex :: StackIndex lower upper -> Word8
unsafeStackIndex = coerce

-- | Convert an 'Integer' to the corresponding 'StackIndex'. Returns 'Nothing' if the integer
-- doesn't fit into 'StackIndex'.
toStackIndex ::
      forall lower upper. (KnownNat lower, KnownNat upper)
    => Integer -> Maybe (StackIndex lower upper)
toStackIndex i
    | lower <= i && i <= upper = Just . StackIndex $ fromInteger i
    | otherwise                = Nothing
    where
        lower = toInteger . natVal $ Proxy @lower
        upper = toInteger . natVal $ Proxy @upper

-- See Note [Handling of integral types].
-- | Convert an 'Integer' to the corresponding 'StackIndex'. Silently overflows if the integer
-- doesn't fit into 'StackIndex'. Use 'toStackIndex' whenever possible instead.
unsafeToStackIndex :: Integer -> StackIndex lower upper
unsafeToStackIndex = coerce (fromInteger :: Integer -> Word8)

-- | The type of indices in Miden's global memory (accessed via @mem_load@, @mem_store@ etc).
newtype MemoryIndex = MemoryIndex Word32
    deriving stock (Generic, Typeable)
    deriving newtype (Eq, Ord, Show)

-- | Convert a 'MemoryIndex' to the corresponding 'Integer'.
unMemoryIndex :: MemoryIndex -> Integer
unMemoryIndex = coerce (toInteger :: Word32 -> Integer)

-- See Note [Handling of integral types].
-- | Unwrap a 'MemoryIndex' to get the underlying 'Word32'. This is only unsafe in the sense that
-- performing operations over the resulting 'Word32' may result in overflow and hence usage of
-- 'unsafeMemoryIndex' is discouraged. Use 'unMemoryIndex' whenever possible instead.
unsafeUnMemoryIndex :: MemoryIndex -> Word32
unsafeUnMemoryIndex = coerce

-- | Convert an 'Integer' to the corresponding 'MemoryIndex'. Returns 'Nothing' if the integer
-- doesn't fit into 'MemoryIndex'.
--
-- >>> import Data.Word
-- >>> let m = toInteger (maxBound :: Word32)
-- >>> map toMemoryIndex [-42, 0, 42, m, m + 1]
-- [Nothing,Just 0,Just 42,Just 4294967295,Nothing]
toMemoryIndex :: Integer -> Maybe MemoryIndex
toMemoryIndex = coerce (toIntegralSized :: Integer -> Maybe Word32)

-- | Convert an 'Integer' to the corresponding 'MemoryIndex'. Returns 'Nothing' if the integer
-- doesn't fit into 'MemoryIndex' or is strictly less than the given threshold.
toMemoryIndexBelow :: Integer -> Integer -> Maybe MemoryIndex
toMemoryIndexBelow maxExcl n = do
    guard $ n < maxExcl
    toMemoryIndex n

-- See Note [Handling of integral types].
-- | Convert an 'Integer' to the corresponding 'MemoryIndex'. Silently overflows if the integer
-- doesn't fit into 'MemoryIndex'. Use 'toMemoryIndex' whenever possible instead.
unsafeToMemoryIndex :: Integer -> MemoryIndex
unsafeToMemoryIndex = coerce (fromInteger :: Integer -> Word32)
