{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Nightfall.Lang.Internal.Felt where

import Nightfall.Alphabet

import Data.Mod

{- Note [Handling of integral types]
Without completely paranoic handling of integral types it is trivial to cause an overflow, which may
easily result in a vulnerability. We therefore 'newtype'-wrap every integral type (including the
type of field elements), specify its range (sometimes as right at the type level, sometimes as a
comment) and discourage usage of functions that operate on the underlying representation by
attaching the "unsafe" prefix to their name.

Any wrapping is only considered safe when either any input is handled correctly or bounds are
checked and 'Nothing' is returned if the provided value doesn't fit. Thus both throwing and implicit
conversions between types of different cardinality are considered unsafe, although between the two
unsafe options the former is highly preferable.

Any unwrapping is only considered safe as long as the result is converted to 'Integer' before the
unwrapping function returns, so that the user cannot accidentally cause an overflow using the
result of the unwrapping function and subsequently "safely" embed that overflow back into the
integral type.
-}

-- | The order (a.k.a. size) of the 'Felt' field.
type FeltOrder = 2 ^ 64 - 2 ^ 32 + 1

-- | The order (a.k.a. size) of the 'Felt' field as a 'Natural'.
feltOrder :: Natural
feltOrder = fromIntegral . natVal $ Proxy @FeltOrder
-- No point in inlining this definition, it would be neither efficient nor readable in the generated
-- Core.
{-# NOINLINE feltOrder #-}

-- | The order (a.k.a. size) of the 'Felt' field as an 'Integer', provided for solely for the
-- purpose of not recomputing the 'Integer' version of 'feltOrder'.
feltOrderInteger :: Integer
feltOrderInteger = fromIntegral feltOrder
{-# NOINLINE feltOrderInteger #-}

{- Note [Felt representation]
Possible options for the underlying representation of 'Felt' are

- 'Word64'
- 'Natural'
- @Mod FeltOrder@
- @Galois.Prime FeltOrder@
- 'Integer'

with @Galois.Prime FeltOrder@ being a @newtype@ wrapper around @Mod FeltOrder@ and the latter being
a @newtype@ wrapper around 'Natural'.

We use @Mod FeltOrder@ as the underlying representation of 'Felt'. Here is why not others:

- We don't use 'Word64', because it's inefficient. We can't do better than 'Natural'
  performance-wise, because we need to be able to handle, say, @(2 * Felt (maxBound :: Word64))@
  somehow, i.e. an intermediate step can go well beyond 'Word64' before the 'mod' operation is
  executed and the result is back within 'Word64'. Plus, using 'Natural' is simply safer as it
  doesn't pose any risk of causing an overflow. There's still risk of underflow, but at least that
  causes an exception with 'Natural'. Plus, 'Felt' is strictly smaller than 'Word64' and as such
  doesn't provide any benefit over using 'Natural'.
  It is however the case that 'Word64' values can be consecutively put in a 'PrimArray' without any
  pointer indirections, however since we know that 'Felt' fits into a 'Word64' we can always
  implement the right 'Prim' instance for the type and put 'Felt's into a 'PrimArray' despite the
  fact that they are 'Natural's.
- We don't use 'Natural' directly, because @Mod FeltOrder@ is 'Natural' plus all the operations that
  we need such as those from the 'Num' and  'Fractional' classes. No need to implement them manually
  ourselves given that they're provided via a library.
- We don't use @Galois.Prime FeltOrder@, because the API of 'Galois.Prime' doesn't seem to provide
  direct conversion from/to 'Natural' and doesn't expose the constructor of the @newtype@ thereby
  limiting performance for absolutely no reason other than "we know better what you need dummy".
- We don't use 'Integer', because conversions to/from @Mod FeltOrder@ implementing all the
  operations for us would be expensive and given Note [Handling of integral types] we should be able
  to afford the minor unsafety of 'Natural' here, since we're sufficiently paranoic when it comes to
  constructing and deconstructing 'Felt's.
-}

-- See Note [Felt representation].
-- | The type of finite field elements that the Miden VM operates on.
-- Use 'fromInteger' or numeric syntax to construct a value of this type.
--
-- Invariant:
--
-- > forall (i :: Felt). 0 <= i && i < feltOrder
newtype Felt = Felt (Mod FeltOrder)
    deriving stock (Generic, Typeable)
    deriving newtype (Eq, Ord, Num, Fractional, Enum, Bounded)

-- Implementing manually, because 'Show' for 'Mod' renders the order of the field, which is only
-- extra noise for us.
instance Show Felt where
    showsPrec pr = showsPrec pr . unFelt

instance Read Felt where
    readsPrec pr = map (first fromInteger) . readsPrec pr

-- | Convert a 'Felt' to the corresponding 'Integer'.
unFelt :: Felt -> Integer
unFelt = coerce $ toInteger . unMod
{-# INLINE unFelt #-}

-- See Note [Handling of integral types].
-- | Unwrap a 'Felt' to get the underlying 'Natural'. This is only unsafe in the sense that
-- performing operations over the resulting 'Natural' may result in overflow and hence usage of
-- 'unsafeUnFelt' is discouraged. Use 'unFelt' whenever possible instead.
unsafeUnFelt :: Felt -> Natural
unsafeUnFelt = coerce unMod
{-# INLINE unsafeUnFelt #-}
