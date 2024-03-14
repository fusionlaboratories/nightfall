{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nightfall.Lang.Syntax.DotRecord where

import Nightfall.Alphabet
import Nightfall.Lang.Types
import Nightfall.Lang.Internal.Types

import Data.Kind
import GHC.Records
import Unsafe.Coerce (UnsafeEquality (..), unsafeEqualityProof)

{- Note [Surface syntax through OverloadedRecordDot]
We (ab)use the @OverloadedRecordDot@ extension to provide a nice type-safe syntax for our eDSL.

If we were (and we originally were) to create a program manually, we'd get something along the lines
of

    sumTo10Stmts :: Body asm ()
    sumTo10Stmts = do
        declareVarF "n" 10
        declareVarF "acc" 0
        while (varF "n" `gt` 0) $ do
            assignVarF "acc" (varF "acc" + varF "n")
            assignVarF "n" (varF "n" - 1)
        ret $ varF "acc"

This is a pretty regular way of manually creating an AST, but regular ways of creating ASTs aren't
exactly known for their ergonomics or type safety. In our case

- Two quotes for each variable and manual type disambiguation (the "F" suffixes) create a lot of
  visual noise.
- The Haskell type checker
  * won't save you from using an undeclared variable.
  * won't save you from declaring the same variable twice (potentially with different types).
  * won't save you from declaring one type for a variable and subsequently assigning a value of a
    different type to the variable.
  * generally won't be able to infer the type of a value that you're assigning to or already storing
    in a variable. Which in our case requires you to manually pick different variable assignment /
    referencing functions depending on the type of the variable -- bloating the API and creating
    more room for mistake.

This module implements syntax sugar making it more pleasant and type-safe to create ASTs by hand.
Here's how the program above looks with the sugar (obviously, it elaborates to the same thing):

    sumTo10Stmts :: Body asm ()
    sumTo10Stmts = do
        Felt <- declare.n 10
        Felt <- declare.acc 0
        while (get.n `gt` 0) $ do
            set.acc $ get.acc + get.n
            set.n $ get.n - 1
        ret get.acc

Let's dive into the machinery feature by feature.

The most basic features are declaring variables, as well as setting and getting their values. E.g.

    declareSetGetFelt :: Body asm ()
    declareSetGetFelt = do
        Felt <- declare.x 42
        set.x 3
        ret get.x

Here's how this program elaborates:

    >>> import Data.Foldable
    >>> import Nightfall.Lang.Types (runBody)
    >>> traverse_ print $ runBody declareSetGetFelt
    DeclVariable "x" (Lit (42 `modulo` 18446744069414584321))
    AssignVar "x" (Lit (3 `modulo` 18446744069414584321))
    Return (Var "x")

As you can see the @x@ name that we introduced via @declare@ became the @"x"@ name in the code.

Variable names are not hardcoded in any way, you can declare the same @x@ variable as being a
boolean one:

    declareSetGetBool :: Body asm ()
    declareSetGetBool = do
        Bool <- declare.x $ lit True
        set.x $ lit False
        ret get.x

or instead of a single-letter name use something more appropriate for serious production:

    declareSetGetProductionBool :: Body asm ()
    declareSetGetProductionBool = do
        Bool <- declare.suchEnterpriseNameWowMuchDescriptive $ lit True
        set.suchEnterpriseNameWowMuchDescriptive $ lit False
        ret get.suchEnterpriseNameWowMuchDescriptive

If you attempt to declare the same variable twice, you'll get a Haskell type error:

    declareTwiceFelt :: Body asm ()
    declareTwiceFelt = do
        Felt <- declare.x 42
        -- error: [GHC-64725]
        --     • 'x' is declared twice
        Felt <- declare.x 3
        pure ()

If you attempt to initialize a 'Felt' variable with a boolean value, you'll get another Haskell type
error:

    declareFeltBool :: Body asm ()
    declareFeltBool = do
        -- error: [GHC-18872]
        --     • Couldn't match type ‘DeclBool’ with ‘DeclFelt’
        Felt <- declare.x $ lit True
        pure ()

If you attempt to use an undeclared variable, you'll get a Haskell type error too, although not a
very readable one currently:

    getUndeclared :: Body asm ()
    getUndeclared =
        -- error: [GHC-39999]
        --     • No instance for ‘KnownType (TypeOf "x")’
        ret get.x

Note that in the example above

    declareSetGetFelt :: Body asm ()
    declareSetGetFelt = do
        Felt <- declare.x 42
        set.x 3
        ret get.x

the type of @42@ and @3@ (whose unmonomorphised type is @Num a => a@) in inferred to be 'Felt',
because of matching on the 'Felt' constructor, the user doesn't need to disambiguate types manually,
a single match is enough.

TODO: describe 'mut'.
-}

{- Note [Alternative surface syntax implementations]
(P)HOAS can be convenient, but it has a number of drawbacks:

- It falls short when it comes to static handling of names, e.g. we wouldn't be able to implement
  throwing a type error when the same variable is declared twice, because variable names are runtime
  info with (P)HOAS unlike with our approach where names exist at the Haskell's type level.
- When using (P)HOAS one has to actually bind a Haskell variable whenever they want to use a
  variable of the target language, which can be a hassle in cases when it's enough to merely know
  that the target language variable exists. For example, if you have a global target language
  variable that all procedures can read from / write to, then with HOAS you'd have to carry it
  around in all functions as an argument, while with our approach we can simply reassure the type
  checker that the variable exists using a 'declare'-like construct or introduce 'gget' and 'gset'
  for global variable reading and global variable writing etc. Point is, our approach is quite more
  flexible than (P)HOAS.
- Having to use Haskell names with (P)HOAS necessarily entails having to avoid clashing with names
  of common Haskell functions. It would be annoying to force the user to avoid shadowing names
  like @sum@, @null@, @and@, @elem@, @even@, @id@, @map@, @last@, @length@ or hide them from the
  implicitly imported Prelude. We even use the @length@ in our own tests!

Another thing we could do differently is use the @OverloadedLabels@ extension instead of
@OverloadedRecordDot@, but the simplest approach would require us to write @f (get #x)@ instead of
@f get.x@, which doesn't appear to be an improvement at all, so if we were to use overloaded labels,
we'd probably have to commit to @#x@ meaning different things in different contexts (for example
@set #x@ would elaborate one way and @#x + #y@ would elaborate a completely different way), which
sounds like it's not gonna play very nice with type inference unless we use @INCOHERENT@ pragmas,
which are brittle. Plus those hashes seem like a lot of visual noise, perhaps simply due to the
sheer amount of ink in them.
-}

type Prefix :: Symbol -> Type -> Type -> Type
data Prefix prefix asm a = Prefix

type TypeOf :: Symbol -> Type
type family TypeOf var

data DeclTypeOf name a where
    DeclFelt :: TypeOf name ~ Felt => DeclTypeOf name Felt
    DeclBool :: TypeOf name ~ Bool => DeclTypeOf name Bool

type KnownDecl :: (Symbol -> Type) -> Type -> Constraint
class KnownDecl decl a | decl -> a, a -> decl where
    knownDecl :: forall name. TypeOf name ~ a => decl name
    varType :: Proxy a -> VarType

data DeclFelt name where
    Felt :: TypeOf name ~ Felt => DeclFelt name
instance KnownDecl DeclFelt Felt where
    knownDecl = Felt
    varType _ = VarFelt

data DeclBool name where
    Bool :: TypeOf name ~ Bool => DeclBool name
instance KnownDecl DeclBool Bool where
    knownDecl = Bool
    varType _ = VarBool

class KnownType a where
    knownType :: forall name. TypeOf name ~ a => DeclTypeOf name a
instance KnownType Felt where
    knownType = DeclFelt
instance KnownType Bool where
    knownType = DeclBool

type Equals :: forall a. a -> a -> Bool
type family Equals x y where
    Equals x x = 'True
    Equals _ _ = 'False

type UnequatableGo :: Bool -> ErrorMessage -> Constraint
class UnequatableGo eq msg
instance {-# INCOHERENT #-} UnequatableGo eq msg
instance TypeError msg => UnequatableGo 'True  msg
instance TypeError msg => UnequatableGo 'False msg

type Unequatable :: ErrorMessage -> Type -> Type -> Constraint
type Unequatable msg a b = UnequatableGo (Equals a b) msg

-- The 'Unequatable' constraint prevents @declare.name@ from being used for the same @name@
-- twice. The way it works is that upon a subsequent call to @declare.name@ the user either supplies
-- the same type for @name@ triggering the @UnequatableGo 'True msg@ instance or a different one
-- triggering the @UnequatableGo 'False msg@ instance and both of those throw a
-- 'TypeError'. I.e. this instance only type checks successfully when GHC can't tell whether @TypeOf
-- name@ is equal to @a@ or not, which is only the case when @declare.name@ is invoked exactly once.
--
-- TODO: would be nice to make pattern matching on the type tag mandatory, because without the
-- matching a lot guarantees go out of the window... or do they, given that the forged equality
-- constraint is only accessible through pattern matching?
instance
        ( res ~ (Expr asm a -> Body asm (decl name)), KnownSymbol name, KnownDecl decl a
        , Unequatable ('Text "'" :<>: 'Text name :<>: 'Text "' is declared twice") (TypeOf name) a
        ) => HasField name (Prefix "declare" asm a) res where
    getField _ (Expr expr_) = do
        let name = symbolVal' (proxy# @name)
        statement $ DeclVariable (varType $ Proxy @a) name expr_
        -- It would be very dangerous to allow for forging multiple @TypeOf name ~ a1@,
        -- @TypeOf name ~ a2@ etc constraints, since GHC could conclude @a1 ~ a2@ from those,
        -- which for all intents and purposes would be implicit uncontrolled 'unsafeCoerce', but
        -- declaring the same name twice is supposed to result in a 'TypeError', so this danger
        -- should be ruled out.
        case unsafeEqualityProof @(TypeOf name) @a of
            UnsafeRefl -> pure knownDecl

instance (res ~ Expr asm a, TypeOf name ~ a, KnownSymbol name, KnownType a) =>
        HasField name (Prefix "get" asm a) res where
    getField _ = Expr . Var $ symbolVal' (proxy# @name)

-- 'KnownType' is only needed to prevent @set.x@ from being successfully type checked when there's
-- no @x@ in the current scope. Without the constraint, GHC would happily infer
--
-- > set.x :: Expr (TypeOf "x") -> Body asm ()
instance (res ~ (Expr asm a -> Body asm ()), TypeOf name ~ a, KnownSymbol name, KnownType a) =>
        HasField name (Prefix "set" asm a) res where
    getField _ (Expr expr_) = do
        let name = symbolVal' (proxy# @name)
        statement $ AssignVar name expr_

instance
        ( res ~ ((Expr asm a -> Expr asm a) -> Body asm ())
        , TypeOf name ~ a, KnownSymbol name, KnownType a
        ) => HasField name (Prefix "mut" asm a) res where
    getField _ f = getField @name set . f $ getField @name get

newtype Mut a = Mut a
instance res ~ Mut (Expr asm a -> r) => HasField "mut" (Expr asm a -> r) res where
    getField = Mut

-- TODO: generalize to arbitrary arity.
instance
        ( a ~ a', res ~ (Expr asm b -> Body asm ())
        , TypeOf name ~ a, KnownSymbol name, KnownType a
        ) => HasField name (Mut (Expr asm a -> Expr asm b -> Expr asm a')) res where
    getField (Mut f) y = getField @name mut $ \x -> f x y

declare :: Prefix "declare" asm a
declare = Prefix

get :: Prefix "get" asm a
get = Prefix

set :: Prefix "set" asm a
set = Prefix

mut :: Prefix "mut" asm a
mut = Prefix
