{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Nightfall.Lang.Types ( Felt
                            , MidenWordOf(..)
                            , MidenWord
                            , VarName
                            , FunName
                            , Statement_
                            , Expr(..)
                            , ZKProgramAsm(..)
                            , ZKProgram
                            , BodyF(..)
                            , Body(..)
                            , SecretInputs(..)
                            , emptySecretInputs
                            , runBody
                            , mkSimpleProgramAsm
                            , mkSimpleProgram
                            , mkZKProgram
                            , IsLiteral(..)
                            , lit
                            , assembly
                            , unOp
                            , binOp
                            , add
                            , sub
                            , mul
                            , div'
                            , idiv32
                            , add256
                            , addNat
                            , eq
                            , not'
                            , lt
                            , lte
                            , gt
                            , gte
                            , isOdd
                            , getAt
                            , setAt
                            , statement
                            , statements
                            -- , fcall
                            , nextSecret
                            , ifElse
                            , simpleIf
                            , while
                            , initArray
                            -- , nakedCall
                            , ret
                            , comment
                            , emptyLine
                            ) where

import Nightfall.Alphabet
import Nightfall.Lang.Internal.Types

import Control.Monad.Free.Church
import qualified Data.Map.Strict as Map

-- | Expression wrapper type, typed for safety, exposed to use
newtype Expr asm a = Expr
    { unExpr :: Expr_ asm
    } deriving (Eq, Show)

class IsLiteral a where
    literal :: a -> Literal

instance IsLiteral Felt where
    literal = LiteralFelt

instance IsLiteral Bool where
    literal = LiteralBool

instance IsLiteral Natural where
    literal = LiteralNat

lit :: IsLiteral a => a -> Expr asm a
lit = Expr . Literal . literal

-- | Num instance to make writings easier, to allow wriring expressions with "+", "-", etc.
instance a ~ Felt => Num (Expr asm a) where
    (+) = add
    (-) = sub
    (*) = mul
    fromInteger = lit . fromInteger
    abs = error "'abs' not implemented for 'Expr'"
    signum = error "'signum' not implemented for 'Expr'"

data BodyF asm a = BodyF (Statement_ asm) a
    deriving (Eq, Show, Functor)

-- | A 'Body' represents a list of 'Statement_'s. We do not simply use @[Statement_ asm]@, because
--
-- 1. a 'Body' can be constructed using the do notation, which is much nicer than the list syntax
-- 2. concatenation of 'Body's is more efficient than concatentation of lists, because 'Body' is
--    implemented as a Church-encoded free monad (which is to the regular free monad what @DList@ is
--    to @[]@).
newtype Body asm a = Body
    { unBody :: F (BodyF asm) a
    } deriving newtype (Functor, Applicative, Monad)

data SecretInputs = SecretInputs
    { _adviceStack :: [Felt],
      _adviceMap :: Map Text [MidenWord]
    } deriving (Eq, Ord, Show)

emptySecretInputs :: SecretInputs
emptySecretInputs = SecretInputs [] Map.empty

data ZKProgramAsm asm = ZKProgram
    { pName :: String
      -- ^ Program name, useful for tests.
    , pBody :: Body asm ()
      -- ^ List of statements comprising the program
    , pPublicInputs :: [Felt]
      -- ^ Defining the public inputs as a list of field elements for now
    , pSecretInputs :: Either SecretInputs FilePath
      -- ^ Either a list with private inputs or an inputs file path.
      -- TODO: make it either just 'SecretInputs' or at least @IO SecretInputs@ or something.
      -- 'FilePath' doesn't belong there.
      -- TODO: is there a reason to merge public and secret inputs into a single data type?
    }

type ZKProgram = ZKProgramAsm Void

-- | Helper to quickly make a simple @ZKProgram@ from a list of statements, no inputs
mkSimpleProgramAsm :: String -> Body asm () -> ZKProgramAsm asm
mkSimpleProgramAsm name body = ZKProgram
    { pName = name
    , pBody = body
    , pPublicInputs = []
    , pSecretInputs = Left emptySecretInputs
    }

mkSimpleProgram :: String -> Body Void () -> ZKProgram
mkSimpleProgram = mkSimpleProgramAsm

-- | Helper to build a @ZKProgram@.
mkZKProgram :: String -> Body Void () -> [Felt] -> FilePath -> ZKProgram
mkZKProgram name body pubs secretFP = ZKProgram
    { pName = name
    , pBody = body
    , pPublicInputs = pubs
    , pSecretInputs = Right secretFP
    }

-- * "Smart Constructors" for building (type-safe) @Expr. They are the ones exposed for users to use
-- instead of constructing the types directly.

assembly :: asm -> Expr asm a
assembly = Expr . Assembly

unOp :: forall asm a b. UnOp -> Expr asm a -> Expr asm b
unOp = coerce (UnOp :: UnOp -> Expr_ asm -> Expr_ asm)

binOp :: forall asm a b c. BinOp -> Expr asm a -> Expr asm b -> Expr asm c
binOp = coerce (BinOp :: BinOp -> Expr_ asm -> Expr_ asm -> Expr_ asm)

-- ** Arithmetic operations

add :: Expr asm Felt -> Expr asm Felt -> Expr asm Felt
add = binOp Add

sub :: Expr asm Felt -> Expr asm Felt -> Expr asm Felt
sub = binOp Sub

mul :: Expr asm Felt -> Expr asm Felt -> Expr asm Felt
mul = binOp Mul

div' :: Expr asm Felt -> Expr asm Felt -> Expr asm Felt
div' = binOp Div

idiv32 :: Expr asm Word32 -> Expr asm Word32 -> Expr asm Word32
idiv32 = binOp IDiv32

add256 :: Expr asm Word256 -> Expr asm Word256 -> Expr asm Word256
add256 = binOp Add256

addNat :: Expr asm Natural -> Expr asm Natural -> Expr asm Natural
addNat = binOp AddNat

-- ** Boolean operations

eq :: Expr asm Felt -> Expr asm Felt -> Expr asm Bool
eq = binOp Equal

not' :: Expr asm Bool -> Expr asm Bool
not' = unOp Not

lt :: Expr asm Felt -> Expr asm Felt -> Expr asm Bool
lt = binOp Lower

lte :: Expr asm Felt -> Expr asm Felt -> Expr asm Bool
lte = binOp LowerEq

gt :: Expr asm Felt -> Expr asm Felt -> Expr asm Bool
gt = binOp Greater

gte :: Expr asm Felt -> Expr asm Felt -> Expr asm Bool
gte = binOp GreaterEq

isOdd :: Expr asm Felt -> Expr asm Bool
isOdd = unOp IsOdd

getAt :: VarName -> Expr asm Felt -> Expr asm Felt
getAt var (Expr i) = Expr $ GetAt var i

-- Function calls will come later
-- fcall :: FunName -> [Expr asm] -> Expr asm
-- fcall = FCall

-- * Inputs

-- ** Secret input

nextSecret :: Expr asm a
nextSecret = Expr NextSecret

-- * "Smart Constructors" for building (type-safe) @Statement. They are the ones exposed for users to use

-- | Turn a 'Statement_' into a 'Body' representing the statement.
statement :: Statement_ asm -> Body asm ()
statement stmt = Body . liftF $ BodyF stmt ()

-- | Turn a list of 'Statement_'s into a 'Body' representing the statements.
statements :: [Statement_ asm] -> Body asm ()
statements = traverse_ statement

-- | Turn a 'Body' into the list of 'Statement_'s that it represents.
runBody :: Body asm () -> [Statement_ asm]
runBody body = runF (unBody body) (const []) $ \(BodyF stmt stmts) -> stmt : stmts

setAt :: VarName -> Expr asm Felt -> Expr asm Felt -> Body asm ()
setAt var (Expr i) (Expr val) = statement $ SetAt var i val

ifElse :: Expr asm Bool -> Body asm () -> Body asm () -> Body asm ()
ifElse (Expr cond) ifBlock elseBlock = statement $ IfElse cond (runBody ifBlock) (runBody elseBlock)

-- | A constructor for when you don't want 'else' statement
simpleIf :: Expr asm Bool -> Body asm () -> Body asm ()
simpleIf cond ifBlock = ifElse cond ifBlock $ pure ()

while :: Expr asm Bool -> Body asm () -> Body asm ()
while (Expr cond) body = statement $ While cond (runBody body)

initArray :: VarName -> [Felt] -> Body asm ()
initArray var = statement . InitArray var

-- nakedCall :: FunName -> [Expr asm] -> Body asm ()
-- nakedCall = NakedCall

ret :: Expr asm a -> Body asm ()
ret = statement . Return . unExpr

comment :: String -> Body asm ()
comment = statement . Comment

emptyLine :: Body asm ()
emptyLine = statement EmptyLine
