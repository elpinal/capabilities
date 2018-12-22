{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Capabilities
  (
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Lazy as Map

data Kind
  = Type
  | Rgn
  | Cap
  deriving (Eq, Show)

newtype Variable = Variable Int
  deriving (Eq, Show)

newtype Name = Name Int
  deriving (Eq, Show)

newtype Location = Location Int
  deriving (Eq, Show)

data Type
  = TVar Variable
  | IntType
  | HandleType Region
  | Fun ConstrContext Capability (NonEmpty.NonEmpty Type) Region
  | TupleType (NonEmpty.NonEmpty Type) Region
  deriving (Eq, Show)

data Region
  = RVar Variable
  | RegionName Name
  deriving (Eq, Show)

data Term
  = Let Decl Term
  | If0 Value Term Term
  | App Value (NonEmpty.NonEmpty Value)
  | Halt Value
  deriving (Eq, Show)

data Decl
  = Assign Value
  | Arith BinOp Value Value
  | Alloc HValue Value
  | Proj Int Value
  | NewRgn Variable
  | FreeRgn Value
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  deriving (Eq, Show)

data Value
  = Var Int
  | Int Int
  | Address Name Location
  | Handle Name
  | Inst Value Constructor
  deriving (Eq, Show)

data Constructor
  = CVar Variable
  | CType Type
  | CRegion Region
  | CCapability Capability
  deriving (Eq, Show)

data HValue
  = Fix ConstrContext Capability (NonEmpty.NonEmpty Type) Term
  | Tuple (NonEmpty.NonEmpty Value)
  deriving (Eq, Show)

data Capability
  = CapVar Variable
  | Empty
  | Singleton Region Multi
  | Join Capability Capability
  | Strip Capability
  deriving (Eq, Show)

-- Multiplicities.
data Multi
  = Unique
  | NonUnique
  deriving (Eq, Show)

data ConstrBinding
  = Bind Kind
  | Subcap Capability
  deriving (Eq, Show)

newtype ConstrContext = ConstrContext [ConstrBinding]
  deriving (Eq, Show)

newtype Context = Context [Type]
  deriving (Eq, Show)

newtype RegionContext = RegionContext (Map.Map Location Type)
  deriving (Eq, Show)

newtype MemoryContext = MemoryContext (Map.Map Name RegionContext)
  deriving (Eq, Show)

data TypeError
  = TypeMismatch Type Type
  | UnboundVariable Int
  deriving (Eq, Show)

nth :: Int -> [a] -> Maybe a
nth n xs
  | 0 <= n && n < length xs = return $ xs !! n
  | otherwise               = Nothing

lookupVar :: Members '[Reader Context, Error TypeError] r => Int -> Eff r Type
lookupVar n = do
  Context ts <- ask
  maybe (throwError $ UnboundVariable n) return $ nth n ts

class Typed a where
  type Output a
  type Effects a :: [* -> *]

  typeOf :: Members (Effects a) r => a -> Eff r (Output a)

instance Typed Value where
  type Output Value = Type
  type Effects Value = '[Reader Context, Error TypeError]

  typeOf (Var n) = lookupVar n
  typeOf (Int _) = return IntType
  typeOf (Handle name) = return $ HandleType $ RegionName name
