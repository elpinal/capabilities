{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Capabilities
  (
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Lazy as Map

import GHC.Generics hiding (Constructor)

import Index

data Kind
  = Type
  | Rgn
  | Cap
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

data Region
  = RVar Variable
  | RegionName Name
  deriving (Eq, Show, Generic)

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

fromType :: Constructor -> Type
fromType (CType ty) = ty
fromType c          = error $ "fromType: not type: " ++ show c

fromRegion :: Constructor -> Region
fromRegion (CRegion r) = r
fromRegion c           = error $ "fromRegion: not region: " ++ show c

fromCapability :: Constructor -> Capability
fromCapability (CCapability cap) = cap
fromCapability c                 = error $ "fromCapability: not capability: " ++ show c

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
  deriving (Eq, Show, Generic)

-- Multiplicities.
data Multi
  = Unique
  | NonUnique
  deriving (Eq, Show, Generic)

data ConstrBinding
  = Bind Kind
  | Subcap Capability
  deriving (Eq, Show, Generic)

newtype ConstrContext = ConstrContext { getConstrContext :: [ConstrBinding] }
  deriving (Eq, Show, Generic)

cctxLen :: ConstrContext -> Int
cctxLen (ConstrContext xs) = length xs

newtype Context = Context [Type]
  deriving (Eq, Show)

newtype RegionContext = RegionContext (Map.Map Location Type)
  deriving (Eq, Show)

newtype MemoryContext = MemoryContext (Map.Map Name RegionContext)
  deriving (Eq, Show)

instance Shift Variable where
  shiftAbove c d v @ (Variable n)
    | c <= n    = Variable $ n + d
    | otherwise = v

instance Shift Name where
  -- Names are not variables, so nothing happens.
  shiftAbove _ _ = id

instance Shift Capability
instance Shift ConstrBinding
instance Shift ConstrContext
instance Shift Kind
instance Shift Multi
instance Shift Region

instance Shift a => Shift (NonEmpty.NonEmpty a) where
  shiftAbove c d x = shiftAbove c d <$> x

instance Shift a => Shift [a] where
  shiftAbove c d x = shiftAbove c d <$> x

instance Shift Type where
  shiftAbove c d (Fun cctx cap ts r) = Fun (f cctx) (g cap) (g ts) (f r)
    where
      c' = c + cctxLen cctx

      f :: Shift a => a -> a
      f = shiftAbove c d

      g :: Shift a => a -> a
      g = shiftAbove c' d
  shiftAbove c d ty = to $ gShiftAbove c d $ from ty

class Subst a where
  -- Assume `j` is 0 when called from other functions.
  subst :: Int -> Constructor -> a -> a

instance Subst Type where
  subst _ _ IntType  = IntType
  subst j c ty @ (TVar v)
    | Variable j == v = shift j $ fromType c
    | otherwise       = ty
  subst j c (HandleType r) = HandleType $ subst j c r
  subst j c (Fun cctx cap ts r) = Fun (subst j' c cctx) (subst j' c cap) (subst j' c <$> ts) (subst j c r)
    where
      j' = j + cctxLen cctx
  subst j c (TupleType ts r) = TupleType (subst j c <$> ts) $ subst j c r

instance Subst Region where
  subst _ _ r @ (RegionName _) = r
  subst j c r @ (RVar v)
    | Variable j == v = shift j $ fromRegion c
    | otherwise       = r

instance Subst ConstrContext where
  subst j c (ConstrContext bs) = ConstrContext $ subst j c <$> bs

instance Subst ConstrBinding where
  subst _ _ b @ (Bind _) = b
  subst j c (Subcap cap) = Subcap $ subst j c cap

instance Subst Capability where
  subst _ _ Empty = Empty
  subst j c cap @ (CapVar v)
    | Variable j == v = shift j $ fromCapability c
    | otherwise       = cap
  subst j c (Singleton r m) = Singleton (subst j c r) m
  subst j c (Join c1 c2)    = Join (subst j c c1) (subst j c c2)
  subst j c (Strip cap)     = Strip $ subst j c cap

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
