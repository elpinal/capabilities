{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Capabilities
  ( Capability(..)
  , Region(..)
  , Multi(..)
  , Variable(..)
  , capEqual
  , Subcap(..)
  , ConstrContext(..)
  , ConstrBinding(..)
  , TypeError(..)
  , Kind(..)
  , wfCctx
  , fromBQ
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State

import Data.Coerce
import Data.Foldable
import Data.Functor
import Data.Heap (Heap, viewMin)
import qualified Data.Heap as Heap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

import GHC.Generics hiding (Constructor)

import Index

data Kind
  = Type
  | Rgn
  | Cap
  deriving (Eq, Show, Generic)

newtype Variable = Variable Int
  deriving (Eq, Ord, Show)

newtype Name = Name Int
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

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
instance Shift Constructor
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
  | KindMismatch Kind Kind
  | UnboundVariable Int
  | UnboundConstrVariable Variable
  | NotFunction Type
  | NotPolymorphic Type
  | NotCapability Constructor
  | NotBoundedQuantification ConstrBinding
  | NotSubcapability Capability Capability
  deriving (Eq, Show)

nth :: Int -> [a] -> Maybe a
nth n xs
  | 0 <= n && n < length xs = return $ xs !! n
  | otherwise               = Nothing

lookupVar :: Members '[Reader Context, Error TypeError] r => Int -> Eff r Type
lookupVar n = do
  Context ts <- ask
  maybe (throwError $ UnboundVariable n) return $ nth n ts

lookupCVar :: Members CEnv r => Variable -> Eff r ConstrBinding
lookupCVar v @ (Variable n) = do
  ConstrContext bs <- ask
  maybe (throwError $ UnboundConstrVariable v) return $ nth n bs

-- From bounded quantification.
fromBQ :: Members CEnv r => Variable -> Eff r Capability
fromBQ v = do
  b <- lookupCVar v
  case b of
    Subcap c -> return c
    Bind _ -> throwError $ NotBoundedQuantification b

-- The first argument will be the top.
appnedCctx :: ConstrContext -> ConstrContext -> ConstrContext
appnedCctx cctx1 cctx2 = f cctx1 $ shift (cctxLen cctx1) cctx2
  where
    f (ConstrContext bs1) (ConstrContext bs2) = ConstrContext $ bs1 ++ bs2

class Typed a where
  type Output a
  type Effects a :: [* -> *]

  typeOf :: Members (Effects a) r => a -> Eff r (Output a)

withFun :: Member (Error TypeError) r => (ConstrContext -> Capability -> NonEmpty.NonEmpty Type -> Region -> Eff r a) -> Type -> Eff r a
withFun f (Fun cctx cap ts r) = f cctx cap ts r
withFun _ ty                  = throwError $ NotFunction ty

getHead :: ConstrContext -> Maybe (ConstrBinding, ConstrContext)
getHead (ConstrContext [])       = Nothing
getHead (ConstrContext (x : xs)) = return (x, ConstrContext xs)

wfCBinding :: Members CEnv r => ConstrBinding -> Eff r ()
wfCBinding (Bind _) = return ()
wfCBinding (Subcap cap) = kindIs cap Cap

wfCctx :: Members CEnv r => ConstrContext -> Eff r ()
wfCctx (ConstrContext bs) = void $ foldlM f id bs
  where
    f m b = do
      local m $ wfCBinding b
      return $ appnedCctx (ConstrContext [b]) . m

withCctx :: Member (Reader ConstrContext) r => ConstrContext -> Eff r a -> Eff r a
withCctx cctx m = local (appnedCctx cctx) m

subcap :: Members CEnv r => Capability -> Capability -> Eff r ()
subcap c1 c2 = do
  ok <- c1 <: c2
  unless ok $
    throwError $ NotSubcapability c1 c2

type Env = '[Reader Context, Reader ConstrContext, Error TypeError]
type CEnv = '[Reader ConstrContext, Error TypeError]

class Kinded a where
  kindOf :: Members CEnv r => a -> Eff r Kind

instance Kinded Variable where
  kindOf v = do
    b <- lookupCVar v
    case b of
      Bind k -> return k
      Subcap _ -> return Cap

instance Kinded Constructor where
  kindOf (CVar v)          = kindOf v
  kindOf (CType ty)        = kindOf ty
  kindOf (CRegion r)       = kindOf r
  kindOf (CCapability cap) = kindOf cap

instance Kinded Type where
  kindOf (TVar v) = kindOf v
  kindOf IntType  = return Type
  kindOf (HandleType r) = kindIs (CRegion r) Rgn $> Type
  kindOf (Fun cctx cap ts r) = do
    kindIs r Rgn
    wfCctx cctx
    withCctx cctx $ do
      mapM_ (`kindIs` Type) ts
      kindIs cap Cap
    return Type
  kindOf (TupleType ts r) = mapM_ (`kindIs` Type) ts >> kindIs r Rgn $> Type

instance Kinded Region where
  kindOf (RegionName _) = return Rgn
  kindOf (RVar v)       = kindOf v

instance Kinded Capability where
  kindOf (CapVar v)      = kindOf v
  kindOf Empty           = return Cap
  kindOf (Singleton r _) = kindIs r Rgn $> Cap
  kindOf (Join c1 c2)    = mapM_ (`kindIs` Cap) [c1, c2] $> Cap
  kindOf (Strip cap)     = kindIs cap Cap $> Cap

kindIs :: (Members CEnv r, Kinded a) => a -> Kind -> Eff r ()
kindIs x k0 = do
  k <- kindOf x
  if k == k0
    then return ()
    else throwError $ KindMismatch k k0

instance Typed Value where
  type Output Value = Type
  type Effects Value = Env

  typeOf (Var n) = lookupVar n
  typeOf (Int _) = return IntType
  typeOf (Handle name) = return $ HandleType $ RegionName name
  typeOf (Inst v constr) = do
    typeOf v >>= withFun f
    where
      f cctx cap ts r = do
        case getHead cctx of
          Nothing         -> throwError $ NotPolymorphic $ Fun cctx cap ts r
          Just (b, cctx') ->
            case b of
              Bind k -> kindIs constr k $> Fun (g cctx') (g cap) (g <$> ts) r
                where
                  g :: (Shift a, Subst a) => a -> a
                  g = substTop (cctxLen cctx) constr
              Subcap cap1 -> do
                case constr of
                  CVar _ -> undefined
                  CCapability cap0 -> do
                    subcap cap0 cap1
                    return $ Fun (g cctx') (g cap) (g <$> ts) r
                      where
                        g :: (Shift a, Subst a) => a -> a
                        g = substTop (cctxLen cctx) constr
                  _ -> throwError $ NotCapability constr

substTop :: (Shift a, Subst a) => Int -> Constructor -> a -> a
substTop n c x = shift (-1) $ subst 0 (shift n c) x

data CapElem
  = EVar Variable -- capability variable
  | ESVar Variable -- stripped capability variable
  | ERegion Region Multi -- region with multiplicity
  deriving (Eq, Show)

isDuplicatable :: CapElem -> Bool
isDuplicatable (ESVar _)             = True
isDuplicatable (ERegion _ NonUnique) = True
isDuplicatable _                     = False

-- ESVar < EVar < ERegion.
instance Ord CapElem where
  ESVar v1 <= ESVar v2 = v1 <= v2
  ESVar _  <= _        = True

  EVar v1 <= EVar v2     = v1 <= v2
  EVar _  <= ESVar _     = False
  EVar _  <= ERegion _ _ = True

  ERegion r1 m1 <= ERegion r2 m2 = (r1, m2) <= (r2, m2)
  ERegion _ _   <= ESVar _       = False
  ERegion _ _   <= EVar _        = False

newtype NormalizedCap = NormalizedCap (Heap CapElem)
  deriving (Eq, Show)

mapNormalizedCap :: (CapElem -> CapElem) -> NormalizedCap -> NormalizedCap
mapNormalizedCap = coerce . Heap.map

singleton :: CapElem -> NormalizedCap
singleton = coerce . Heap.singleton

strip :: CapElem -> CapElem
strip (EVar v)      = ESVar v
strip e @ (ESVar _) = e
strip (ERegion r _) = ERegion r NonUnique

normalize_ :: Capability -> NormalizedCap
normalize_ (CapVar v)      = singleton $ EVar v
normalize_ Empty           = NormalizedCap Heap.empty
normalize_ (Singleton r m) = singleton $ ERegion r m
normalize_ (Join c1 c2)    = coerce Heap.union (normalize_ c1) (normalize_ c2)
normalize_ (Strip cap)     = mapNormalizedCap strip $ normalize_ cap

normalize :: Capability -> NormalizedCap
normalize = coerce removeDup . normalize_

skip :: CapElem -> Heap CapElem -> Heap CapElem
skip e0 h0 @ (viewMin -> Just (e, h))
  | e0 == e   = skip e0 h
  | otherwise = h0
skip _ h = h

removeDup :: Heap CapElem -> Heap CapElem
removeDup (viewMin -> Just (e, h)) =
  Heap.insert e $ removeDup $
    if isDuplicatable e
      then skip e h
      else h
removeDup _ = Heap.empty

equal :: Heap CapElem -> Heap CapElem -> Bool
equal (viewMin -> Nothing) (viewMin -> Nothing) = True
equal (viewMin -> Just (e1, h1)) (viewMin -> Just (e2, h2))
  | e1 /= e2  = False
  | otherwise = equal h1 h2
equal _ _ = False

capEqual :: Capability -> Capability -> Bool
capEqual c1 c2 = normalize c1 `eq` normalize c2
  where eq = coerce equal

class Subcap a where
  (<:) :: Members CEnv r => a -> a -> Eff r Bool

instance Subcap Multi where
  Unique    <: _         = return True
  NonUnique <: NonUnique = return True
  NonUnique <: Unique    = return False

isESVar :: CapElem -> Bool
isESVar (ESVar _) = True
isESVar _         = False

isNonUnique :: CapElem -> Bool
isNonUnique (ERegion _ NonUnique) = True
isNonUnique _                     = False

is :: Variable -> CapElem -> Bool
is v0 (EVar v)     = v == v0
is v0 (ESVar v)    = v == v0
is _ (ERegion _ _) = False

isR :: Region -> Multi -> CapElem -> Bool
isR r0 NonUnique (ERegion r NonUnique) = r0 == r
isR r0 Unique (ERegion r _)            = r0 == r
isR _ _ _                              = False

newtype Store = Store (Set.Set (Either Region Variable))
  deriving (Eq, Show)

addToStore :: Either Region Variable -> Store -> Store
addToStore e (Store s) = Store $ Set.insert e s

-- | A type class for duplicatable elements.
class Dup a where
  inj :: a -> Either Region Variable

instance Dup Region where
  inj = Left

instance Dup Variable where
  inj = Right

mem :: (Dup a, Member (State Store) r) => a -> Eff r Bool
mem x = do
  Store s <- get
  return $ inj x `Set.member` s

expectCap :: Member (Error TypeError) r => Constructor -> Kind -> Eff r ()
expectCap _ Cap = return ()
expectCap c _   = throwError $ NotCapability c

sc :: Members (State Store ': CEnv) r => Heap CapElem -> Heap CapElem -> Eff r Bool
sc (viewMin -> Just (e, h1)) h2 =
  case e of
    EVar v ->
      let (h, rem) = Heap.partition (is v) h2 in
        case viewMin h of
          Just (e', h') -> do
            when (isESVar e') $
              modify $ addToStore $ Right v
            sc h1 $ h' <> rem
          Nothing -> do
            cb <- lookupCVar v
            case cb of
              Bind k   -> expectCap (CVar v) k >> [ b' && b | b' <- mem v, b <- sc h1 h2 ]
              Subcap c -> do
                Store h' <- get
                if Right v `Set.member` h'
                  then [ b1 || b2 | b1 <- sc h1 h2, b2 <- put (Store h') >> sc (coerce (normalize c) <> h1) h2 ]
                  else sc (coerce (normalize c) <> h1) h2
    ESVar v ->
      let (h, rem) = Heap.partition (== ESVar v) h2 in
        case viewMin h of
          Just (_, h') -> modify (addToStore $ Right v) >> sc h1 (h' <> rem)
          Nothing      -> do
            cb <- lookupCVar v
            case cb of
              Bind k   -> expectCap (CVar v) k >> [ b' && b | b' <- mem v, b <- sc h1 h2 ]
              Subcap c -> do
                Store h' <- get
                if Right v `Set.member` h'
                  then [ b1 || b2 | b1 <- sc h1 h2, b2 <- put (Store h') >> sc (coerce (normalize $ Strip c) <> h1) h2 ]
                  else sc (coerce (normalize $ Strip c) <> h1) h2
    ERegion r m ->
      let (h, rem) = Heap.partition (isR r m) h2 in
        case viewMin h of
          Just (e', h') -> do
            when (isNonUnique e') $
              modify $ addToStore $ Left r
            sc h1 $ h' <> rem
          Nothing -> [ b' && b | b' <- mem r, b <- sc h1 h2 ]
sc _ h = return $ Heap.null h

instance Subcap (Heap CapElem) where
  h1 <: h2 = evalState (Store mempty) $ sc h1 h2

instance Subcap NormalizedCap where
  NormalizedCap h1 <: NormalizedCap h2 = h1 <: h2

instance Subcap Capability where
  c1 <: c2 = normalize c1 <: normalize c2
