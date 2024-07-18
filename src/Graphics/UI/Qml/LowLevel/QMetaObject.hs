{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, DataKinds, PolyKinds, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Graphics.UI.Qml.LowLevel.QMetaObject where

import Data.Kind
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Graphics.UI.Qml.LowLevel.QVariant
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM

import qualified Graphics.UI.Qml.Internal.QMetaObject as Raw
import qualified Graphics.UI.Qml.Internal.Types as Raw
import qualified Data.Map.Strict as Map
import qualified Foreign.Concurrent as F

type family TySig e (a :: [Type]) :: k
type instance TySig e '[] = IO e
type instance TySig e (Int:xs) = Int -> TySig e xs
type instance TySig e (String:xs) = String -> TySig e xs
type instance TySig e (Bool:xs) = Bool -> TySig e xs

data CType (a :: [Type]) = CType

class IsQMetaType (ty :: [Type]) where
  qMetaType :: CType ty -> [CInt]
  invoke    :: Slot ty e -> [QVariant] -> IO e

instance IsQMetaType '[] where
  qMetaType _ = []
  invoke (Slot _ _ e) _ = e

instance (IsQMetaType xs) => IsQMetaType (Int:xs) where
  qMetaType _ = 2 : qMetaType (CType @xs)
  invoke (Slot n _ e) (y:ys) = do
    i <- fromQVariant y
    invoke (Slot n (CType @xs) (e i)) ys
  invoke (Slot n _ _) _ = error $ "Slot " <> n <> " Expected int"

instance (IsQMetaType xs) => IsQMetaType (Bool:xs) where
  qMetaType _ = 1 : qMetaType (CType @xs)
  invoke (Slot n _ e) (y:ys) = do
    i <- fromQVariant y
    invoke (Slot n (CType @xs) (e i)) ys
  invoke (Slot n _ _) _ = error $ "Slot " <> n <> " Expected bool"

instance (IsQMetaType xs) => IsQMetaType (String:xs) where
  qMetaType _ = 10 : qMetaType (CType @xs)
  invoke (Slot n _ e) (y:ys) = do
    i <- fromQVariant y
    invoke (Slot n (CType @xs) (e i)) ys
  invoke (Slot n _ _) _ = error $ "Slot " <> n <> " Expected string"

data Slot ty e where
  Slot :: (IsQMetaType ty) => String -> CType ty -> TySig e ty -> Slot ty e

data VSlot e = forall ty . IsQMetaType ty => VSlot (Slot ty e)

data Prop = forall a . (IsQVariant a) => Prop String a

type CallBackMap = Map.Map String (TChan [QVariant])

data QMetaObject = QMetaObject
  { qMetaObjectPtr :: ForeignPtr Raw.DosQMetaObject
  , callbackMap    :: CallBackMap
  }

newParameterDef :: CInt -> IO Raw.ParameterDefinition
newParameterDef ty = do
  name <- newCString ""
  return $ Raw.ParameterDefinition name ty

newSlotDef :: TChan e -> VSlot e -> IO (Raw.SlotDefinition, CallBackMap)
newSlotDef echan (VSlot slot@(Slot name t _)) = do
  cname <- newCString name
  let metaTypes = qMetaType t
  params <- mapM newParameterDef metaTypes
  pParams <- mallocArray (length metaTypes)
  pokeArray pParams params
  argChan <- atomically newTChan
  let m = Map.singleton name argChan
  _ <- forkIO $ fix $ \loop -> do
    args <- atomically $ readTChan argChan
    e <- invoke slot args
    atomically $ writeTChan echan e
    loop
  return (Raw.SlotDefinition cname 43 (fromIntegral $ length metaTypes) pParams, m)

newQMetaObject :: TChan e -> String -> [VSlot e] -> IO QMetaObject
newQMetaObject echan name slots = do
  let count     = length slots
  slotDefsMap <- mapM (newSlotDef echan) slots
  let slotDefs = map fst slotDefsMap
      maps     = Map.unions $ map snd slotDefsMap
  slotBuffer <- mallocArray count
  pokeArray slotBuffer slotDefs
  pRawSlotDefs <- malloc
  poke pRawSlotDefs $ Raw.SlotDefinitions (fromIntegral count) slotBuffer
  ptr <- withCString name $ \cname -> Raw.create nullPtr cname nullPtr pRawSlotDefs nullPtr
  fptr <- newForeignPtr Raw.finalizer ptr
  F.addForeignPtrFinalizer fptr $ do
    mapM_ (free . Raw.sltParameters) slotDefs
    free pRawSlotDefs
    free slotBuffer
  return $ QMetaObject fptr maps
