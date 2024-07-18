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

import qualified Graphics.UI.Qml.Internal.QObject as Q
import qualified Graphics.UI.Qml.Internal.QVariant as QV
import qualified Graphics.UI.Qml.Internal.String as QS
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
  invoke    :: Slot ty e -> [Ptr Raw.DosQVariant] -> IO e

instance IsQMetaType '[] where
  qMetaType _ = []
  invoke (Slot _ _ e) _ = e

instance (IsQMetaType xs) => IsQMetaType (Int:xs) where
  qMetaType _ = 2 : qMetaType (CType @xs)
  invoke (Slot n _ e) (y:ys) = do
    i <- fromIntegral <$> QV.toInt y
    invoke (Slot n (CType @xs) (e i)) ys
  invoke (Slot n _ _) _ = error $ "Slot " <> n <> " Expected int"

instance (IsQMetaType xs) => IsQMetaType (Bool:xs) where
  qMetaType _ = 1 : qMetaType (CType @xs)
  invoke (Slot n _ e) (y:ys) = do
    i <- cbool2bool <$> QV.toBool y
    invoke (Slot n (CType @xs) (e i)) ys
  invoke (Slot n _ _) _ = error $ "Slot " <> n <> " Expected bool"

instance (IsQMetaType xs) => IsQMetaType (String:xs) where
  qMetaType _ = 10 : qMetaType (CType @xs)
  invoke (Slot n _ e) (y:ys) = do
    ptr <- QV.toString y
    i <- peekCString ptr
    QS.delete ptr
    invoke (Slot n (CType @xs) (e i)) ys
  invoke (Slot n _ _) _ = error $ "Slot " <> n <> " Expected string"

data Slot ty e where
  Slot :: (IsQMetaType ty) => String -> CType ty -> TySig e ty -> Slot ty e

data VSlot e = forall ty . IsQMetaType ty => VSlot (Slot ty e)

data Prop = forall a . (IsQVariant a) => Prop String a

type CallBackMap = Map.Map String (TChan [Ptr Raw.DosQVariant], TChan ())

type PropsMap = Map.Map String (TChan QVariant)

type ValsMap = Map.Map String (TVar QVariant)

data QMetaObject = QMetaObject
  { qMetaObjectPtr :: ForeignPtr Raw.DosQMetaObject
  , callbackMap    :: CallBackMap
  , propMap        :: PropsMap
  , valsMap        :: ValsMap
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
  retChan <- atomically newTChan
  let m = Map.singleton name (argChan, retChan)
  _ <- forkIO $ fix $ \loop -> do
    args <- atomically $ readTChan argChan
    e <- invoke slot args
    atomically $ do
      writeTChan echan e
      writeTChan retChan ()
    loop
  return (Raw.SlotDefinition cname 43 (fromIntegral $ length metaTypes) pParams, m)

newSignalDef :: Ptr Raw.DosQObject -> Prop -> IO (Raw.SignalDefinition, Map.Map String (TChan QVariant))
newSignalDef qo (Prop name val) = do
  param <- newParameterDef (metaType val)
  pparam <- mallocArray 1
  pokeArray pparam [param]
  notifyName <- newCString $ "notify" <> name
  argChan <- atomically newTChan
  _ <- forkIO $ do
    arr <- mallocArray 1
    fix $ \loop -> do
      newVal <- atomically $ readTChan argChan
      withForeignPtr newVal $ \v -> do
        pokeArray arr [v]
        putStrLn $ "New value for:" <> name
        Q.signalEmit qo notifyName 1 arr
      loop
  return (Raw.SignalDefinition notifyName 1 pparam, Map.singleton name argChan)

newPropertyDef :: Prop -> IO Raw.PropertyDefinition
newPropertyDef (Prop name v) = do
  propName <- newCString name
  propNoti <- newCString $ "notify" <> name
  propGet  <- newCString $ "get" <> name
  let propMt = metaType v
  return $ Raw.PropertyDefinition propName propMt propGet nullPtr propNoti

newQMetaObject :: ValsMap -> TChan e -> String -> [Prop] -> [VSlot e] -> IO QMetaObject
newQMetaObject vmap echan name props slots = do
  let count     = length slots
  slotDefsMap <- mapM (newSlotDef echan) slots
  let slotDefs = map fst slotDefsMap
      maps     = Map.unions $ map snd slotDefsMap
  sigsDefsMap <- mapM (newSignalDef nullPtr) props
  let sigsDefs = map fst sigsDefsMap
      sigs     = Map.unions $ map snd sigsDefsMap
  propDefs    <- mapM newPropertyDef props
  slotBuffer <- mallocArray count
  sigsBuffer <- mallocArray (fromIntegral $ length sigs)
  propBuffer <- mallocArray (fromIntegral $ length propDefs)
  pokeArray slotBuffer slotDefs
  pokeArray sigsBuffer sigsDefs
  pokeArray propBuffer propDefs
  pRawSigsDefs <- malloc
  pRawSlotDefs <- malloc
  pRawPropDefs <- malloc
  poke pRawSigsDefs $ Raw.SignalDefinitions (fromIntegral $ length sigsDefs) sigsBuffer
  poke pRawSlotDefs $ Raw.SlotDefinitions (fromIntegral count) slotBuffer
  poke pRawPropDefs $ Raw.PropertyDefinitions (fromIntegral $ length propDefs) propBuffer
  cname <- newCString name
  staticMetaObject <- Q.qMetaObject
  ptr <- Raw.create staticMetaObject cname pRawSigsDefs pRawSlotDefs pRawPropDefs
  fptr <- F.newForeignPtr ptr (Raw.delete ptr)
  F.addForeignPtrFinalizer fptr $ do
    mapM_ (free . Raw.sltParameters) slotDefs
    mapM_ (free . Raw.sdParameterDefinition) sigsDefs
    free pRawSigsDefs
    free pRawSlotDefs
    free pRawPropDefs
    free slotBuffer
    free sigsBuffer
    free propBuffer
  return $ QMetaObject fptr maps sigs vmap
