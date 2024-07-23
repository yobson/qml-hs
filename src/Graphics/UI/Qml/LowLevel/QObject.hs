{-# LANGUAGE StrictData, RankNTypes, TypeFamilies, DataKinds #-}

module Graphics.UI.Qml.LowLevel.QObject where

import qualified Graphics.UI.Qml.Internal.QObject as Raw
import qualified Graphics.UI.Qml.Internal.Types as Raw
import qualified Graphics.UI.Qml.Internal.QVariant as QV
import qualified Graphics.UI.Qml.Internal.String as SV
import qualified Data.Map as Map

import Graphics.UI.Qml.LowLevel.QMetaObject
import Graphics.UI.Qml.LowLevel.QVariant
import Control.Concurrent.STM

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import qualified Foreign.Concurrent as F

data QObject = QObject
  { metaObj :: QMetaObject
  , rawObj  :: ForeignPtr Raw.DosQObject
  }

instance Eq QObject where
  x == y = metaObj x == metaObj y

objectCallback :: CallBackMap -> ValsMap -> IO Raw.DObjectCallback
objectCallback cbm valsm = mkCallBack $ \_ slotName args vars -> do
  sltnmC <- QV.toString slotName
  sltnm  <- peekCString sltnmC
  SV.delete sltnmC
  vargs <- peekArray (fromIntegral args) vars
  case Map.lookup sltnm cbm of
    Just (chan, ret) -> do
      atomically $ writeTChan chan $ tail vargs
      atomically $ readTChan ret
    Nothing -> case Map.lookup sltnm valsm of
                 Just nm -> do
                   val <- readTVarIO nm
                   withForeignPtr val $
                     QV.assign (head vargs)
                   return ()
                 Nothing -> QV.setInt (head vargs) 0

foreign import ccall "wrapper" mkCallBack 
  :: (Ptr () -> Ptr Raw.DosQVariant -> CInt -> Ptr (Ptr Raw.DosQVariant) -> IO ())
  -> IO Raw.DObjectCallback

objToVariant :: QObject -> IO QVariant
objToVariant (QObject mo ro) = do
  ptr <- QV.create
  withForeignPtr ro $ \optr ->
    QV.setQObject ptr optr
  F.newForeignPtr ptr $ do
    touchForeignPtr ro
    touchForeignPtr $ qMetaObjectPtr mo
    QV.delete ptr

instance IsQVariant QObject where
  toQVariant = objToVariant
  metaType _ = 39
  unsafeToQVariant (QObject mo ro) = do
    ptr <- QV.create
    withForeignPtr ro $ \optr ->
      QV.setQObject ptr optr
    return ptr
  fromQVariant = undefined
  setQVarient = undefined

type instance IsList QObject = 'True

newQObject :: String -> QMetaObject -> IO QObject
newQObject name qmo@(QMetaObject mo cbm _ vm qo) = withForeignPtr mo $ \o -> do
  callback <- objectCallback cbm vm
  className <- newCString name
  ptr <- Raw.create (plusPtr nullPtr 1) o callback
  atomically $ writeTVar qo ptr
  Raw.setObjectName ptr className
  fptr <- newForeignPtr Raw.finalizer ptr
  return $ QObject qmo fptr
