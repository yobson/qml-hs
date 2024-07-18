{-# LANGUAGE StrictData, FlexibleInstances, ScopedTypeVariables, PolyKinds, DataKinds, TypeApplications, MultiParamTypeClasses, TypeFamilies, GADTs, GeneralisedNewtypeDeriving, TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.UI.Qml
( App(..)
, QObject
, QViewModel
, CType(..)
, Qml
, modify
, get
, put
, runQApplication
, rootObject
, qObject
, qSlot
, qProperty
) where

import Graphics.UI.Qml.LowLevel.QVariant
import qualified Graphics.UI.Qml.LowLevel.QObject as Q
import Graphics.UI.Qml.LowLevel.QMetaObject
import Graphics.UI.Qml.LowLevel.QApplication
import Graphics.UI.Qml.LowLevel.QQmlApplicationEngine
import Control.Monad.State
import Control.Concurrent.STM

data QViewModel e = QViewModel
  { objName :: String
  , slots :: [VSlot e]
  , props :: [Prop]
  , objs  :: [QViewModel e]
  }

type Qml s a = StateT s IO a

newtype QObject e a = QObj { runQObject :: State (QViewModel e) a }
  deriving (Functor,Applicative,Monad,MonadState (QViewModel e))

data App e s = QmlApp
  { qmlFile      :: FilePath
  , appUpdate    :: e -> Qml s ()
  , appViewModel :: s -> QViewModel e
  }

runQApplication :: App e s -> s -> IO ()
runQApplication (QmlApp qf qu qvm) i = do
  let (QViewModel name slts _ _) = qvm i
  eChan <- atomically newTChan
  metaObj <- newQMetaObject eChan name slts
  qobj <- Q.newQObject name metaObj >>= Q.objToVariant
  app <- initQApplication
  ctx <- newQmlAppEngine app

  setContextProperty ctx "hs" qobj
  loadQml ctx qf

  execQApplication app
  undefined

rootObject :: String -> QObject e a -> QViewModel e
rootObject name (QObj st) = execState st (QViewModel name [] [] [])

qObject :: String -> QObject e a -> QObject e ()
qObject name obj = do
  (QViewModel n s p o) <- get
  put $ QViewModel n s p (rootObject name obj : o)

qSlot :: (IsQMetaType ty) => String -> CType ty -> TySig e ty -> QObject e ()
qSlot name ty cb = do
  (QViewModel n s p o) <- get
  put $ QViewModel n (VSlot (Slot name ty cb):s) p o

qProperty :: (IsQVariant a) => String -> a -> QObject e ()
qProperty name val = do
  (QViewModel n s p o) <- get
  put $ QViewModel n s (Prop name val:p) o
