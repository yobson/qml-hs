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
import Control.Monad.Fix
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Applicative

import Foreign.ForeignPtr

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
  { qmlFile        :: FilePath
  , appUpdate      :: e -> Qml s ()
  , appViewModel   :: s -> QViewModel e
  , externalEvents :: Maybe (TChan e)
  }

mkPropsMap :: [Prop] -> IO (Map.Map String (TVar QVariant))
mkPropsMap props = Map.fromList <$> mapM go props
  where go (Prop n v) = do
              var <- toQVarient v
              tvar <- newTVarIO var
              return (n, tvar)

runQApplication :: App e s -> s -> IO ()
runQApplication (QmlApp qf qu qvm custom) i = do
  let vm@(QViewModel name slts props _) = qvm i
  eChan <- atomically newTChan
  pm <- mkPropsMap props
  st <- newTVarIO i
  lastVm <- newTVarIO vm
  metaObj <- newQMetaObject pm eChan name props slts
  qobj <- Q.newQObject name metaObj
  qobjv <- Q.objToVariant qobj
  app <- initQApplication
  ctx <- newQmlAppEngine app

  setContextProperty ctx name qobjv
  loadQml ctx qf

  _ <- forkIO $ fix $ \loop -> do
    e <- atomically $ readTChan eChan <|> maybe empty readTChan custom
    stt <- readTVarIO st
    newSt <- execStateT (qu e) stt
    atomically $ writeTVar st newSt
    oldVm <- readTVarIO lastVm
    let newVm = qvm newSt
        propM = propMap metaObj
    diff <- diffViewModels oldVm newVm
    atomically $ writeTVar lastVm newVm
    forM_ diff $ \(n, newVal) -> atomically $ do
      case Map.lookup n pm of
        Just chan -> writeTVar chan newVal
        Nothing -> return ()
      case Map.lookup n propM of
        Just chan -> writeTChan chan newVal
        Nothing -> return ()
    loop

  execQApplication app
  touchForeignPtr $ Q.rawObj qobj

-- Completely Stupid
diffViewModels :: QViewModel e -> QViewModel e -> IO [(String, QVariant)]
diffViewModels (QViewModel _ _ [] _) (QViewModel _ _ [] _) = return []
diffViewModels (QViewModel _ _ ((Prop n v1):xs) _) (QViewModel _ _ ((Prop _ v2):ys) _) = do 
  theSame <- sameVar v1 v2
  if theSame 
     then diffViewModels (QViewModel "" [] xs []) (QViewModel "" [] ys [])
     else do
      nxt <- diffViewModels (QViewModel "" [] xs []) (QViewModel "" [] ys [])
      v <- toQVarient v2
      return $ (n,v) : nxt
diffViewModels _ _ = return []

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
