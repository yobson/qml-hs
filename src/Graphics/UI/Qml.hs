{-# LANGUAGE StrictData, FlexibleInstances, ScopedTypeVariables, PolyKinds, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GADTs, GeneralisedNewtypeDeriving #-}
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

-- | A monad which is @MonadState s@ and @MonadIO@
type Qml s a = StateT s IO a

newtype QObject e a = QObj (State (QViewModel e) a)
  deriving (Functor,Applicative,Monad,MonadState (QViewModel e))

data App e s = QmlApp
  { qmlFile        :: FilePath
  , appUpdate      :: e -> Qml s ()
  , appViewModel   :: s -> QViewModel e
  , externalEvents :: Maybe (TChan e)
  }

mkPropsMap :: [Prop] -> IO (Map.Map String (TVar QVariant))
mkPropsMap prps = Map.fromList <$> mapM go prps
  where go (Prop n v) = do
              var <- toQVarient v
              tvar <- newTVarIO var
              return (n, tvar)

runQApplication :: App e s -> s -> IO ()
runQApplication (QmlApp qf qu qvm custom) i = do
  let vm = qvm i
  eChan <- atomically newTChan
  pm <- mkPropsMap $ props vm
  st <- newTVarIO i
  lastVm <- newTVarIO vm
  metaObj <- newQMetaObject pm eChan (objName vm) (props vm) (slots vm)
  qobj <- Q.newQObject (objName vm) metaObj
  qobjv <- Q.objToVariant qobj
  app <- initQApplication
  ctx <- newQmlAppEngine app

  setContextProperty ctx (objName vm) qobjv
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
  vm <- get
  put $ vm{objs = rootObject name obj : objs vm}

qSlot :: (IsQMetaType ty) => String -> CType ty -> TySig e ty -> QObject e ()
qSlot name ty cb = do
  vm <- get
  put $ vm{slots = VSlot (Slot name ty cb) : slots vm}

qProperty :: (IsQVariant a) => String -> a -> QObject e ()
qProperty name val = do
  vm <- get
  put $ vm{props = Prop name val : props vm}
