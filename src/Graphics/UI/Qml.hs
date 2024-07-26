{-# LANGUAGE StrictData, FlexibleInstances, ScopedTypeVariables, PolyKinds, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GADTs, GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, TupleSections, FlexibleContexts, TypeApplications, InstanceSigs #-}

module Graphics.UI.Qml
( App(..)
, QObject
, QViewModel
, CType(..)
, Qml
, JsonData(..)
, QmlFile(..)
, quit
, modify
, get
, put
, runQApplication
, rootObject
, qSlot
, qProperty
, properties
) where

import Data.Aeson
import Graphics.UI.Qml.LowLevel.QVariant
import qualified Graphics.UI.Qml.LowLevel.QObject as Q
import Graphics.UI.Qml.LowLevel.QMetaObject
import Graphics.UI.Qml.LowLevel.QApplication
import qualified Graphics.UI.Qml.Internal.QApplication as Raw
import Graphics.UI.Qml.LowLevel.QQmlApplicationEngine
import Control.Monad.Fix
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map ((!))
import Control.Applicative

import Foreign.ForeignPtr

data QViewModel e = QViewModel
  { objName   :: String
  , slots     :: [VSlot e]
  , dataModel :: QDataModel
  }


data QDataModel = DataModel
  { dmName :: String
  , props  :: [Prop]
  , objs   :: [QDataModel]
  } deriving Show
  

-- | A monad which is @MonadState s@ and @MonadIO@
type Qml s a = StateT s IO a

newtype QObject e a = QObj (State (QViewModel e) a)
  deriving (Functor,Applicative,Monad,MonadState (QViewModel e))

data QmlFile = QmlFile FilePath | QRC FilePath [String] String

data App e s = QmlApp
  { qmlFile        :: QmlFile
  , appUpdate      :: e -> Qml s ()
  , appViewModel   :: s -> QViewModel e
  , externalEvents :: Maybe (TChan e)
  }

mkPropsMap :: [Prop] -> IO (Map.Map String (TVar QVariant))
mkPropsMap prps = Map.fromList <$> mapM go prps
  where go (Prop n v) = do
              var <- toQVariant v
              tvar <- newTVarIO var
              return (n, tvar)

helperObj2Prop :: (String, Q.QObject) -> IO Prop
helperObj2Prop (name, obj) = do
  var <- toQVariant obj
  return $ Prop name var

helperObj2PropList :: (String, [Q.QObject]) -> IO Prop
helperObj2PropList (name, objs) = do
  var <- toQVariant objs
  return $ Prop name var

prop2variant :: Prop -> IO QVariant
prop2variant (Prop n v) = toQVariant v

innerQObject :: TChan (Maybe QDataModel) -> QDataModel -> IO (String, Q.QObject)
innerQObject sChan dm = do
  oldDm <- newTVarIO dm
  eChan <- newTChanIO

  ioMap <- forM (objs dm) $ \iDM -> do
    sChan' <- newTChanIO
    ret <- innerQObject sChan' iDM
    return (ret, (dmName iDM, sChan'))
  

  ps <- mapM (helperObj2Prop . fst) ioMap
  pm <- mkPropsMap $ ps <> props dm

  metaObj <- newQMetaObject pm eChan (dmName dm) (ps <> props dm) []
  qobj <- Q.newQObject (dmName dm) metaObj

  let innerObjectMap = Map.fromList $ map snd ioMap

  forkIO $ fix $ \loop -> do
    newMod <- atomically $ readTChan sChan
    case newMod of
      Nothing -> do
        let toKill = map snd $ Map.toList innerObjectMap
        mapM_ (\chan -> atomically $ writeTChan chan Nothing) toKill
        touchForeignPtr $ Q.rawObj qobj
      Just newDm -> do
        old <- readTVarIO oldDm
        -- update inner states
        forM_ (objs newDm) $ \dm' -> do
          case Map.lookup (dmName dm') innerObjectMap of
            Just chan -> atomically $ writeTChan chan $ Just dm'
            Nothing -> return ()
          let liveMap = map dmName $ objs newDm
              toKill  = map snd $ filter (\(n,_) -> n `notElem` liveMap) $ Map.toList innerObjectMap
          mapM_ (\chan -> atomically $ writeTChan chan Nothing) toKill
        -- update our state
        diff <- diffViewModels old newDm
        forM_ diff $ \(n, newVal) -> atomically $ do
          case Map.lookup n pm of
            Just chan -> writeTVar chan newVal
            Nothing -> return ()
          case Map.lookup n $ propMap metaObj of
            Just chan -> writeTChan chan newVal
            Nothing -> return ()
    loop
  return (dmName dm, qobj)

quit :: Qml s ()
quit = liftIO Raw.quit

runQApplication :: App e s -> s -> IO ()
runQApplication (QmlApp qf qu qvm custom) i = do
  let vm = qvm i
  eChan <- atomically newTChan

  ioMap <- forM (objs $ dataModel vm) $ \iDM -> do
    sChan' <- newTChanIO
    ret <- innerQObject sChan' iDM
    return (ret, (dmName iDM, sChan'))

  ps <- mapM (helperObj2Prop . fst) ioMap
  let innerObjectMap = Map.fromList $ map snd ioMap

  pm <- mkPropsMap $ ps <> props (dataModel vm)
  st <- newTVarIO i
  lastVm <- newTVarIO vm


  metaObj <- newQMetaObject pm eChan (objName vm) (ps <> props (dataModel vm)) (slots vm)
  qobj <- Q.newQObject (objName vm) metaObj
  qobjv <- Q.objToVariant qobj
  -- SPLIT
  app <- initQApplication
  ctx <- newQmlAppEngine app

  setContextProperty ctx (objName vm) qobjv
  case qf of
    QmlFile file -> loadQml ctx file
    QRC file imports url -> do
      loadResource ctx file imports url

  forkIO $ fix $ \loop -> do
    e <- atomically $ readTChan eChan <|> maybe empty readTChan custom
    stt <- readTVarIO st
    newSt <- execStateT (qu e) stt
    atomically $ writeTVar st newSt
    oldVm <- readTVarIO lastVm
    let newVm = qvm newSt
        propM = propMap metaObj
    forM_ (objs $ dataModel newVm) $ \dm' -> do
      case Map.lookup (dmName dm') innerObjectMap of
        Just chan -> atomically $ writeTChan chan $ Just dm'
        Nothing -> return ()
      let liveMap = map dmName $ objs $ dataModel newVm
          toKill  = map snd $ filter (\(n,_) -> n `notElem` liveMap) $ Map.toList innerObjectMap
      mapM_ (\chan -> atomically $ writeTChan chan Nothing) toKill
    diff <- diffViewModels (dataModel oldVm) (dataModel newVm)
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

lookupProp :: String -> [Prop] -> Maybe Prop
lookupProp _ [] = Nothing
lookupProp n (p@(Prop pn _):xs) | n == pn   = Just p
                                | otherwise = lookupProp n xs

-- TODO: Not very performant
diffViewModels :: QDataModel -> QDataModel -> IO [(String, QVariant)]
diffViewModels (DataModel _ [] _) DataModel{} = return []
diffViewModels (DataModel _ ((Prop n v1):xs) _) (DataModel _ ps _) = do 
  d <- case lookupProp n ps of
    Nothing -> (:[]) . (n,) <$> toQVariant ""
    Just (Prop _ v2) -> do
      theSame <- sameVar v1 v2
      if theSame 
         then return []
         else do
          v <- toQVariant v2
          return [(n,v)]
  nxt <- diffViewModels (DataModel [] xs []) (DataModel [] ps [])
  return $ d <> nxt


rootObject :: String -> QObject e a -> QViewModel e
rootObject name (QObj st) = execState st (QViewModel name [] (DataModel [] [] []))

properties :: State QDataModel () -> QObject e ()
properties xm = let dm = execState xm (DataModel "" [] []) in
                   modify (\vm -> vm {dataModel = dm})

qSlot :: (IsQMetaType ty) => String -> CType ty -> TySig e ty -> QObject e ()
qSlot name ty cb = do
  vm <- get
  put $ vm{slots = VSlot (Slot name ty cb) : slots vm}

class IsQProperty a where
  mkProp :: String -> a -> State QDataModel ()

mkProp' :: (IsQVariant a) => String -> a -> State QDataModel ()
mkProp' name val = do
  vm <- get
  put $ vm{props = Prop name val : props vm}

instance IsQProperty Int where mkProp = mkProp'
instance IsQProperty String where mkProp = mkProp'
instance IsQProperty Bool where mkProp = mkProp'
instance IsQProperty [Int] where mkProp = mkProp'
instance IsQProperty [String] where mkProp = mkProp'
instance IsQProperty [Bool] where mkProp = mkProp'
instance (ToJSON a, FromJSON a, Eq a) => IsQProperty (JsonData a) where mkProp = mkProp'

instance IsQProperty (State QDataModel ()) where
  mkProp name obj = do
    vm <- get
    let o = execState obj $ DataModel name [] []
    put $ vm{objs = o : objs vm}

qProperty :: (IsQProperty a) => String -> a -> State QDataModel ()
qProperty = mkProp
