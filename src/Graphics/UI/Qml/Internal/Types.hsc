
{-# LANGUAGE StrictData, GeneralizedNewtypeDeriving #-}

module Graphics.UI.Qml.Internal.Types where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable

#include "DOtherSideTypes.h"

type DosQVariant              = ()
type DosQModelIndex           = ()
type DosQAbstractItemModel    = ()
type DosQAbstractListModel    = ()
type DosQAbstractTableModel   = ()
type DosQQmlApplicationEngine = ()
type DosQQuickView            = ()
type DosQQmlContext           = ()
type DosQHashIntQByteArray    = ()
type DosQUrl                  = ()
type DosQMetaObject           = ()
type DosQObject               = ()
type DosQQuickImageProvider   = ()
type DosPixmap                = ()
type DosQPointer              = ()
type DosQMetaObjectConnection = ()

type RequestPixmapCallback = FunPtr (CString -> CInt -> CInt -> CInt -> CInt -> Ptr DosPixmap -> IO ())

type DObjectCallback = FunPtr (Ptr () -> Ptr DosQVariant -> CInt -> Ptr (Ptr DosQVariant) -> IO ())

type RowCountCallback = FunPtr (Ptr () -> Ptr DosQModelIndex -> Ptr CInt -> IO ())

type ColumnCountCallback = FunPtr (Ptr () -> Ptr DosQModelIndex -> Ptr CInt -> IO ())

type DataCallback = FunPtr (Ptr () -> Ptr DosQModelIndex -> CInt -> Ptr DosQVariant -> IO ())

type SetDataCallback = FunPtr (Ptr () -> Ptr DosQModelIndex -> Ptr DosQVariant -> CInt -> Ptr CBool -> IO ())

type RoleNamesCallback = FunPtr (Ptr () -> Ptr DosQHashIntQByteArray -> IO ())

type FlagsCallback = FunPtr (Ptr () -> Ptr DosQModelIndex -> Ptr CInt -> IO ())

type HeaderDataCallback = FunPtr (Ptr () -> CInt -> CInt -> CInt -> Ptr DosQVariant -> IO ())

type IndexCallback = FunPtr (Ptr () -> CInt -> CInt -> Ptr DosQModelIndex -> Ptr DosQModelIndex -> IO ())

type ParentCallback = FunPtr (Ptr () -> Ptr DosQModelIndex -> Ptr DosQModelIndex -> IO ())

type HasChildrenCallback = FunPtr (Ptr () -> Ptr DosQModelIndex -> Ptr CBool -> IO ())

type CanFetchMoreCallback = FunPtr (Ptr () -> Ptr DosQModelIndex -> Ptr CBool -> IO ())

type FetchMoreCallback = FunPtr (Ptr () -> Ptr DosQModelIndex -> IO ())

type CreateDObject = FunPtr (CInt -> Ptr () -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO ())

type DeleteDObject = FunPtr (CInt -> Ptr () -> IO ())

type DosQObjectConnectLambdaCallback = FunPtr (Ptr () -> CInt -> Ptr (Ptr DosQVariant) -> IO ())

type DosQMetaObjectInvokeMethodCallback = FunPtr (Ptr () -> IO ())

data DosQVariantArray = DosQVariantArray
    { dqvaSize :: CInt
    , dqvaData :: Ptr (Ptr DosQVariant)
    } deriving Show

instance Storable DosQVariantArray where
    sizeOf ~_ = (#size struct DosQVariantArray)
    alignment ~_ = alignment (0 :: CInt)

    peek p = DosQVariantArray
        <$> (#peek struct DosQVariantArray, size) p
        <*> (#peek struct DosQVariantArray, data) p

    poke p (DosQVariantArray sz dt) = do
        (#poke struct DosQVariantArray, size) p sz
        (#poke struct DosQVariantArray, data) p dt

data QmlRegisterType = QmlRegisterType
    { qrtMajor            :: CInt
    , qrtMinor            :: CInt
    , qrtUri              :: CString
    , qrtQml              :: CString
    , qrtStaticMetaObject :: Ptr DosQMetaObject
    , qrtCreateDObject    :: CreateDObject
    , qrtDeleteDObject    :: DeleteDObject
    } deriving Show

instance Storable QmlRegisterType where
    sizeOf ~_ = (#size struct QmlRegisterType)
    alignment ~_ = alignment (0 :: CInt)

    peek p = QmlRegisterType
        <$> (#peek struct QmlRegisterType, major) p
        <*> (#peek struct QmlRegisterType, minor) p
        <*> (#peek struct QmlRegisterType, uri) p
        <*> (#peek struct QmlRegisterType, qml) p
        <*> (#peek struct QmlRegisterType, staticMetaObject) p
        <*> (#peek struct QmlRegisterType, createDObject) p
        <*> (#peek struct QmlRegisterType, deleteDObject) p

    poke p (QmlRegisterType major minor uri qml smo cdo ddo) = do
        (#poke struct QmlRegisterType, major) p major
        (#poke struct QmlRegisterType, minor) p minor
        (#poke struct QmlRegisterType, uri) p uri
        (#poke struct QmlRegisterType, qml) p qml
        (#poke struct QmlRegisterType, staticMetaObject) p smo
        (#poke struct QmlRegisterType, createDObject) p cdo
        (#poke struct QmlRegisterType, deleteDObject) p ddo

data ParameterDefinition = ParameterDefinition
    { pdName     :: CString
    , pdMetaType :: CInt
    } deriving Show

instance Storable ParameterDefinition where
    sizeOf    ~_ = (#size struct ParameterDefinition)
    alignment ~_ = alignment (0 :: CInt)

    peek p = ParameterDefinition
        <$> (#peek struct ParameterDefinition, name) p
        <*> (#peek struct ParameterDefinition, metaType) p

    poke p (ParameterDefinition name metaType) = do
        (#poke struct ParameterDefinition, name) p name
        (#poke struct ParameterDefinition, metaType) p metaType

data SignalDefinition = SignalDefinition
    { sdName                :: CString
    , sdParametersCount     :: CInt
    , sdParameterDefinition :: Ptr ParameterDefinition
    } deriving Show

instance Storable SignalDefinition where
    sizeOf    ~_ = (#size struct SignalDefinition)
    alignment ~_ = alignment (0 :: CInt)

    peek p = SignalDefinition
        <$> (#peek struct SignalDefinition, name) p
        <*> (#peek struct SignalDefinition, parametersCount) p
        <*> (#peek struct SignalDefinition, parameters) p

    poke p (SignalDefinition name paramCount params) = do
        (#poke struct SignalDefinition, name) p name
        (#poke struct SignalDefinition, parametersCount) p paramCount
        (#poke struct SignalDefinition, parameters) p params

data SignalDefinitions = SignalDefinitions
    { sigCount       :: CInt
    , sigDefinitions :: Ptr SignalDefinition
    } deriving Show

instance Storable SignalDefinitions where
    sizeOf    ~_ = (#size struct SignalDefinitions)
    alignment ~_ = alignment (0 :: CInt)

    peek p = SignalDefinitions
        <$> (#peek struct SignalDefinitions, count) p
        <*> (#peek struct SignalDefinitions, definitions) p

    poke p (SignalDefinitions count defs) = do
        (#poke struct SignalDefinitions, count) p count
        (#poke struct SignalDefinitions, definitions) p defs


data SlotDefinition = SlotDefinition
    { sltName            :: CString
    , sltReturnMetaType  :: CInt
    , sltParametersCount :: CInt
    , sltParameters      :: Ptr ParameterDefinition
    } deriving Show

instance Storable SlotDefinition where
    sizeOf    ~_ = (#size struct SlotDefinition)
    alignment ~_ = alignment (0 :: CInt)

    peek p = SlotDefinition
        <$> (#peek struct SlotDefinition, name) p
        <*> (#peek struct SlotDefinition, returnMetaType) p
        <*> (#peek struct SlotDefinition, parametersCount) p
        <*> (#peek struct SlotDefinition, parameters) p

    poke p (SlotDefinition name rmt paramCount params) = do
        (#poke struct SlotDefinition, name) p name
        (#poke struct SlotDefinition, returnMetaType) p rmt
        (#poke struct SlotDefinition, parametersCount) p paramCount
        (#poke struct SlotDefinition, parameters) p params

data SlotDefinitions = SlotDefinitions
    { sltCount       :: CInt
    , sltDefinitions :: Ptr SlotDefinition
    } deriving Show

instance Storable SlotDefinitions where
    sizeOf    ~_ = (#size struct SlotDefinitions)
    alignment ~_ = alignment (0 :: CInt)

    peek p = SlotDefinitions
        <$> (#peek struct SlotDefinitions, count) p
        <*> (#peek struct SlotDefinitions, definitions) p

    poke p (SlotDefinitions count defs) = do
        (#poke struct SlotDefinitions, count) p count
        (#poke struct SlotDefinitions, definitions) p defs


data PropertyDefinition = PropertyDefinition
    { propName             :: CString
    , propPropertyMetaType :: CInt
    , propReadSlot         :: CString
    , propWriteSlot        :: CString
    , propNotifySignal     :: CString
    } deriving Show

instance Storable PropertyDefinition where
    sizeOf    ~_ = (#size struct PropertyDefinition)
    alignment ~_ = alignment (0 :: CInt)

    peek p = PropertyDefinition
        <$> (#peek struct PropertyDefinition, name) p
        <*> (#peek struct PropertyDefinition, propertyMetaType) p
        <*> (#peek struct PropertyDefinition, readSlot) p
        <*> (#peek struct PropertyDefinition, writeSlot) p
        <*> (#peek struct PropertyDefinition, notifySignal) p

    poke p (PropertyDefinition name pmt rs ws ns) = do
        (#poke struct PropertyDefinition, name) p name
        (#poke struct PropertyDefinition, propertyMetaType) p pmt
        (#poke struct PropertyDefinition, readSlot) p rs
        (#poke struct PropertyDefinition, writeSlot) p ws
        (#poke struct PropertyDefinition, notifySignal) p ns

data PropertyDefinitions = PropertyDefinitions
    { propCount       :: CInt
    , propDefinitions :: Ptr PropertyDefinition
    } deriving Show

instance Storable PropertyDefinitions where
    sizeOf    ~_ = (#size struct PropertyDefinitions)
    alignment ~_ = alignment (0 :: CInt)

    peek p = PropertyDefinitions
        <$> (#peek struct PropertyDefinitions, count) p
        <*> (#peek struct PropertyDefinitions, definitions) p

    poke p (PropertyDefinitions count defs) = do
        (#poke struct PropertyDefinitions, count) p count
        (#poke struct PropertyDefinitions, definitions) p defs

data DosQAbstractItemModelCallbacks = DosQAbstractItemModelCallbacks
    { rowCount     :: RowCountCallback
    , columnCount  :: ColumnCountCallback
    , dataCallback :: DataCallback
    , setData      :: SetDataCallback
    , roleNames    :: RoleNamesCallback
    , flags        :: FlagsCallback
    , headerData   :: HeaderDataCallback
    , index        :: IndexCallback
    , parent       :: ParentCallback
    , hasChildren  :: HasChildrenCallback
    , canFetchMore :: CanFetchMoreCallback
    , fetchMore    :: FetchMoreCallback
    } deriving Show

instance Storable DosQAbstractItemModelCallbacks where
    sizeOf    ~_ = (#size struct DosQAbstractItemModelCallbacks)
    alignment ~_ = alignment (0 :: CInt)

    peek p = DosQAbstractItemModelCallbacks
        <$> (#peek struct DosQAbstractItemModelCallbacks, rowCount) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, columnCount) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, data) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, setData) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, roleNames) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, flags) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, headerData) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, index) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, parent) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, hasChildren) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, canFetchMore) p
        <*> (#peek struct DosQAbstractItemModelCallbacks, fetchMore) p

    poke p (DosQAbstractItemModelCallbacks rc cc dt sd rn fl hd ix pt hc cf fm) = do
        (#poke struct DosQAbstractItemModelCallbacks, rowCount) p rc
        (#poke struct DosQAbstractItemModelCallbacks, columnCount) p cc
        (#poke struct DosQAbstractItemModelCallbacks, data) p dt
        (#poke struct DosQAbstractItemModelCallbacks, setData) p sd
        (#poke struct DosQAbstractItemModelCallbacks, roleNames) p rn
        (#poke struct DosQAbstractItemModelCallbacks, flags) p fl
        (#poke struct DosQAbstractItemModelCallbacks, headerData) p hd
        (#poke struct DosQAbstractItemModelCallbacks, index) p ix
        (#poke struct DosQAbstractItemModelCallbacks, parent) p pt
        (#poke struct DosQAbstractItemModelCallbacks, hasChildren) p hc
        (#poke struct DosQAbstractItemModelCallbacks, canFetchMore) p cf
        (#poke struct DosQAbstractItemModelCallbacks, fetchMore) p fm

newtype DosQEventLoopProcessEventFlag = DosQEventLoopProcessEventFlag CInt
    deriving (Storable)

#{ enum DosQEventLoopProcessEventFlag, DosQEventLoopProcessEventFlag
 , processAllEvents                  = DosQEventLoopProcessEventFlagProcessAllEvents
 , excludeUserInputEvents            = DosQEventLoopProcessEventFlagExcludeUserInputEvents
 , processExcludeSocketNotifiers     = DosQEventLoopProcessEventFlagProcessExcludeSocketNotifiers
 , processAllEventsWaitForMoreEvents = DosQEventLoopProcessEventFlagProcessAllEventsWaitForMoreEvents
 }

newtype DosQtConnectionType = DosQtConnectionType CInt
    deriving (Storable)

#{ enum DosQtConnectionType, DosQtConnectionType
 , autoConnection     = DosQtConnectionTypeAutoConnection
 , directConnection   = DosQtConnectionTypeDirectConnection
 , queuedConnection   = DosQtConnectionTypeQueuedConnection
 , blockingConnection = DosQtConnectionTypeBlockingConnection
 , uniqueConnection   = DosQtCOnnectionTypeUniqueConnection
 }
