{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module LLVM.Untyped.Core
    (
    -- * Core Monad
    LLVM,

    -- * Modules
    Module,
    moduleCreateWithName,
    getDataLayout,
    setDataLayout,
    getTarget,
    setTarget,

    -- * Module Providers
    ModuleProvider,
    createModuleProviderForExistingModule,

    -- * Types
    Type,
    L.TypeKind,
    addTypeName,
    deleteTypeName,
    getTypeKind,
    
    -- * Integer Types
    int1Type,
    int8Type,
    int16Type,
    int32Type,
    integerType,
    getIntTypeWidth,
    
    -- * Float Types
    floatType,
    doubleType,
    x86FP80Type,
    fp128Type,
    ppcFP128Type
    )
where

import Control.Applicative
import Data.Typeable
import Foreign
import Foreign.C.String
import qualified LLVM.FFI.Core as L

newtype LLVM a = LLVM (IO a)
    deriving (Applicative, Functor, Monad)

newtype Module = Module L.ModuleRef

newtype ModuleProvider = ModuleProvider L.ModuleProviderRef

newtype Type = Type L.TypeRef
    deriving Typeable

-- | Create a new module.
moduleCreateWithName :: String -- ^ Name of module
                        -> LLVM Module
moduleCreateWithName name = LLVM $ Module <$> withCString name L.moduleCreateWithName

disposeModule :: Module -> LLVM ()
disposeModule (Module moduleRef) = LLVM $ L.disposeModule moduleRef
                                      
getDataLayout :: Module -> LLVM String
getDataLayout (Module moduleRef) = LLVM $ L.getDataLayout moduleRef >>= peekCAString

setDataLayout :: Module -> String -> LLVM ()
setDataLayout (Module moduleRef) layout = LLVM $ withCString layout (L.setDataLayout moduleRef)

getTarget :: Module -> LLVM String
getTarget (Module moduleRef) = LLVM $ L.getTarget moduleRef >>= peekCAString

setTarget :: Module -> String -> LLVM ()
setTarget (Module moduleRef) target = LLVM $ withCString target (L.setTarget moduleRef)

createModuleProviderForExistingModule :: Module -> LLVM ModuleProvider
createModuleProviderForExistingModule (Module moduleRef) =
    LLVM $ ModuleProvider <$> L.createModuleProviderForExistingModule moduleRef

-- What does this return?
addTypeName :: Module -> String -> Type -> LLVM Int
addTypeName (Module moduleRef) name (Type typeRef) = 
    LLVM . withCString name $ \cName -> fromIntegral <$> L.addTypeName moduleRef cName typeRef

deleteTypeName :: Module -> String -> LLVM ()
deleteTypeName (Module moduleRef) name = LLVM . withCString name $ L.deleteTypeName moduleRef

getTypeKind :: Type -> LLVM L.TypeKind
getTypeKind (Type typeRef) = LLVM $ L.getTypeKind typeRef

int1Type :: Type
int1Type = Type L.int1Type

int8Type :: Type
int8Type = Type L.int8Type

int16Type :: Type
int16Type = Type L.int16Type

int32Type :: Type
int32Type = Type L.int32Type

integerType :: Int -- ^ Width in bits
               -> Type
integerType = Type . L.integerType . fromIntegral

getIntTypeWidth :: Type -> LLVM Int
getIntTypeWidth (Type typeRef) = LLVM $ fromIntegral <$> L.getIntTypeWidth typeRef

floatType :: Type
floatType = Type L.floatType

doubleType :: Type
doubleType = Type L.doubleType

x86FP80Type :: Type
x86FP80Type = Type L.x86FP80Type

fp128Type :: Type
fp128Type = Type L.fp128Type

ppcFP128Type :: Type
ppcFP128Type = Type L.ppcFP128Type
