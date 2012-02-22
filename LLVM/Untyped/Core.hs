{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    addTypeName
    )
where

import Control.Applicative
import Data.Typeable
import Foreign
import Foreign.C.String
import qualified LLVM.FFI.Core as L

newtype LLVM a = LLVM (IO a)
    deriving Monad

newtype Module = Module L.ModuleRef

newtype ModuleProvider = ModuleProvider L.ModuleProviderRef

newtype Type = Type L.TypeRef
    deriving Typeable

-- | Create a new module.
moduleCreateWithName :: String -- ^ Name of module
                        -> LLVM Module
moduleCreateWithName name = LLVM $ Module <$> withCString name L.moduleCreateWithName

-- TODO
disposeModule = undefined
ptrDisposeModule = undefined
                                      
getDataLayout :: Module -> LLVM String
getDataLayout (Module moduleRef) = LLVM $ L.getDataLayout moduleRef >>= peekCAString

setDataLayout :: Module -> String -> LLVM ()
setDataLayout (Module moduleRef) layout = LLVM $ withCString layout L.setDataLayout moduleRef

getTarget :: Module -> LLVM String
getTarget (Module moduleRef) = LLVM $ L.getTarget moduleRef >>= peekCAString

setTarget :: Module -> String -> LLVM ()
setTarget (Module moduleRef) target = LLVM $ withCString target L.setTarget moduleRef

createModuleProviderForExistingModule :: Module -> LLVM ModuleProvider
createModuleProviderForExistingModule (Module moduleRef) =
    ModuleProvider <$> L.createModuleProviderForExistingModule moduleRef

-- TODO
ptrDisposeModuleProvider = undefined

-- What does this return?
addTypeName :: Module -> String -> Type -> LLVM Int
addTypeName (Module moduleRef) name (Type typeRef) = LLVM . withCString name $ \cName ->
    L.addTypeName moduleRef cName typeRef
