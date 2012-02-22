{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LLVM.Untyped.Core
    (
    -- * Core Monad
    LLVM,

    -- * Modules
    Module
    )
where

import Control.Applicative
import Foreign
import Foreign.C.String
import qualified LLVM.FFI.Core as L

newtype LLVM a = LLVM (IO a)
    deriving Monad

data Module = Module L.ModuleRef

-- | Create a new module.
moduleCreateWithName :: String -- ^ Name of module
                        -> LLVM Module
moduleCreateWithName name = LLVM $ Module <$> withCString name L.moduleCreateWithName
                                      
getDataLayout :: Module -> LLVM String
getDataLayout (Module moduleRef) = LLVM $ L.getDataLayout moduleRef >>= peekCAString

setDataLayout :: Module -> String -> LLVM ()
setDataLayout (Module moduleRef) layout = LLVM $ withCString layout L.setDataLayout moduleRef

getTarget :: Module -> LLVM String
getTarget (Module moduleRef) = LLVM $ L.getTarget moduleRef >>= peekCAString

setTarget :: Module -> String -> LLVM ()
setTarget (Module moduleRef) target = LLVM $ withCString target L.setTarget moduleRef
