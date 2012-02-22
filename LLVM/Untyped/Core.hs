{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LLVM.Untyped.Core
    (
    LLVM,

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
