{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module LLVM.Untyped.Core
    (
    -- * Core Monad
    LLVM

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
    
    -- ** Integer Types
    int1Type,
    int8Type,
    int16Type,
    int32Type,
    integerType,
    getIntTypeWidth,
    
    -- ** Float Types
    floatType,
    doubleType,
    x86FP80Type,
    fp128Type,
    ppcFP128Type,

    -- ** Function Types
    functionType,
    isFunctionVarArg,
    getReturnType,
    countParamTypes,
    getParamTypes,

    -- ** Miscellanious Types
    voidType,
    labelType,
    opaqueType,
    
    -- ** Array Types, Pointer Types, Vector Types
    arrayType,
    pointerType,
    vectorType,
    getElementType,
    getArrayLength,
    getPointerAddressSpace,
    getVectorSize,

    -- ** Struct Types
    structType,
    countStructElementTypes,
    getStructElementTypes,
    isPackedStruct,

    -- * Type Handles
    TypeHandle,
    createTypeHandle,
    refineType,
    resolveTypeHandle,
    disposeTypeHandle
    )
where

import Control.Applicative
import Data.Typeable
import Foreign hiding (unsafePerformIO)
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import qualified LLVM.FFI.Core as L

newtype LLVM a = LLVM (IO a)
    deriving (Applicative, Functor, Monad)

newtype Module = Module L.ModuleRef

newtype ModuleProvider = ModuleProvider L.ModuleProviderRef

newtype Type = Type { untype :: L.TypeRef }
    deriving Typeable

newtype TypeHandle = TypeHandle L.TypeHandleRef

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

-- | We require IO to allocate an array. This is abstracted out with
-- unsafePerformIO.
functionType :: Type -- ^ Return Type
                -> [Type] -- ^ Argument Types
                -> Bool -- ^ Is Var Arg?
                -> Type
functionType (Type retType) argTypes isVarArg = unsafePerformIO $ do
    let argTypes' = map untype argTypes
        argCount = fromIntegral . length $ argTypes'
        isVarArg' = fromIntegral $ case isVarArg of
            True -> 1 -- Is this right?
            False -> 0
    resultRef <- withArray argTypes' $ \argTypesPtr -> return $
        L.functionType retType argTypesPtr argCount isVarArg'
    return $ Type resultRef

-- Theoretically a lot of these don't need to be in the LLVM monad.
-- Should experiment.

isFunctionVarArg :: Type -> LLVM Int
isFunctionVarArg (Type typeRef) = LLVM $ fromIntegral <$> L.isFunctionVarArg typeRef

getReturnType :: Type -> LLVM Type
getReturnType (Type typeRef) = LLVM $ Type <$> L.getReturnType typeRef

countParamTypes :: Type -> LLVM Int
countParamTypes (Type typeRef) = LLVM $ fromIntegral <$> L.countParamTypes typeRef

-- This function in particular is probably safe to unsafePerformIO on.
getParamTypes :: Type -> LLVM [Type]
getParamTypes functionType'@(Type functionType) = 
    do numParams <- countParamTypes functionType'
       LLVM $ do
        typeArray <- mallocArray numParams
        L.getParamTypes functionType typeArray
        typeList <- peekArray numParams typeArray
        free typeArray
        return $ map Type typeList

voidType :: Type
voidType = Type L.voidType

labelType :: Type
labelType = Type L.labelType

opaqueType :: Type
opaqueType = Type L.opaqueType

arrayType :: Type -- ^ Element type
             -> Int -- ^ Number of elements
             -> Type
arrayType (Type elementType) n = Type $ L.arrayType elementType (fromIntegral n)

pointerType :: Type -- ^ Element type
               -> Int -- ^ Address space
               -> Type
pointerType (Type elementType) n = Type $ L.pointerType elementType (fromIntegral n)

vectorType :: Type -- ^ Element type
              -> Int -- ^ Number of elements
              -> Type
vectorType (Type elementType) n = Type $ L.vectorType elementType (fromIntegral n)

-- | Get the element type of a sequential type, or the type of an individual element.
getElementType :: Type -> LLVM Type
getElementType (Type typeRef) = LLVM $ Type <$> L.getElementType typeRef

getArrayLength :: Type -> LLVM Int
getArrayLength (Type typeRef) = LLVM $ fromIntegral <$> L.getArrayLength typeRef

getPointerAddressSpace :: Type -> LLVM Int
getPointerAddressSpace (Type typeRef) = LLVM $ fromIntegral <$> L.getPointerAddressSpace typeRef

getVectorSize :: Type -> LLVM Int
getVectorSize (Type typeRef) = LLVM $ fromIntegral <$> L.getVectorSize typeRef

structType :: [Type] -- ^ List of Types that the struct contains
              -> Bool -- ^ Is the struct packed?
              -> Type
structType typeList packed = unsafePerformIO $
    let typeRefList = map untype typeList
        packed' = fromIntegral $ case packed of
            True -> 1
            False -> 0
        len = fromIntegral $ length typeRefList
    in Type <$> (withArray typeRefList $ do \typeRefPtr -> return $ L.structType typeRefPtr len packed')

countStructElementTypes :: Type -> Int
countStructElementTypes (Type typeRef) = fromIntegral $ L.countStructElementTypes typeRef

getStructElementTypes :: Type -> LLVM [Type]
getStructElementTypes structType'@(Type structType) = LLVM $ do
    let count = countStructElementTypes structType'
    typeArray <- mallocArray count
    L.getStructElementTypes structType typeArray
    typeList <- peekArray count typeArray
    free typeArray
    return $ map Type typeList

isPackedStruct :: Type -> Bool
isPackedStruct (Type typeRef) = case L.isPackedStruct typeRef of
                                    0 -> False
                                    _ -> True

createTypeHandle :: Type -> LLVM TypeHandle
createTypeHandle (Type typeRef) = LLVM $ TypeHandle <$> L.createTypeHandle typeRef

-- I don't really know what this does
refineType :: Type -> Type -> LLVM ()
refineType (Type t1) (Type t2) = LLVM $ L.refineType t1 t2

resolveTypeHandle :: TypeHandle -> LLVM Type
resolveTypeHandle (TypeHandle handle) = LLVM $ Type <$> L.resolveTypeHandle handle

disposeTypeHandle :: TypeHandle -> LLVM ()
disposeTypeHandle (TypeHandle handle) = LLVM $ L.disposeTypeHandle handle
