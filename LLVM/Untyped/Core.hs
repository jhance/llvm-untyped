{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module LLVM.Untyped.Core
    (
    -- * Core Monad
    LLVM,
    runLLVM,

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
    int64Type,
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
    disposeTypeHandle,

    -- * Values
    Value,
    typeOf,
    getValueName,
    setValueName,
    dumpValue,

    -- ** Constants
    constNull,
    constAllOnes,
    getUndef,
    isConstant,
    isNull,
    isUndef,

    -- ** Linkage
    Linkage,
    getLinkage,
    setLinkage,

    -- ** Visibility, Sections, and Alignment
    getVisibility,
    setVisibility,
    isDeclaration,
    getSection,
    setSection,
    getAlignment,
    setAlignment,

    -- ** Globals
    addGlobal,
    getNamedGlobal,
    deleteGlobal,
    getInitializer,
    setInitializer,
    isThreadLocal,
    setThreadLocal,
    isGlobalConstant,
    setGlobalConstant,
    getFirstGlobal,
    getNextGlobal,
    getPreviousGlobal,
    getLastGlobal,
    getGlobalParent,

    -- ** Functions
    addFunction,
    getNamedFunction,
    deleteFunction,
    countParams,
    getParams,
    getIntrinsicID,
    getGC,
    setGC,
    getFirstFunction,
    getNextFunction,
    getPreviousFunction,
    getLastFunction,
    getFirstParam,
    getNextParam,
    getPreviousParam,
    getLastParam,
    getParamParent,
    isTailCall,
    setTailCall,

    -- ** Calling Conventions
    CallingConvention,
    getFunctionCallConv,
    setFunctionCallConv,
    getInstructionCallConv,
    setInstructionCallConv,

    -- * Constants
    -- ** Scalar Constants
    constInt,
    constReal,

    -- ** Constant Operations
    LLVM.Untyped.Core.sizeOf,
    constNeg,
    constNot,
    constAdd,
    constSub,
    constMul,
    constExactSDiv,
    constFAdd,
    constFMul,
    constFNeg,
    constFPCast,
    constFSub,
    constUDiv,
    constSDiv,
    constFDiv,
    constURem,
    constSRem,
    constFRem,
    constAnd,
    constOr,
    constXor,
    constICmp,
    constFCmp,
    constShl,
    constLShr,
    constAShr,

    -- * Basic Blocks
    BasicBlock,
    basicBlockAsValue,
    valueIsBasicBlock,
    valueAsBasicBlock,
    countBasicBlocks,
    getBasicBlocks,
    getEntryBasicBlock,
    appendBasicBlock,
    insertBasicBlock,
    deleteBasicBlock,
    getFirstBasicBlock,
    getNextBasicBlock,
    getPreviousBasicBlock,
    getLastBasicBlock,
    getInsertBlock,
    getBasicBlockParent,

    -- * Instruction Building
    Builder,
    createBuilder,
    positionBuilder,
    positionBefore,
    positionAtEnd,
    getFirstInstruction,
    getNextInstruction,
    getPreviousInstruction,
    getLastInstruction,
    getInstructionParent,

    -- ** Terminators
    buildRetVoid,
    buildRet,
    buildBr,
    --buildIndirectBr,
    buildCondBr,
    --buildSwitch,
    --buildInvoke,
    buildUnwind,
    buildUnreachable,

    -- ** Arithmetic
    buildAdd,
    buildSub,
    buildMul,
    buildFAdd,
    buildFMul,
    buildFPCast,
    buildFSub,
    buildUDiv,
    buildSDiv,
    buildExactSDiv,
    buildFDiv,
    buildURem,
    buildSRem,
    buildFRem,
    buildShl,
    buildLShr,
    buildAShr,
    buildAnd,
    buildOr,
    buildXor,
    buildNeg,
    buildFNeg,
    buildNot,
    buildNSWMul,
    buildNSWNeg,
    buildNSWSub,
    buildNUWAdd,
    buildNUWMul,
    buildNUWNeg,
    buildNUWSub,

    -- ** Memory
    buildMalloc,
    buildArrayMalloc,
    buildAlloca,
    buildArrayAlloca,
    buildFree,
    buildLoad,
    buildStore,
    --buildGEP,

    -- ** Comparison
    IntComparison(..),
    RealComparison(..),
    buildICmp,
    buildFCmp,
    
    -- ** Bit Writer
    writeBitcodeToFile
    )
where

import Control.Applicative
import Data.Typeable (Typeable)
import Foreign hiding (unsafePerformIO)
import Foreign.C.String hiding (withCString)
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import qualified LLVM.FFI.BitWriter as L
import LLVM.FFI.Core (CallingConvention, Linkage, Visibility)
import qualified LLVM.FFI.Core as L

newtype LLVM a = LLVM { runLLVM :: IO a }
    deriving (Applicative, Functor, Monad)

newtype BasicBlock = BasicBlock L.BasicBlockRef

newtype Builder = Builder L.BuilderRef

newtype Module = Module L.ModuleRef

newtype ModuleProvider = ModuleProvider L.ModuleProviderRef

newtype Type = Type { untype :: L.TypeRef }
    deriving Typeable

newtype TypeHandle = TypeHandle L.TypeHandleRef

newtype Value = Value { unvalue :: L.ValueRef }

data IntComparison =
    IntEQ
    | IntNE
    | IntUGT
    | IntUGE
    | IntULT
    | IntULE
    | IntSGT
    | IntSGE
    | IntSLT
    | IntSLE

data RealComparison =
    RealPredicateFalse
    | RealOEQ
    | RealOGT
    | RealOGE
    | RealOLT
    | RealOLE
    | RealONE
    | RealORD
    | RealUNO
    | RealUEQ
    | RealUGT
    | RealUGE
    | RealULT
    | RealULE
    | RealUNE
    | RealPredicateTrue

intToBool :: CInt -> Bool
intToBool i = case i of
                0 -> False
                _ -> True

boolToInt :: Bool -> CInt
boolToInt True = 1
boolToInt False = 0

-- | withCString that doesn't free memory after, but rather uses a foreign ptr
withCString :: String -> (CString -> IO a) -> IO a
withCString s f = do cs <- newCAString s
                     --csf <- newForeignPtr finalizerFree cs
                     --withForeignPtr csf $ \cs' -> f cs
                     f cs


-- | Create a new module.
moduleCreateWithName :: String -- ^ Name of module
                        -> LLVM Module
moduleCreateWithName name = LLVM $ Module <$> withCString name L.moduleCreateWithName

disposeModule :: Module -> LLVM ()
disposeModule (Module moduleRef) = LLVM $ L.disposeModule moduleRef
                                      
getDataLayout :: Module -> LLVM String
getDataLayout (Module moduleRef) = LLVM $ L.getDataLayout moduleRef >>= peekCString

setDataLayout :: Module -> String -> LLVM ()
setDataLayout (Module moduleRef) layout = LLVM $ withCString layout (L.setDataLayout moduleRef)

getTarget :: Module -> LLVM String
getTarget (Module moduleRef) = LLVM $ L.getTarget moduleRef >>= peekCString

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

int64Type :: Type
int64Type = Type L.int64Type

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
                -> LLVM Type
functionType (Type retType) argTypes isVarArg = LLVM $ do
    let argTypes' = map untype argTypes
        argCount = fromIntegral . length $ argTypes'
        isVarArg' = fromIntegral $ case isVarArg of
            True -> 1
            False -> 0
    array <- newArray argTypes'
    return . Type $ L.functionType retType array argCount isVarArg'

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

-- | This implementation is wrong. Don't use. Ever.
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
isPackedStruct (Type typeRef) = intToBool $ L.isPackedStruct typeRef

createTypeHandle :: Type -> LLVM TypeHandle
createTypeHandle (Type typeRef) = LLVM $ TypeHandle <$> L.createTypeHandle typeRef

-- I don't really know what this does
refineType :: Type -> Type -> LLVM ()
refineType (Type t1) (Type t2) = LLVM $ L.refineType t1 t2

resolveTypeHandle :: TypeHandle -> LLVM Type
resolveTypeHandle (TypeHandle handle) = LLVM $ Type <$> L.resolveTypeHandle handle

disposeTypeHandle :: TypeHandle -> LLVM ()
disposeTypeHandle (TypeHandle handle) = LLVM $ L.disposeTypeHandle handle

typeOf :: Value -> LLVM Type
typeOf (Value value) = LLVM $ Type <$> L.typeOf value

getValueName :: Value -> LLVM String
getValueName (Value value) = LLVM $ L.getValueName value >>= peekCString

setValueName :: Value -> String -> LLVM ()
setValueName (Value value) name = LLVM $ withCString name (L.setValueName value)

dumpValue :: Value -> LLVM ()
dumpValue (Value value) = LLVM $ L.dumpValue value

constNull :: Type -> Value
constNull (Type t) = Value $ L.constNull t

constAllOnes :: Type -> Value
constAllOnes (Type t) = Value $ L.constAllOnes t

getUndef :: Type -> Value
getUndef (Type t) = Value $ L.getUndef t

isConstant :: Value -> LLVM Bool
isConstant (Value value) = LLVM $ intToBool <$> L.isConstant value

isNull :: Value -> LLVM Bool
isNull (Value value) = LLVM $ intToBool <$> L.isNull value

isUndef :: Value -> LLVM Bool
isUndef (Value value) = LLVM $ intToBool <$> L.isUndef value

getLinkage :: Value -> LLVM Linkage
getLinkage (Value value) = LLVM $ L.toLinkage <$> L.getLinkage value

setLinkage :: Value -> Linkage -> LLVM ()
setLinkage (Value value) linkage = LLVM $ L.setLinkage value (L.fromLinkage linkage)

getVisibility :: Value -> LLVM Visibility
getVisibility (Value value) = LLVM $ L.toVisibility <$> L.getVisibility value

setVisibility :: Value -> Visibility -> LLVM ()
setVisibility (Value value) vis = LLVM $ L.setVisibility value (L.fromVisibility vis)

isDeclaration :: Value -> LLVM Bool
isDeclaration (Value value) = LLVM $ intToBool <$> L.isDeclaration value

getSection :: Value -> LLVM String
getSection (Value value) = LLVM $ L.getSection value >>= peekCString

setSection :: Value -> String -> IO ()
setSection (Value value) name = withCString name (L.setSection value)

getAlignment :: Value -> LLVM Int
getAlignment (Value value) = LLVM $ fromIntegral <$> L.getAlignment value

setAlignment :: Value -> Int -> LLVM ()
setAlignment (Value value) a = LLVM $ L.setAlignment value (fromIntegral a)

addGlobal :: Module -> Type -> String -> LLVM Value
addGlobal (Module mod) (Type t) name = LLVM $ Value <$> withCString name (L.addGlobal mod t)

getNamedGlobal :: Module -> String -> LLVM Value
getNamedGlobal (Module mod) name = LLVM $ Value <$> withCString name (L.getNamedGlobal mod)

deleteGlobal :: Value -> LLVM ()
deleteGlobal (Value value) = LLVM $ L.deleteGlobal value

getInitializer :: Value -> LLVM Value
getInitializer (Value value) = LLVM $ Value <$> L.getInitializer value

-- | Need to look up which argument is which...
-- The implementation of my code is correct though.
setInitializer :: Value
                  -> Value
                  -> LLVM ()
setInitializer (Value v1) (Value v2) = LLVM $ L.setInitializer v1 v2

isThreadLocal :: Value -> LLVM Bool
isThreadLocal (Value value) = LLVM $ intToBool <$> L.isThreadLocal value

setThreadLocal :: Value -> Bool -> LLVM ()
setThreadLocal (Value value) l = LLVM $ L.setThreadLocal value (boolToInt l)

isGlobalConstant :: Value -> LLVM Bool
isGlobalConstant (Value value) = LLVM $ intToBool <$> L.isGlobalConstant value

setGlobalConstant :: Value -> Bool -> LLVM ()
setGlobalConstant (Value value) g = LLVM $ L.setGlobalConstant value (boolToInt g)

getFirstGlobal :: Module -> LLVM Value
getFirstGlobal (Module mod) = LLVM $ Value <$> L.getFirstGlobal mod

getLastGlobal :: Module -> LLVM Value
getLastGlobal (Module mod) = LLVM $ Value <$> L.getLastGlobal mod

getNextGlobal :: Value -> LLVM Value
getNextGlobal (Value value) = LLVM $ Value <$> L.getNextGlobal value

getPreviousGlobal :: Value -> LLVM Value
getPreviousGlobal (Value value) = LLVM $ Value <$> L.getPreviousGlobal value

getGlobalParent :: Value -> LLVM Module
getGlobalParent (Value value) = LLVM $ Module <$> L.getGlobalParent value

addFunction :: Module -> String -> Type -> LLVM Value
addFunction (Module mod) name (Type t) = LLVM $ Value <$> withCString name
    (\name' -> L.addFunction mod name' t)

getNamedFunction :: Module -> String -> LLVM Value
getNamedFunction (Module mod) name = LLVM $ Value <$> withCString name (L.getNamedFunction mod)

deleteFunction :: Value -> LLVM ()
deleteFunction (Value value) = LLVM $ L.deleteFunction value

countParams :: Value -> Int
countParams (Value value) = fromIntegral $ L.countParams value

getParams :: Value -> LLVM [Value]
getParams value'@(Value value) = LLVM $ do
    let n = countParams value'
    valueArray <- mallocArray n
    L.getParams value valueArray
    valueList <- peekArray n valueArray
    free valueArray
    return $ map Value valueList

getParam :: Value -> Int -> Value
getParam (Value value) offset = Value $ L.getParam value (fromIntegral offset)

getIntrinsicID :: Value -> Int
getIntrinsicID (Value value) = fromIntegral $ L.getIntrinsicID value

getGC :: Value -> LLVM String
getGC (Value value) = LLVM $ L.getGC value >>= peekCString

setGC :: Value -> String -> LLVM ()
setGC (Value value) gc = LLVM $ withCString gc (L.setGC value)

getFirstFunction :: Module -> LLVM Value
getFirstFunction (Module mod) = LLVM $ Value <$> L.getFirstFunction mod

getLastFunction :: Module -> LLVM Value
getLastFunction (Module mod) = LLVM $ Value <$> L.getLastFunction mod

getNextFunction :: Value -> LLVM Value
getNextFunction (Value value) = LLVM $ Value <$> L.getNextFunction value

getPreviousFunction :: Value -> LLVM Value
getPreviousFunction (Value value) = LLVM $ Value <$> L.getPreviousFunction value

getFirstParam :: Value -> LLVM Value
getFirstParam (Value value) = LLVM $ Value <$> L.getFirstParam value

getLastParam :: Value -> LLVM Value
getLastParam (Value value) = LLVM $ Value <$> L.getLastParam value

getNextParam :: Value -> LLVM Value
getNextParam (Value value) = LLVM $ Value <$> L.getNextParam value

getPreviousParam :: Value -> LLVM Value
getPreviousParam (Value value) = LLVM $ Value <$> L.getPreviousParam value

getParamParent :: Value -> LLVM Value
getParamParent (Value value) = LLVM $ Value <$> L.getParamParent value

isTailCall :: Value -> LLVM Bool
isTailCall (Value value) = LLVM $ intToBool <$> L.isTailCall value

setTailCall :: Value -> Bool -> LLVM ()
setTailCall (Value value) t = LLVM $ L.setTailCall value (boolToInt t)

getFunctionCallConv :: Value -> LLVM CallingConvention
getFunctionCallConv (Value value) = LLVM $ L.toCallingConvention <$> L.getFunctionCallConv value

setFunctionCallConv :: Value -> CallingConvention -> LLVM ()
setFunctionCallConv (Value value) cc = LLVM $ L.setFunctionCallConv value (L.fromCallingConvention cc)

getInstructionCallConv :: Value -> LLVM CallingConvention
getInstructionCallConv (Value value) = LLVM $ L.toCallingConvention <$> L.getInstructionCallConv value

setInstructionCallConv :: Value -> CallingConvention -> LLVM ()
setInstructionCallConv (Value value) cc = LLVM $ L.setInstructionCallConv value (L.fromCallingConvention cc)

-- Constants

constInt :: Type -> Integer -> Bool -> Value
constInt (Type t) i1 i2 = Value $ L.constInt t (fromIntegral i1) (boolToInt i2)

constReal :: Type -> Double -> Value
constReal (Type t) d = Value $ L.constReal t (realToFrac d)

-- I don't feel like doing composite consts right now

-- Constant Expressions

binaryHelper :: (L.ValueRef -> L.ValueRef -> L.ValueRef) -> Value -> Value -> Value
binaryHelper f (Value v1) (Value v2) = Value $ f v1 v2

sizeOf :: Type -> LLVM Value
sizeOf (Type t) = LLVM $ Value <$> L.sizeOf t

constNeg :: Value -> Value
constNeg (Value v) = Value $ L.constNeg v

constNot :: Value -> Value
constNot (Value v) = Value $ L.constNot v

constAdd :: Value -> Value -> Value
constAdd = binaryHelper L.constAdd

constSub :: Value -> Value -> Value
constSub = binaryHelper L.constSub

constMul :: Value -> Value -> Value
constMul = binaryHelper L.constMul

constExactSDiv :: Value -> Value -> LLVM Value
constExactSDiv (Value v1) (Value v2) = LLVM $ Value <$> L.constExactSDiv v1 v2

constFAdd :: Value -> Value -> Value
constFAdd = binaryHelper L.constFAdd

constFMul :: Value -> Value -> Value
constFMul = binaryHelper L.constFMul

constFNeg :: Value -> Value
constFNeg (Value v) = Value $ L.constFNeg v

constFPCast :: Value -> Type -> Value
constFPCast (Value v) (Type t) = Value $ L.constFPCast v t

constFSub :: Value -> Value -> Value
constFSub = binaryHelper L.constFSub

constUDiv :: Value -> Value -> Value
constUDiv = binaryHelper L.constUDiv

constSDiv :: Value -> Value -> Value
constSDiv = binaryHelper L.constSDiv

constFDiv :: Value -> Value -> Value
constFDiv = binaryHelper L.constFDiv

constURem :: Value -> Value -> Value
constURem = binaryHelper L.constURem

constSRem :: Value -> Value -> Value
constSRem = binaryHelper L.constSRem

constFRem :: Value -> Value -> Value
constFRem = binaryHelper L.constFRem

constAnd :: Value -> Value -> Value
constAnd = binaryHelper L.constAnd

constOr :: Value -> Value -> Value
constOr = binaryHelper L.constOr

constXor :: Value -> Value -> Value
constXor = binaryHelper L.constXor

constICmp :: Int -> Value -> Value -> Value
constICmp i (Value v1) (Value v2) = Value $ L.constICmp (fromIntegral i) v1 v2

constFCmp :: Int -> Value -> Value -> Value
constFCmp i (Value v1) (Value v2) = Value $ L.constFCmp (fromIntegral i) v1 v2

constShl :: Value -> Value -> Value
constShl = binaryHelper L.constShl

constLShr :: Value -> Value -> Value
constLShr = binaryHelper L.constLShr

constAShr :: Value -> Value -> Value
constAShr = binaryHelper L.constAShr

-- Again, don't even know what GEP Is.
-- constGEP :: Value

-- Basic Blocks

basicBlockAsValue :: BasicBlock -> Value
basicBlockAsValue (BasicBlock b) = Value $ L.basicBlockAsValue b

valueIsBasicBlock :: Value -> Bool
valueIsBasicBlock (Value v) = L.valueIsBasicBlock v

valueAsBasicBlock :: Value -> BasicBlock
valueAsBasicBlock (Value v) = BasicBlock $ L.valueAsBasicBlock v

countBasicBlocks :: Value -- ^ Function
                    -> LLVM Int
countBasicBlocks (Value v) = LLVM $ fromIntegral <$> L.countBasicBlocks v

getBasicBlocks :: Value -- ^ Function
                  -> LLVM [BasicBlock]
getBasicBlocks f'@(Value f) = do
    n <- countBasicBlocks f'
    LLVM $ do blockArray <- mallocArray n
              L.getBasicBlocks f blockArray
              blockList <- peekArray n blockArray
              free blockArray
              return $ map BasicBlock blockList

getEntryBasicBlock :: Value -- ^ Function
                      -> LLVM BasicBlock
getEntryBasicBlock (Value v) = LLVM $ BasicBlock <$> L.getEntryBasicBlock v

appendBasicBlock :: Value -- ^ Function
                    -> String -- ^ Name for Label
                    -> LLVM BasicBlock
appendBasicBlock (Value v) name =
    LLVM $ BasicBlock <$> withCString name (L.appendBasicBlock v)

insertBasicBlock :: BasicBlock -- ^ Insert before this one
                    -> String
                    -> LLVM BasicBlock
insertBasicBlock (BasicBlock b) name =
    LLVM $ BasicBlock <$> withCString name (L.insertBasicBlock b)

deleteBasicBlock :: BasicBlock -> LLVM ()
deleteBasicBlock (BasicBlock b) = LLVM $ L.deleteBasicBlock b

getFirstBasicBlock :: Value -> LLVM BasicBlock
getFirstBasicBlock (Value v) = LLVM $ BasicBlock <$> L.getFirstBasicBlock v

getLastBasicBlock :: Value -> LLVM BasicBlock
getLastBasicBlock (Value v) = LLVM $ BasicBlock <$> L.getLastBasicBlock v

getNextBasicBlock :: BasicBlock -> LLVM BasicBlock
getNextBasicBlock (BasicBlock b) = LLVM $ BasicBlock <$> L.getNextBasicBlock b

getPreviousBasicBlock :: BasicBlock -> LLVM BasicBlock
getPreviousBasicBlock (BasicBlock b) = LLVM $ BasicBlock <$> L.getPreviousBasicBlock b

getInsertBlock :: Builder -> LLVM BasicBlock
getInsertBlock (Builder b) = LLVM $ BasicBlock <$> L.getInsertBlock b

getBasicBlockParent :: BasicBlock -> LLVM Value
getBasicBlockParent (BasicBlock b) = LLVM $ Value <$> L.getBasicBlockParent b

-- BUILDER TIME! This stuff is actually important...

createBuilder :: LLVM Builder
createBuilder = LLVM $ Builder <$> L.createBuilder

positionBuilder :: Builder -> BasicBlock -> Value -> LLVM ()
positionBuilder (Builder b) (BasicBlock block) (Value v) = LLVM $ L.positionBuilder b block v

positionBefore :: Builder -> Value -> LLVM ()
positionBefore (Builder b) (Value v) = LLVM $ L.positionBefore b v

positionAtEnd :: Builder -> BasicBlock -> LLVM ()
positionAtEnd (Builder b) (BasicBlock block) = LLVM $ L.positionAtEnd b block

getFirstInstruction :: BasicBlock -> LLVM Value
getFirstInstruction (BasicBlock block) = LLVM $ Value <$> L.getFirstInstruction block

getLastInstruction :: BasicBlock -> LLVM Value
getLastInstruction (BasicBlock block) = LLVM $ Value <$> L.getLastInstruction block

getNextInstruction :: Value -> LLVM Value
getNextInstruction (Value v) = LLVM $ Value <$> L.getNextInstruction v

getPreviousInstruction :: Value -> LLVM Value
getPreviousInstruction (Value v) = LLVM $ Value <$> L.getPreviousInstruction v

getInstructionParent :: Value -> LLVM BasicBlock
getInstructionParent (Value v) = LLVM $ BasicBlock <$> L.getInstructionParent v

-- Terminator Instructions
buildRetVoid :: Builder -> LLVM Value
buildRetVoid (Builder b) = LLVM $ Value <$> L.buildRetVoid b

buildRet :: Builder -> Value -> LLVM Value
buildRet (Builder b) (Value v) = LLVM $ Value <$> L.buildRet b v

buildBr :: Builder -> BasicBlock -> LLVM Value
buildBr (Builder builder) (BasicBlock block) = LLVM $ Value <$> L.buildBr builder block

-- Commented out because I don't think its right
--buildIndirectBr :: Builder -> Value -> Int -> LLVM Value
--buildIndirectBr (Builder b) (Value v) i = LLVM $ Value <$> L.buildIndirectBr b v (fromIntegral i)

buildCondBr :: Builder -> Value -> BasicBlock -> BasicBlock -> LLVM Value
buildCondBr (Builder b) (Value v) (BasicBlock b1) (BasicBlock b2) =
    LLVM $ Value <$> L.buildCondBr b v b1 b2

-- Commented out becuase I don't think its right
-- buildSwitch :: Builder -> Value -> BasicBlock -> 

-- Commented out because I haven't implemented yet
-- buildInvoke :: 

buildUnwind :: Builder -> LLVM Value
buildUnwind (Builder b) = LLVM $ Value <$> L.buildUnwind b

buildUnreachable :: Builder -> LLVM Value
buildUnreachable (Builder b) = LLVM $ Value <$> L.buildUnreachable b

-- Arithmetic Ops

arithHelper :: (L.BuilderRef -> L.ValueRef -> L.ValueRef -> CString -> IO L.ValueRef)
                 -> Builder -> Value -> Value -> String -> LLVM Value
arithHelper f (Builder b) (Value v1) (Value v2) s =
    LLVM $ Value <$> withCString s (f b v1 v2)

arithuHelper :: (L.BuilderRef -> L.ValueRef -> CString -> IO L.ValueRef)
                -> Builder -> Value -> String -> LLVM Value
arithuHelper f (Builder b) (Value v) s =
    LLVM $ Value <$> withCString s (f b v)

buildAdd :: Builder -> Value -> Value -> String -> LLVM Value
buildAdd = arithHelper L.buildAdd

buildSub :: Builder -> Value -> Value -> String -> LLVM Value
buildSub = arithHelper L.buildSub

buildMul :: Builder -> Value -> Value -> String -> LLVM Value
buildMul = arithHelper L.buildMul

buildFAdd :: Builder -> Value -> Value -> String -> LLVM Value
buildFAdd = arithHelper L.buildFAdd

buildFMul :: Builder -> Value -> Value -> String -> LLVM Value
buildFMul = arithHelper L.buildFMul

buildFPCast :: Builder -> Value -> Type -> String -> LLVM Value
buildFPCast (Builder b) (Value v) (Type t) s =
    LLVM $ Value <$> withCString s (L.buildFPCast b v t)

buildFSub :: Builder -> Value -> Value -> String -> LLVM Value
buildFSub = arithHelper L.buildFSub

buildUDiv :: Builder -> Value -> Value -> String -> LLVM Value
buildUDiv = arithHelper L.buildUDiv

buildSDiv :: Builder -> Value -> Value -> String -> LLVM Value
buildSDiv = arithHelper L.buildSDiv

buildExactSDiv :: Builder -> Value -> Value -> String -> LLVM Value
buildExactSDiv = arithHelper L.buildExactSDiv

buildFDiv :: Builder -> Value -> Value -> String -> LLVM Value
buildFDiv = arithHelper L.buildFDiv

buildURem :: Builder -> Value -> Value -> String -> LLVM Value
buildURem = arithHelper L.buildURem

buildSRem :: Builder -> Value -> Value -> String -> LLVM Value
buildSRem = arithHelper L.buildSRem

buildFRem :: Builder -> Value -> Value -> String -> LLVM Value
buildFRem = arithHelper L.buildFRem

buildShl :: Builder -> Value -> Value -> String -> LLVM Value
buildShl = arithHelper L.buildShl

buildLShr :: Builder -> Value -> Value -> String -> LLVM Value
buildLShr = arithHelper L.buildLShr

buildAShr :: Builder -> Value -> Value -> String -> LLVM Value
buildAShr = arithHelper L.buildAShr

buildAnd :: Builder -> Value -> Value -> String -> LLVM Value
buildAnd = arithHelper L.buildAnd

buildOr :: Builder -> Value -> Value -> String -> LLVM Value
buildOr = arithHelper L.buildOr

buildXor :: Builder -> Value -> Value -> String -> LLVM Value
buildXor = arithHelper L.buildXor

buildNeg :: Builder -> Value -> String -> LLVM Value
buildNeg = arithuHelper L.buildNeg

buildFNeg :: Builder -> Value -> String -> LLVM Value
buildFNeg = arithuHelper L.buildFNeg

buildNot :: Builder -> Value -> String -> LLVM Value
buildNot = arithuHelper L.buildNot

buildNSWMul :: Builder -> Value -> Value -> String -> LLVM Value
buildNSWMul = arithHelper L.buildNSWMul

buildNSWNeg :: Builder -> Value -> String -> LLVM Value
buildNSWNeg = arithuHelper L.buildNSWNeg

buildNSWSub :: Builder -> Value -> Value -> String -> LLVM Value
buildNSWSub = arithHelper L.buildNSWSub

buildNUWAdd :: Builder -> Value -> Value -> String -> LLVM Value
buildNUWAdd = arithHelper L.buildNUWAdd

buildNUWMul :: Builder -> Value -> Value -> String -> LLVM Value
buildNUWMul = arithHelper L.buildNUWMul

buildNUWNeg :: Builder -> Value -> String -> LLVM Value
buildNUWNeg = arithuHelper L.buildNUWNeg

buildNUWSub :: Builder -> Value -> Value -> String -> LLVM Value
buildNUWSub = arithHelper L.buildNUWSub

-- Memory Allocation Operations

buildMalloc :: Builder -> Type -> String -> LLVM Value
buildMalloc (Builder b) (Type t) name =
    LLVM $ Value <$> withCString name (L.buildMalloc b t)

buildArrayMalloc :: Builder -> Type -> Value -> String -> LLVM Value
buildArrayMalloc (Builder b) (Type t) (Value v) name =
    LLVM $ Value <$> withCString name (L.buildArrayMalloc b t v)

buildAlloca :: Builder -> Type -> String -> LLVM Value
buildAlloca (Builder b) (Type t) name =
    LLVM $ Value <$> withCString name (L.buildAlloca b t)

buildArrayAlloca :: Builder -> Type -> Value -> String -> LLVM Value
buildArrayAlloca (Builder b) (Type t) (Value v) name =
    LLVM $ Value <$> withCString name (L.buildArrayAlloca b t v)

buildFree :: Builder -> Value -> LLVM Value
buildFree (Builder b) (Value v) = LLVM $ Value <$> L.buildFree b v

buildLoad :: Builder -> Value -> String -> LLVM Value
buildLoad (Builder b) (Value v) name =
    LLVM $ Value <$> withCString name (L.buildLoad b v)

buildStore :: Builder -> Value -> Value -> LLVM Value
buildStore (Builder b) (Value v1) (Value v2) = LLVM $ Value <$> L.buildStore b v1 v2

-- I have no idea what buildGEP does

-- Casts
castHelper :: (L.BuilderRef 
               -> L.ValueRef 
               -> L.TypeRef 
               -> CString
               -> IO L.ValueRef)
              -> Builder
              -> Value
              -> Type
              -> String
              -> LLVM Value
castHelper f (Builder b) (Value v) (Type t) s =
    LLVM $ Value <$> withCString s (f b v t)

buildTrunc :: Builder -> Value -> Type -> String -> LLVM Value
buildTrunc = castHelper L.buildTrunc

buildZExt :: Builder -> Value -> Type -> String -> LLVM Value
buildZExt = castHelper L.buildZExt

-- More Casts to do

-- Comparisons

convertIntCmp :: IntComparison -> CInt
convertIntCmp cmp = fromIntegral $ case cmp of
    IntEQ -> 32
    IntNE -> 33
    IntUGT -> 34
    IntUGE -> 36
    IntULT -> 37
    IntULE -> 38
    IntSGT -> 39
    IntSGE -> 40
    IntSLT -> 41
    IntSLE -> 42

convertRealCmp :: RealComparison -> CInt
convertRealCmp cmp = fromIntegral $ case cmp of
    RealPredicateFalse -> 0
    RealOEQ -> 1
    RealOGT -> 2
    RealOGE -> 3
    RealOLT -> 4
    RealOLE -> 5
    RealONE -> 6
    RealORD -> 7
    RealUNO -> 8
    RealUEQ -> 9
    RealUGT -> 10
    RealUGE -> 11
    RealULT -> 12
    RealULE -> 13
    RealUNE -> 14
    RealPredicateTrue -> 15

buildICmp :: Builder -> IntComparison -> Value -> Value -> String -> LLVM Value
buildICmp (Builder b) c (Value v1) (Value v2) name =
    LLVM $ Value <$> withCString name (L.buildICmp b (convertIntCmp c) v1 v2)

buildFCmp :: Builder -> RealComparison -> Value -> Value -> String -> LLVM Value
buildFCmp (Builder b) c (Value v1) (Value v2) name =
    LLVM $ Value <$> withCString name (L.buildFCmp b (convertRealCmp c) v1 v2)

-- Utils

writeBitcodeToFile :: Module -> String -> IO Bool
writeBitcodeToFile (Module m) name = 
    intToBool <$> withCString name (L.writeBitcodeToFile m)
