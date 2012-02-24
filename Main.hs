module Main
where

import Data.Int
import System.IO
import System.Process

import LLVM.Untyped.Core

main :: IO ()
main = do mod' <- runLLVM $ do
            m <- moduleCreateWithName "main"
            ft <- functionType int32Type [int32Type] False
            f <- addFunction m "add" ft
            entryBlock <- appendBasicBlock f ""
            builder <- createBuilder
            positionAtEnd builder entryBlock
            var1 <- buildAlloca builder int32Type "var1"
            var2 <- buildAlloca builder int32Type "var2"
            buildStore builder (constInt int32Type 5 True) var1
            buildStore builder (constInt int32Type 7 True) var2
            val1 <- buildLoad builder var1 "val1"
            val2 <- buildLoad builder var2 "val2"
            addRes <- buildAdd builder val1 val2 "addRes"
            buildRet builder addRes
            return m

          writeBitcodeToFile mod' "out.bc"
          createProcess (shell "llvm-dis out.bc")
          return ()
