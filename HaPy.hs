{-# LANGUAGE ForeignFunctionInterface #-}
module HaskellModule where

import Foreign
import Foreign.StablePtr
import Foreign.C.String

-- Placeholder type used where we actually throw away the type info
data Irrelevant 

castToIrrelevantPtr :: StablePtr a -> StablePtr Irrelevant
castToIrrelevantPtr = castPtrToStablePtr . castStablePtrToPtr

exampleFunction = (+)

foreign export ccall getSymbol_c :: CString -> IO (StablePtr Irrelevant)
getSymbol_c _ = do
    fun <- newStablePtr exampleFunction
    return $ castToIrrelevantPtr fun

-- Apply an argument supplied from the C side to the function in the pointer

applyArg :: StablePtr (a -> Irrelevant) -> a -> IO (StablePtr Irrelevant)
applyArg sPtr arg = do
    fun <- deRefStablePtr sPtr
    _ <- freeStablePtr sPtr
    res <- return (fun arg)
    newStablePtr res

-- export applyArg for each supported type

foreign export ccall applyInt_c :: StablePtr (Int -> Irrelevant) -> Int -> IO (StablePtr Irrelevant)
applyInt_c = applyArg

foreign export ccall applyBool_c :: StablePtr (Bool -> Irrelevant) -> Bool -> IO (StablePtr Irrelevant)
applyBool_c = applyArg

foreign export ccall applyDouble_c :: StablePtr (Double -> Irrelevant) -> Double -> IO (StablePtr Irrelevant)
applyDouble_c = applyArg

foreign export ccall applyString_c :: StablePtr (String -> Irrelevant) -> CString -> IO(StablePtr Irrelevant)
applyString_c sPtr cString = do
    str <- peekCString cString 
    res <- applyArg sPtr str
    return res

-- Pull out the final result from the ptr

retrieveResult :: StablePtr a -> IO(a)
retrieveResult sPtr = do
    res <- deRefStablePtr sPtr
    _ <- freeStablePtr sPtr
    return res

-- export for each supported type

foreign export ccall retrieveInt_c :: StablePtr Int -> IO (Int)
retrieveInt_c = retrieveResult

foreign export ccall retrieveDouble_c :: StablePtr Double -> IO (Double)
retrieveDouble_c = retrieveResult

foreign export ccall retrieveBool_c :: StablePtr Bool -> IO (Bool)
retrieveBool_c = retrieveResult

foreign export ccall retrieveString_c :: StablePtr String -> IO (CString)
retrieveString_c sPtr = do
    str <- retrieveResult sPtr
    newCString str

{-
import System.Plugins

main = do
    _ <- initLinker
    mod <- loadModule "./Mod.o"
    _ <- resolveObjs (return mod)
    sym_q <- loadFunction mod "one"
    --sym_q <- loadPackageFunction "random-1.0.0.3" "System.Random" "mkStdGen"
    case sym_q of
        Just sym -> print "just fun!"
        Nothing -> print "couldn't load"
-}
