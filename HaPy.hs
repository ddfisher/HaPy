{-# LANGUAGE ForeignFunctionInterface #-}
module HaPy where

import Foreign.StablePtr
import Foreign.Ptr
import Foreign.C.String
import System.Plugins

-- Placeholder type used where the actual type is hidden or unknown
data Opaque 

castToOpaquePtr :: StablePtr a -> StablePtr Opaque
castToOpaquePtr = castPtrToStablePtr . castStablePtrToPtr


-- Dynamically load the given Haskell module and lookup the given sybmol
-- returns an opaque pointer that can be forced to one of the primitive
-- types or can be treated as a function to which args may be applied

foreign export ccall getSymbol_c :: CString -> CString -> IO (StablePtr Opaque)
getSymbol_c modName_c symName_c = do
    modName <- peekCString modName_c
    symName <- peekCString symName_c
    status <- load modName [] [] symName
    case status of
        LoadFailure msg -> do -- Symbol not found, return error
            return $ castPtrToStablePtr nullPtr 
        LoadSuccess _ sym -> do -- Return symbol
            sPtr <- newStablePtr sym
            return $ castToOpaquePtr sPtr


-- Apply an argument supplied from the C side to the function in the pointer

applyArg :: StablePtr (a -> Opaque) -> a -> IO (StablePtr Opaque)
applyArg sPtr arg = do
    fun <- deRefStablePtr sPtr
    _ <- freeStablePtr sPtr
    res <- return (fun arg)
    newStablePtr res

-- export applyArg for each supported type

foreign export ccall applyInt_c :: StablePtr (Int -> Opaque) -> Int ->
                                   IO (StablePtr Opaque)
applyInt_c = applyArg

foreign export ccall applyBool_c :: StablePtr (Bool -> Opaque) -> Bool ->
                                    IO (StablePtr Opaque)
applyBool_c = applyArg

foreign export ccall applyDouble_c :: StablePtr (Double -> Opaque) ->
                                      Double -> IO (StablePtr Opaque)
applyDouble_c = applyArg

foreign export ccall applyString_c :: StablePtr (String -> Opaque) ->
                                      CString -> IO (StablePtr Opaque)
applyString_c sPtr cString = do
    str <- peekCString cString 
    res <- applyArg sPtr str
    return res

-- Apply an arbitrary Haskell value obtained by a call to getSymbol_c or 
-- apply*_c. The difference is that opaque values are not translated between 
-- C and Haskell types and remain opaque pointers to Haskell heap objects.
foreign export ccall applyOpaque_c :: StablePtr (Opaque -> Opaque) ->
                                      StablePtr Opaque -> IO (StablePtr Opaque)
applyOpaque_c funPtr argPtr = do
    arg <- deRefStablePtr argPtr
    _ <- freeStablePtr argPtr
    applyArg funPtr arg


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
