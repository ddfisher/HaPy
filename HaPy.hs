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

foreign export ccall getSymbol :: CString -> CString -> IO (StablePtr Opaque)
getSymbol modName_c symName_c = do
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

foreign export ccall applyInt :: StablePtr (Int -> Opaque) -> Int ->
                                   IO (StablePtr Opaque)
applyInt = applyArg

foreign export ccall applyBool :: StablePtr (Bool -> Opaque) -> Bool ->
                                    IO (StablePtr Opaque)
applyBool = applyArg

foreign export ccall applyDouble :: StablePtr (Double -> Opaque) ->
                                      Double -> IO (StablePtr Opaque)
applyDouble = applyArg

foreign export ccall applyString :: StablePtr (String -> Opaque) ->
                                      CString -> IO (StablePtr Opaque)
applyString sPtr cString = do
    str <- peekCString cString 
    res <- applyArg sPtr str
    return res

-- Apply an arbitrary Haskell value obtained by a call to getSymbol or 
-- apply*. The difference is that opaque values are not translated between 
-- C and Haskell types and remain opaque pointers to Haskell heap objects.
foreign export ccall applyOpaque :: StablePtr (Opaque -> Opaque) ->
                                      StablePtr Opaque -> IO (StablePtr Opaque)
applyOpaque funPtr argPtr = do
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

foreign export ccall retrieveInt :: StablePtr Int -> IO (Int)
retrieveInt = retrieveResult

foreign export ccall retrieveDouble :: StablePtr Double -> IO (Double)
retrieveDouble = retrieveResult

foreign export ccall retrieveBool :: StablePtr Bool -> IO (Bool)
retrieveBool = retrieveResult

foreign export ccall retrieveString :: StablePtr String -> IO (CString)
retrieveString sPtr = do
    str <- retrieveResult sPtr
    newCString str