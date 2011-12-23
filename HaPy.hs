{-# LANGUAGE ForeignFunctionInterface #-}
module HaPy where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.StablePtr
import Foreign.C.String
import qualified System.Plugins as Plug
import System.Directory
import GHC
import Module
import GHC.Paths (libdir)
import System.FilePath
import Data.List.Split
import qualified Control.Exception as Except

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
    result <- loadSym modName symName
    case result of
        Nothing -> do -- Symbol not found, return error
            return $ castPtrToStablePtr nullPtr 
        Just sym -> do -- Return symbol
            sPtr <- newStablePtr sym
            return $ castToOpaquePtr sPtr

loadSym :: String -> String -> IO (Maybe a)
loadSym modName sym = do
    isLocal <- isLocalMod modName
    if isLocal then
        localLoad modName sym
    else
        packageLoad modName sym

localLoad :: String -> String -> IO (Maybe a)
localLoad modName sym = do
        objectFilePath <- localModuleFilePath modName
        result <- Plug.load objectFilePath [] [] sym
        case result of
            Plug.LoadSuccess _ s -> return $ Just s
            Plug.LoadFailure _ -> return Nothing

packageLoad :: String -> String -> IO (Maybe a)
packageLoad modName sym = do
        -- objectFilePath <- externalModuleObjectFilePath modName
        packageName <- hostPackage modName
        Plug.initLinker
        Plug.loadPackageFunction packageName modName sym

foreign export ccall doesModuleExist :: CString -> IO (Bool)
doesModuleExist modName_c = do
    modName <- peekCString modName_c
    local <- localModuleFilePath modName
    isLocal <- doesFileExist local
    if isLocal then
        return True
    else
        Except.catch
            (hostPackage modName >> return True)
            (\e -> seq (e::Except.SomeException) (return False))

isLocalMod :: String -> IO (Bool)
isLocalMod modName = do
    local <- localModuleFilePath modName
    doesFileExist local

foreign export ccall getInterfaceFilePath :: CString -> IO (CString)
getInterfaceFilePath modName_c = do
    modName <- peekCString modName_c
    objFile <- localModuleFilePath modName
    let hiFile = replaceExtension objFile ".hi"
    isLocal <- doesFileExist hiFile
    if isLocal then
        newCString hiFile
    else 
        newCString ""

localModuleFilePath :: String -> IO String
localModuleFilePath modName = do
    let modulePath = foldr (</>) "" (splitOn "." modName)
    dot <- getCurrentDirectory
    let localPath = dot </> (modulePath ++ ".o")
    return localPath

externalModuleObjectFilePath :: String -> IO String
externalModuleObjectFilePath mod = do
    package <- hostPackage mod
    path <- libPath package
    let objf = path </> ("HS" ++ package <.> "o")
    let hif = path </> (moduleNameSlashes $ mkModuleName mod) <.> "hi"
    let tohif = path </> (takeFileName (replaceExtension objf "hi"))
    -- Now we're going to do something dirty, copy the interface file over
    copyFile hif tohif
    return objf

libPath :: String -> IO FilePath
libPath packageName = do
    home <- getHomeDirectory
    return $ home </> ".cabal" </> "lib" </> packageName </> "ghc-7.0.3"

hostPackage :: String -> IO String
hostPackage mName = runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    _ <- setSessionDynFlags flags -- this looks stupid but is necesary
    mod <- lookupModule (mkModuleName mName) Nothing
    return $ packageIdString $ modulePackageId mod


-- Apply an argument supplied from the C side to the function in the pointer

applyArg :: StablePtr (a -> Opaque) -> a -> IO (StablePtr Opaque)
applyArg sPtr arg = do
    fun <- deRefStablePtr sPtr
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

foreign export ccall applyIntList :: StablePtr ([Int] -> Opaque) ->
                                      Int -> Ptr Int -> IO (StablePtr Opaque)
applyIntList sPtr len arrPtr = do
    array <- peekArray len arrPtr
    applyArg sPtr array

foreign export ccall applyBoolList :: StablePtr ([Bool] -> Opaque) ->
                                      Int -> Ptr Bool -> IO (StablePtr Opaque)
applyBoolList sPtr len arrPtr = do
    array <- peekArray len arrPtr
    applyArg sPtr array

foreign export ccall applyDoubleList :: StablePtr ([Double] -> Opaque) ->
                                      Int -> Ptr Double -> IO (StablePtr Opaque)
applyDoubleList sPtr len arrPtr = do
    array <- peekArray len arrPtr
    applyArg sPtr array

-- Apply an arbitrary Haskell value obtained by a call to getSymbol or 
-- apply*. The difference is that opaque values are not translated between 
-- C and Haskell types and remain opaque pointers to Haskell heap objects.
foreign export ccall applyOpaque :: StablePtr (Opaque -> Opaque) ->
                                      StablePtr Opaque -> IO (StablePtr Opaque)
applyOpaque funPtr argPtr = do
    arg <- deRefStablePtr argPtr
    applyArg funPtr arg


-- Pull out the final result from the ptr

retrieveResult :: StablePtr a -> IO(a)
retrieveResult sPtr = do
    res <- deRefStablePtr sPtr
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
    newCString str -- TODO: free this later

foreign export ccall retrieveListLength :: StablePtr [a] -> IO (Int)
retrieveListLength sPtr = do
    lst <- retrieveResult sPtr
    return (length lst)

foreign export ccall retrieveIntList :: StablePtr [Int] -> IO (Ptr Int)
retrieveIntList sPtr = do
    lst <- retrieveResult sPtr
    newArray lst

foreign export ccall retrieveBoolList :: StablePtr [Bool] -> IO (Ptr Bool)
retrieveBoolList sPtr = do
    lst <- retrieveResult sPtr
    newArray lst

foreign export ccall retrieveDoubleList :: StablePtr [Double] -> IO (Ptr Double)
retrieveDoubleList sPtr = do
    lst <- retrieveResult sPtr
    newArray lst

foreign export ccall retrieveOpaqueList :: StablePtr [a] -> IO (Ptr (StablePtr Opaque))
retrieveOpaqueList sPtr = do
    lst <- retrieveResult sPtr
    wrappedLst <- mapM newStablePtr lst
    newArray $ map castToOpaquePtr wrappedLst -- TODO: free this later

-- cleanup
foreign export ccall freeArray :: Ptr a -> IO ()
freeArray arr = do
    free arr
    return ()

foreign export ccall freePtr :: StablePtr a -> IO ()
freePtr sPtr = do
    _ <- freeStablePtr sPtr
    return ()

