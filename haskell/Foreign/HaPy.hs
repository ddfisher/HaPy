module Foreign.HaPy (
    pythonExport,
    module Foreign.C
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift
import Foreign.C (CInt(..), CDouble(..))
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad

data FType = FBool | FInt | FDouble | FString | FUnknown
  deriving (Enum, Show)

deriveLift ''FType

-- Can't use e.g. ''Bool when pattern matching
fromHaskellType :: Type -> FType
fromHaskellType (ConT nm) | nm == ''Bool   = FBool
                          | nm == ''Int    = FInt
                          | nm == ''Double = FDouble
                          | nm == ''String = FString
                          | otherwise      = FUnknown

toForeignType :: FType -> Type
toForeignType FBool   = ConT ''Bool
toForeignType FInt    = ConT ''CInt
toForeignType FDouble = ConT ''CDouble
toForeignType FString = error "string type conversion not yet supported!" -- XXX
toForeignType _       = error "unknown type - cannot convert!" -- XXX


-- Converts the type of a function to a list of the type of it's args/return value
toTypeList :: Type -> [Type]
toTypeList (AppT (AppT ArrowT t) ts) = t : toTypeList ts
toTypeList t                         = [t]

fromTypeList :: [Type] -> Type
fromTypeList []     = error "type list empty!"
fromTypeList (t:[]) = t
fromTypeList (t:ts) = (AppT (AppT ArrowT t) (fromTypeList ts))


convertExp :: FType -> ExpQ -> ExpQ
convertExp FBool   exp = exp 
convertExp FInt    exp = [| fromIntegral $exp |]
convertExp FDouble exp = [| realToFrac $exp |]
convertExp FString exp = fail "string conversion not yet supported"
convertExp _       exp = fail "conversion failed: unknown type!"


makeFunction :: (String -> String) -> (Name -> [FType] -> ClauseQ) -> ([FType] -> Type) -> Name -> DecsQ
makeFunction changeName makeClause makeType origName = do
  VarI _ t _ _ <- reify origName
  let types = map fromHaskellType $ toTypeList t
      name = mkName . changeName . nameBase $  origName
      cl   = makeClause origName types
      func = funD name [cl]

      typ  = makeType types
      dec = return $ ForeignD $ ExportF CCall (nameBase name) name typ
  sequence [func, dec]

makeInfoFunction :: Name -> DecsQ
makeInfoFunction name = [t| Ptr CInt -> IO () |] >>= \t -> makeFunction makeName makeClause (const t) name
    where makeName = (++ "__info")
          makeClause _ types = let body = normalB $ [| \arr -> pokeArray0 (- 1::CInt) arr $ map (fromIntegral . fromEnum) $(lift types) |] in
                                clause [] body []


makeExportFunction :: Name -> DecsQ
makeExportFunction = makeFunction makeName makeClause makeType
    where makeName = (++ "__export")
          makeType = fromTypeList . map toForeignType
          makeClause nm types = do
              vars <- replicateM (length types - 1) (newName "x")
              let args = map varP vars
                  convertedArgs = zipWith convertExp types (map varE vars)
                  body = normalB $ convertExp (last types) $ foldl appE (varE nm) convertedArgs
              clause args body []

pythonExport :: Name -> DecsQ
pythonExport nm = do
  info <- makeInfoFunction nm
  export <- makeExportFunction nm
  return $ info ++ export
