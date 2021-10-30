{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Devtools.Parser where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Maybe (fromMaybe)
import System.FilePath as FilePath
import qualified Data.Text.Lazy.IO as LazyIO

import Devtools.Schema

import GHC.SourceGen
import GHC.Paths (libdir)
import GhcMonad (getSessionDynFlags)
import GHC (runGhc, DynFlags)

import Data.String ( IsString(fromString) )

data GenModule = GenModule 
    { _modName :: Text
    , _modDesc :: Maybe Text
    , _modDeprecated :: Bool
    , _modImports :: [Text]
    , _modCustomTypes :: [GenCustomType]
    , _modCommands :: [GenCommand]
    } deriving (Show)

data TypeRef = TypeBoolean
             | TypeInteger
             | TypeString
             | TypeDouble
             | TypeAny
             | TypeObject
             | TypeRefName Text
             | Optional TypeRef
             | Array TypeRef
    deriving (Show)

data CustomType = Wrapper TypeRef
                | CustomObject [GenParameter]
                | CustomEnum [Text]
    deriving (Show)

data GenCustomType = GenCustomType
    { _customTypeName :: Text
    , _customTypeDesc :: Maybe Text
    , _customTypeType :: CustomType
    } deriving (Show)

data GenCommand = GenCommand
    { _commandName :: Text
    , _commandDesc :: Maybe Text
    , _commandExperimental :: Bool
    , _commandParameters :: [GenParameter]
    , _commandReturns :: [GenParameter]
    } deriving (Show)

data GenParameter = GenParameter
    { _parameterName :: Text
    , _parameterDesc :: Maybe Text
    , _parameterType :: TypeRef
    } deriving (Show)

parseParameter :: [unwrap| DevtoolsSchema.domains[].commands[].parameters?[] |] -> GenParameter
parseParameter p = GenParameter 
    { _parameterName = [get| p.name |]
    , _parameterDesc = [get| p.description |]
    , _parameterType = parseTypeRef [get| p.$ref |] [get| p.type |] [get| p.items?.($ref,type) |]
    }
    where parseTypeRef :: Maybe Text -> Maybe Text -> Maybe (Maybe Text, Maybe Text) -> TypeRef
          parseTypeRef (Just t) _ _ = TypeRefName t
          parseTypeRef _ (Just "array") (Just (tr, tt)) = Array (parseTypeRef tr tt Nothing)
          parseTypeRef _ (Just "boolean") _ = TypeBoolean
          parseTypeRef _ (Just "integer") _ = TypeInteger
          parseTypeRef _ (Just "string") _ = TypeString
          parseTypeRef _ (Just "number") _ = TypeDouble
          parseTypeRef _ (Just "any") _ = TypeAny
          parseTypeRef _ (Just "object") _ = TypeObject
          parseTypeRef x y z = error ("Invalid Type: " ++ show x ++ " " ++ show y ++ " " ++ show z)

parseCustomType :: [unwrap| DevtoolsSchema.domains[].types?[] |] -> GenCustomType
parseCustomType t = GenCustomType
    { _customTypeName = [get| t.id |]
    , _customTypeDesc = [get| t.description |]
    , _customTypeType = parseTypeDec [get| t.type |] Nothing {- [get| t.items |] -} [get| t.enum |] (fmap (fmap parseParameter) [get| t.properties |])
    }
    where parseTypeDec :: Text -> Maybe TypeRef -> Maybe [Text] -> Maybe [GenParameter] -> CustomType
          parseTypeDec "boolean" Nothing Nothing Nothing = Wrapper TypeBoolean
          parseTypeDec "integer" Nothing Nothing Nothing = Wrapper TypeInteger
          parseTypeDec "string" Nothing Nothing Nothing = Wrapper TypeString
          parseTypeDec "number" Nothing Nothing Nothing = Wrapper TypeDouble
          parseTypeDec "array" (Just tr) Nothing Nothing = Wrapper (Array tr)
          parseTypeDec "string" Nothing (Just vals) Nothing = CustomEnum vals
          parseTypeDec "object" Nothing Nothing (Just props) = CustomObject props
          parseTypeDec x y z q = error ("Unable to parse Type Dec " ++ show x ++ " " ++ show y ++ " " ++ show z)

g :: IO [GenModule]
g = do
    obj <- either fail return =<< eitherDecodeFileStrict "C:\\Users\\micah\\Source\\devtools-protocol\\json\\browser_protocol.json" :: IO (Object DevtoolsSchema)
    return $ flip fmap [get| obj.domains[] |] $ \d -> 
        GenModule 
        { _modName = [get| d.domain |] 
        , _modDesc = [get| d.description |]
        , _modDeprecated = fromMaybe False [get| d.deprecated |]
        , _modImports = fromMaybe [] [get| d.dependencies?[] |]
        , _modCustomTypes = maybe [] (fmap parseCustomType) [get| d.types |]
        , _modCommands = flip fmap [get| d.commands[] |] $ \c -> 
            GenCommand
            { _commandName = [get| c.name |]
            , _commandDesc = [get| c.description |]
            , _commandExperimental = fromMaybe False [get| c.experimental |]
            , _commandParameters = maybe [] (fmap parseParameter) [get| c.parameters |]
            , _commandReturns = maybe [] (fmap parseParameter) [get| c.returns |]
            }
        }

intercalate :: Builder -> [Builder] -> Builder
intercalate sep [] = "" 
intercalate sep items = foldl1 (\l r -> l <> sep <> r) items

writeComment :: Text -> Builder
writeComment t = foldr (<>) "" $ (\descLine -> "-- | " <> Builder.fromText descLine <> "\n") <$> Text.splitOn "\n" t

writeTypeRef :: TypeRef -> Builder
writeTypeRef TypeBoolean = "Boolean"
writeTypeRef TypeInteger = "Int"
writeTypeRef TypeString = "Text"
writeTypeRef TypeDouble = "Double"
writeTypeRef TypeAny = "AnyType"
writeTypeRef TypeObject = "Value"
writeTypeRef (TypeRefName t) = Builder.fromText t
writeTypeRef (Optional t) = "Maybe (" <> writeTypeRef t <> ")"
writeTypeRef (Array t) = "[" <> writeTypeRef t <> "]"

writeCommand :: GenCommand -> Builder
writeCommand com = descComment <> typeDef <> "\n"
    where descComment = maybe "" writeComment (_commandDesc com)
          ret = "(" <> (intercalate ", " . fmap (writeTypeRef . _parameterType) . _commandReturns $ com) <> ")"
          args = intercalate " -> " . (++ [ret]) . fmap (writeTypeRef . _parameterType) . _commandParameters $ com
          typeDef = Builder.fromText (_commandName com) <> " :: " <> args

writeCustomType :: GenCustomType -> Builder
writeCustomType t = descComment <> decl
    where descComment = maybe "" writeComment (_customTypeDesc t)
          decl = case (_customTypeType t) of
                    Wrapper typeRef -> "newtype " <> Builder.fromText (_customTypeName t) <> " = "
                    _ -> "data " <> Builder.fromText (_customTypeName t)

writeModule :: GenModule -> Data.Text.Lazy.Text
writeModule mod = Builder.toLazyText (moduleComment <> moduleName <> "\n\n" <> imports <> "\n" <> customTypes <> "\n\n" <> commands)
    where moduleComment = maybe "" (\desc -> foldr (<>) "" $ (\descLine -> "-- | " <> Builder.fromText descLine <> "\n") <$> Text.splitOn "\n" desc) (_modDesc mod)
          moduleDeprecated = if _modDeprecated mod then " {-# DEPRECATED \"This module has been deprecated\" #-}" else ""
          moduleName = "module " <>  Builder.fromText (_modName mod) <> moduleDeprecated <> " where"
          imports = foldr (<>) "" $ fmap ((\i -> "import qualified " <> i <> "\n") . Builder.fromText) (_modImports mod)
          customTypes = intercalate "\n\n" $ fmap writeCustomType (_modCustomTypes mod)
          commands = intercalate "\n" $ fmap writeCommand (_modCommands mod)

writeModuleFile :: FilePath -> GenModule -> IO ()
writeModuleFile root mod = do
    let filePath = root FilePath.</> Text.unpack (_modName mod <> ".hs")
    LazyIO.writeFile filePath $ writeModule mod

writeSafeModule :: GenModule -> HsModule'
writeSafeModule mod = mod'''
    where imports = fmap (qualified' . import' . fromString . Text.unpack) (_modImports mod)
          mod' = module' (Just . fromString . Text.unpack . _modName $ mod) Nothing imports []
          mod'' = if _modDeprecated mod then withModuleDeprecated (Just "This module is deprecated") mod' else mod'
          mod''' = withModuleHaddock (Just "\"\n-- | This is a true comment\n\"" {- fmap Text.unpack . _modDesc $ mod -}) mod''

writeTestModule :: FilePath -> GenModule -> IO ()
writeTestModule root mod = do
    let filePath = root FilePath.</> Text.unpack (_modName mod <> ".hs")
    string <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        return $ showPpr dflags (writeSafeModule mod)
    LazyIO.writeFile filePath (Data.Text.Lazy.pack string)

h :: IO ()
h = do
    mods <- g
    mapM_ (writeTestModule "C:\\Users\\micah\\Source\\monitor\\output") mods