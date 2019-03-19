{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Arguments
  ( requiredArgument
  , unknownArguments
  , unsupportedArgumentType
  , argumentError
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLError (..), GQLErrors, MetaError (..))
import           Data.Morpheus.Types.MetaInfo (MetaInfo (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T (concat)

{-
  ARGUMENTS:
    type Experience {
        experience ( lang: LANGUAGE ) : String ,
        date: String
    }

  - required field !?
  - experience( lang: "bal" ) -> "Expected type LANGUAGE, found \"a\"."
  - experience( lang: Bla ) -> "Expected type LANGUAGE, found Bla."
  - experience( lang: 1 ) -> "Expected type LANGUAGE, found 1."
  - experience( a1 : 1 ) -> "Unknown argument \"a1\" on field \"experience\" of type \"Experience\".",
  - date(name: "name") -> "Unknown argument \"name\" on field \"date\" of type \"Experience\"."
-}
argumentError :: MetaError -> GQLErrors
argumentError (UnknownType meta) = unsupportedArgumentType meta
argumentError (UnknownField meta) = unsupportedArgumentType meta
argumentError (TypeMismatch meta _ _) = unsupportedArgumentType meta

unsupportedArgumentType :: MetaInfo -> GQLErrors
unsupportedArgumentType meta = errorMessage (position meta) text
  where
    text = T.concat ["Argument \"", key meta, "\" has unsuported type \"", typeName meta, "\"."]

unknownArguments :: Text -> [Text] -> GQLErrors
unknownArguments fieldName = map keyToError
  where
    keyToError x = GQLError {desc = toMessage x, posIndex = 0}
    toMessage argName = T.concat ["Unknown Argument \"", argName, "\" on Field \"", fieldName, "\"."]

requiredArgument :: MetaInfo -> GQLErrors
requiredArgument meta = errorMessage (position meta) text
  where
    text = T.concat ["Required Argument: \"", key meta, "\" not Found on type \"", typeName meta, "\"."]