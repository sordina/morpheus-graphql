{-# LANGUAGE DataKinds #-}
-- | GQL Types
module Data.Morpheus.Types
  ( resolver
  -- Resolver Monad
  , Resolver
  , SubRootRes
  , Event(..)
  -- Type Classes
  , GQLType(KIND, description)
  , GQLScalar(parseValue, serialize)
  -- Values
  , GQLRequest(..)
  , GQLResponse(..)
  , ID(..)
  , ScalarValue(..)
  , GQLRootResolver(..)
  , constRes
  , Undefined(..)
  , SubResolver
  , MutResolver
  , GADTResolver(..)
  , GraphQLT(..)
  , OperationKind(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  ) where

import           Data.Morpheus.Types.GQLScalar         (GQLScalar (parseValue, serialize))
import           Data.Morpheus.Types.GQLType           (GQLType (KIND, description))
import           Data.Morpheus.Types.ID                (ID (..))
import           Data.Morpheus.Types.Internal.Data     (OperationKind (..))
import           Data.Morpheus.Types.Internal.Resolver (Event (..), GADTResolver (..), GQLRootResolver (..),
                                                        GraphQLT (..), MutResolver, Resolver, SubResolver, SubRootRes,
                                                        resolver)
import           Data.Morpheus.Types.Internal.Value    (ScalarValue (..))
import           Data.Morpheus.Types.IO                (GQLRequest (..), GQLResponse (..))
import           Data.Morpheus.Types.Types             (Undefined (..))
-- resolves constant value on any argument
constRes :: Monad m => b -> a -> m b
constRes = const . return

type QUERY = 'Query
type MUTATION = 'Mutation
type SUBSCRIPTION = 'Subscription
