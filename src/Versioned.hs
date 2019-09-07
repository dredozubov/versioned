{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Versioned where

import Data.Foldable as F
import Data.Kind
import Data.Map.Strict as M
import Data.Maybe
import Data.Set as S
import Data.Text as T


class (Ord (Hash f r), Show (Hash f r), Eq (Hash f r)) => Schema f r | f -> r where
  type Hash f r :: Type
  type Source f r :: Type
  hash :: r -> Hash f r
  hashFromSource :: Source f r -> Maybe (Hash f r)
  reprFromSource :: Hash f r -> Source f r -> Either Text r

data J = J
data R = R Int
newtype H = H Int deriving (Num, Eq, Show, Ord)

instance Schema J R where
  type Hash J R = H
  type Source J R = Text
  hash _ = 42
  hashFromSource _ = Just 3123
  -- reprFromSource = J

data SchemaSet f r where
  SchemaSet
    :: Schema f r
    => r        -- ^ current version
    -> [Arr f r]  -- ^ arrows
    -> SchemaSet f r

type VerticeInfo f r = M.Map (Hash f r) (S.Set (Hash f r))

verticesInfo
  :: forall f r
  . (Schema f r, Ord (Hash f r))
  => [Arr f r]
  -> VerticeInfo f r -- Map from schema to inputs and outputs, FIXME: don't care about inputs
verticesInfo = F.foldl' go M.empty
  where
    go m (Arr f t) =
      let
        fh = hash @f f
        th = hash @f t
        updateTh h Nothing  = Just (S.singleton h)
        updateTh h (Just o) = Just (S.insert h o)
      in M.alter (updateTh fh) th m

mkSchemaSet
  :: forall f r
  . Schema f r
  => r
  -> [Arr f r]
  -> Either Text (SchemaSet f r)
mkSchemaSet repr arrows =
  let
    mError :: Maybe Text
    mError = if S.null co && outErr == Nothing
      then Nothing
      else
        (\x -> T.pack
          $ "schema with hash " ++ show x ++ "has incorrent number of outgoing migrations")
        <$> outErr
      where
        outErr = F.find ((==1) . F.length) (M.delete hc vi)
        vi     = verticesInfo arrows
        hc     = hash @f repr
        -- if it errors, then verticesInfo is faulty
        co     = fromJust $ M.lookup hc vi
  in if Nothing == mError
  then Right $ SchemaSet repr arrows
  else Left $ fromJust mError

data Arr f r where
  Arr :: Schema f r => r -> r -> Arr f r

(~>) = Arr

ss =
  let
    r1 = R 1
    r2 = R 2
    r3 = R 3
    r4 = R 4
  in SchemaSet @J r4 [r1 ~> r2, r3 ~> r2, r2 ~> r4]

-- TODO: ensure reachability to SchemaSet "Current" element
