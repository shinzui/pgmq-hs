{-# LANGUAGE OverloadedStrings #-}

-- | Hedgehog generators for pgmq types
module Generators
  ( genMessageBody,
    genMessageHeaders,
    genJsonValue,
    genJsonObject,
    genJsonScalar,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (fromFloatDigits)
import Data.Vector qualified as V
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pgmq.Types (MessageBody (..), MessageHeaders (..))

-- | Generate a random MessageBody containing a JSON object
genMessageBody :: Gen MessageBody
genMessageBody = MessageBody <$> genJsonObject

-- | Generate random MessageHeaders containing a JSON object
genMessageHeaders :: Gen MessageHeaders
genMessageHeaders = MessageHeaders <$> genJsonObject

-- | Generate a JSON object with random keys and values
genJsonObject :: Gen Value
genJsonObject = do
  numFields <- Gen.int (Range.linear 1 10)
  fields <- Gen.list (Range.singleton numFields) genField
  pure $ Object (KeyMap.fromList fields)
  where
    genField = do
      key <- genFieldName
      value <- genJsonValue
      pure (key, value)

    genFieldName = do
      len <- Gen.int (Range.linear 1 20)
      name <- Gen.text (Range.singleton len) Gen.alphaNum
      pure $ Key.fromText name

-- | Generate arbitrary JSON values (recursively)
genJsonValue :: Gen Value
genJsonValue =
  Gen.recursive
    Gen.choice
    -- Non-recursive cases
    [genJsonScalar]
    -- Recursive cases (less frequent due to shrinking)
    [ genJsonArray,
      genJsonObjectValue
    ]

-- | Generate scalar JSON values (non-recursive)
genJsonScalar :: Gen Value
genJsonScalar =
  Gen.choice
    [ genNull,
      genBool,
      genNumber,
      genString
    ]

genNull :: Gen Value
genNull = pure Null

genBool :: Gen Value
genBool = Bool <$> Gen.bool

genNumber :: Gen Value
genNumber =
  Gen.choice
    [ -- Integer-like numbers
      Number . fromIntegral <$> Gen.int (Range.linearFrom 0 (-1000000) 1000000),
      -- Floating point numbers
      Number . fromFloatDigits <$> Gen.double (Range.linearFracFrom 0 (-1000000) 1000000)
    ]

genString :: Gen Value
genString = do
  len <- Gen.int (Range.linear 0 100)
  txt <- Gen.text (Range.singleton len) Gen.unicode
  pure $ String txt

genJsonArray :: Gen Value
genJsonArray = do
  len <- Gen.int (Range.linear 0 5)
  elements <- Gen.list (Range.singleton len) genJsonScalar
  pure $ Array (V.fromList elements)

genJsonObjectValue :: Gen Value
genJsonObjectValue = do
  numFields <- Gen.int (Range.linear 1 5)
  fields <- Gen.list (Range.singleton numFields) genObjectField
  pure $ Object (KeyMap.fromList fields)
  where
    genObjectField = do
      key <- genKey
      value <- genJsonScalar
      pure (key, value)

    genKey = do
      len <- Gen.int (Range.linear 1 20)
      txt <- Gen.text (Range.singleton len) Gen.alphaNum
      pure $ Key.fromText txt
