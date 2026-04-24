{-# LANGUAGE OverloadedStrings #-}

module ClassificationSpec (tests) where

import Data.HashSet qualified as HashSet
import Hasql.Errors qualified as HasqlErrors
import Pgmq.Effectful
  ( PgmqRuntimeError (..),
    isTransient,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "isTransient classification"
    [ testCase "acquisition timeout is transient" $
        assertBool "expected transient" (isTransient PgmqAcquisitionTimeout),
      testCase "networking connection error is transient" $
        assertBool
          "expected transient"
          ( isTransient
              (PgmqConnectionError (HasqlErrors.NetworkingConnectionError "refused"))
          ),
      testCase "authentication error is not transient" $
        assertBool
          "expected permanent"
          ( not $
              isTransient
                (PgmqConnectionError (HasqlErrors.AuthenticationConnectionError "bad password"))
          ),
      testCase "compatibility error is not transient" $
        assertBool
          "expected permanent"
          ( not $
              isTransient
                (PgmqConnectionError (HasqlErrors.CompatibilityConnectionError "version mismatch"))
          ),
      testCase "other connection error is treated as transient" $
        assertBool
          "expected transient"
          ( isTransient
              (PgmqConnectionError (HasqlErrors.OtherConnectionError "libpq says no"))
          ),
      testCase "connection-drop session error is transient" $
        assertBool
          "expected transient"
          ( isTransient
              (PgmqSessionError (HasqlErrors.ConnectionSessionError "dropped"))
          ),
      testCase "driver session error is not transient" $
        assertBool
          "expected permanent"
          ( not $
              isTransient
                (PgmqSessionError (HasqlErrors.DriverSessionError "bug"))
          ),
      testCase "missing-types session error is not transient" $
        assertBool
          "expected permanent"
          ( not $
              isTransient
                (PgmqSessionError (HasqlErrors.MissingTypesSessionError HashSet.empty))
          )
    ]
