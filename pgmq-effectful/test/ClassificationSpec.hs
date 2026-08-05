{-# LANGUAGE OverloadedStrings #-}

module ClassificationSpec (tests) where

import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Hasql.Errors qualified as HasqlErrors
import Pgmq.Effectful
  ( PgmqRuntimeError (..),
    isTransient,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

-- | A server-reported statement error with the given SQLSTATE, wrapped the way
-- it actually arrives at 'isTransient': inside 'StatementSessionError'
-- (statement count, index, SQL, params, prepared flag) around
-- 'ServerStatementError' around 'ServerError', whose first field is the
-- five-character SQLSTATE.
serverStatementError :: Text -> PgmqRuntimeError
serverStatementError code =
  PgmqSessionError
    ( HasqlErrors.StatementSessionError
        1
        0
        "select 1"
        []
        True
        (HasqlErrors.ServerStatementError (HasqlErrors.ServerError code "boom" Nothing Nothing Nothing))
    )

-- | SQLSTATEs that retries exist for: they arrive as server errors inside
-- 'StatementSessionError' and must classify transient.
transientStates :: [(Text, String)]
transientStates =
  [ ("40001", "serialization_failure"),
    ("40P01", "deadlock_detected"),
    ("55P03", "lock_not_available"),
    ("57P01", "admin_shutdown"),
    ("57P02", "crash_shutdown"),
    ("57P03", "cannot_connect_now"),
    ("53100", "disk_full"),
    ("53200", "out_of_memory"),
    ("53300", "too_many_connections")
  ]

-- | Genuine statement bugs must stay permanent: retrying cannot fix them.
permanentStates :: [(Text, String)]
permanentStates =
  [ ("23505", "unique_violation"),
    ("42P01", "undefined_table"),
    ("22P02", "invalid_text_representation")
  ]

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
          ),
      testGroup
        "server-reported SQLSTATEs (PGH-10)"
        ( [ testCase (show code <> " " <> name <> " is transient") $
              assertBool "expected transient" (isTransient (serverStatementError code))
          | (code, name) <- transientStates
          ]
            <> [ testCase (show code <> " " <> name <> " is permanent") $
                   assertBool "expected permanent" (not (isTransient (serverStatementError code)))
               | (code, name) <- permanentStates
               ]
        ),
      testCase "row-count decode failure is not transient" $
        assertBool
          "expected permanent"
          ( not $
              isTransient
                ( PgmqSessionError
                    ( HasqlErrors.StatementSessionError
                        1
                        0
                        "select 1"
                        []
                        True
                        (HasqlErrors.UnexpectedRowCountStatementError 1 1 0)
                    )
                )
          )
    ]
