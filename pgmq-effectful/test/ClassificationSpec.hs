{-# LANGUAGE OverloadedStrings #-}

module ClassificationSpec (tests) where

import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Hasql.Errors qualified as HasqlErrors
import Pgmq.Effectful
  ( PgmqRuntimeError (..),
    isAmbiguousReply,
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

-- | A server error reported for a 'HasqlErrors.ScriptSessionError' with the
-- given SQLSTATE.
scriptError :: Text -> PgmqRuntimeError
scriptError code =
  PgmqSessionError
    ( HasqlErrors.ScriptSessionError
        "checkpoint"
        (HasqlErrors.ServerError code "boom" Nothing Nothing Nothing)
    )

-- | The value hasql 1.10 surfaces when libpq's own connection-loss result is
-- decoded: a server error with every field empty.
lostReplyStatementError :: PgmqRuntimeError
lostReplyStatementError =
  PgmqSessionError
    ( HasqlErrors.StatementSessionError
        1
        0
        "select 1"
        []
        True
        (HasqlErrors.ServerStatementError (HasqlErrors.ServerError "" "" Nothing Nothing Nothing))
    )

-- | A row-count statement error with the given bounds and actual count.
rowCountError :: Int -> Int -> Int -> PgmqRuntimeError
rowCountError lo hi actual =
  PgmqSessionError
    ( HasqlErrors.StatementSessionError
        1
        0
        "select 1"
        []
        True
        (HasqlErrors.UnexpectedRowCountStatementError lo hi actual)
    )

-- | Connection-level samples (acquisition and connect failures).
connectionSamples :: [(String, PgmqRuntimeError)]
connectionSamples =
  [ ("networking connection error", PgmqConnectionError (HasqlErrors.NetworkingConnectionError "refused")),
    ("authentication error", PgmqConnectionError (HasqlErrors.AuthenticationConnectionError "bad password")),
    ("compatibility error", PgmqConnectionError (HasqlErrors.CompatibilityConnectionError "version mismatch")),
    ("other connection error", PgmqConnectionError (HasqlErrors.OtherConnectionError "libpq says no"))
  ]

-- | Every sample error this module constructs, for whole-table properties.
allSamples :: [(String, PgmqRuntimeError)]
allSamples =
  [("acquisition timeout", PgmqAcquisitionTimeout)]
    <> connectionSamples
    <> [ ("connection-drop session error", PgmqSessionError (HasqlErrors.ConnectionSessionError "dropped")),
         ("driver session error", PgmqSessionError (HasqlErrors.DriverSessionError "bug")),
         ("missing-types session error", PgmqSessionError (HasqlErrors.MissingTypesSessionError HashSet.empty)),
         ("empty-SQLSTATE statement error", lostReplyStatementError),
         ("empty-SQLSTATE script error", scriptError ""),
         ("row count 1 1 0", rowCountError 1 1 0),
         ("row count 1 1 1", rowCountError 1 1 1),
         ("row count 1 1 2", rowCountError 1 1 2)
       ]
    <> [("statement " <> show code, serverStatementError code) | (code, _) <- transientStates <> permanentStates]
    <> [("script " <> show code, scriptError code) | (code, _) <- transientStates <> permanentStates]

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
      testCase "row-count decode failure (1 1 0) is not transient" $
        assertBool "expected permanent" (not (isTransient (rowCountError 1 1 0))),
      testCase "row-count decode failure (1 1 2) is not transient" $
        assertBool "expected permanent" (not (isTransient (rowCountError 1 1 2))),
      testGroup
        "lost replies (BUG-1)"
        [ testCase "statement error with empty SQLSTATE is transient" $
            assertBool "expected transient" (isTransient lostReplyStatementError),
          testCase "script error with empty SQLSTATE is transient" $
            assertBool "expected transient" (isTransient (scriptError "")),
          testCase "stray-result shape (1 1 1) is transient" $
            assertBool "expected transient" (isTransient (rowCountError 1 1 1))
        ],
      testGroup
        "script errors follow the SQLSTATE rule"
        [ testCase "script 57P01 is transient" $
            assertBool "expected transient" (isTransient (scriptError "57P01")),
          testCase "script 23505 is permanent" $
            assertBool "expected permanent" (not (isTransient (scriptError "23505")))
        ],
      testGroup
        "isAmbiguousReply"
        ( [ testCase "empty-SQLSTATE statement error is ambiguous" $
              assertBool "expected ambiguous" (isAmbiguousReply lostReplyStatementError),
            testCase "empty-SQLSTATE script error is ambiguous" $
              assertBool "expected ambiguous" (isAmbiguousReply (scriptError "")),
            testCase "stray-result shape (1 1 1) is ambiguous" $
              assertBool "expected ambiguous" (isAmbiguousReply (rowCountError 1 1 1))
          ]
            <> [ testCase (name <> " is not ambiguous") $
                   assertBool "expected unambiguous" (not (isAmbiguousReply err))
               | (name, err) <-
                   [ ("statement \"57P01\"", serverStatementError "57P01"),
                     ("statement \"23505\"", serverStatementError "23505"),
                     ("script \"57P01\"", scriptError "57P01"),
                     ("row count 1 1 0", rowCountError 1 1 0),
                     ("row count 1 1 2", rowCountError 1 1 2),
                     ("connection-drop session error", PgmqSessionError (HasqlErrors.ConnectionSessionError "dropped")),
                     ("acquisition timeout", PgmqAcquisitionTimeout),
                     ("driver session error", PgmqSessionError (HasqlErrors.DriverSessionError "bug")),
                     ("missing-types session error", PgmqSessionError (HasqlErrors.MissingTypesSessionError HashSet.empty))
                   ]
                     <> connectionSamples
               ]
            <> [ testCase "every ambiguous reply is transient" $
                   mapM_
                     ( \(name, err) ->
                         assertBool
                           (name <> " is ambiguous but not transient")
                           (not (isAmbiguousReply err) || isTransient err)
                     )
                     allSamples
               ]
        )
    ]
