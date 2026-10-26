-- | Check each suite's local root helper using private disposable fixtures.
module RootPermissionsSpec (tests) where

import Control.Exception (IOException, bracket, try)
import Data.Bits ((.&.))
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Posix.Directory qualified as Posix
import System.Posix.Files (createSymbolicLink, fileMode, getSymbolicLinkStatus, isDirectory, setFileMode)
import System.Posix.Temp (mkdtemp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: (FilePath -> IO ()) -> TestTree
tests ensure =
  testGroup
    "RootPermissionsSpec"
    [ testCase "missing root is a real 0700 directory" $ private $ \parent -> do
        let root = parent <> "/root"
        ensure root
        status <- getSymbolicLinkStatus root
        assertBool "real directory" (isDirectory status)
        fileMode status .&. 0o7777 @?= 0o700,
      testCase "owned 0755 directory is tightened without losing contents" $ private $ \parent -> do
        let root = parent <> "/root"
        Posix.createDirectory root 0o755
        setFileMode root 0o755
        writeFile (root <> "/sentinel") "preserve"
        ensure root
        mode root >>= (@?= 0o700)
        readFile (root <> "/sentinel") >>= (@?= "preserve"),
      testCase "repeated call preserves contents and permissions" $ private $ \parent -> do
        let root = parent <> "/root"
        ensure root
        writeFile (root <> "/sentinel") "preserve"
        ensure root
        mode root >>= (@?= 0o700)
        readFile (root <> "/sentinel") >>= (@?= "preserve"),
      testCase "regular file is rejected unchanged" $ private $ \parent -> do
        let root = parent <> "/root"
        writeFile root "preserve"
        setFileMode root 0o644
        rejected (ensure root)
        mode root >>= (@?= 0o644)
        readFile root >>= (@?= "preserve"),
      testCase "symlink is rejected without chmod of its target" $ private $ \parent -> do
        let root = parent <> "/root"
            target = parent <> "/target"
        Posix.createDirectory target 0o755
        setFileMode target 0o755
        writeFile (target <> "/sentinel") "preserve"
        createSymbolicLink target root
        rejected (ensure root)
        mode target >>= (@?= 0o755)
        readFile (target <> "/sentinel") >>= (@?= "preserve")
    ]
  where
    mode path = (.&. 0o7777) . fileMode <$> getSymbolicLinkStatus path
    rejected action = do
      result <- try action :: IO (Either IOException ())
      case result of
        Left _ -> pure ()
        Right () -> assertFailure "unsafe root accepted"
    private action = bracket (getTemporaryDirectory >>= \tmp -> mkdtemp (tmp <> "/pgmq-root-test-")) removePathForcibly action
