{-# LANGUAGE TemplateHaskell #-}

-- | Explicit imports from the predecessor @hasql-migration@ ledger.
module Pgmq.Migration.History.HasqlMigration
  ( AlternativeHistoryPolicy (..),
    pgmqHasqlMigrationMappings,
    pgmqHasqlMigrationSourceConfig,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Database.PostgreSQL.Migrate
  ( ConnectionProvider,
    EvidenceRequirement (AllOf, Evidence),
    HistoryDefinitionError,
    HistoryMapping,
    MigrationId,
    PayloadRelation (EquivalentState, SamePayload),
    evidenceKey,
    historyMapping,
    migrationId,
  )
import Database.PostgreSQL.Migrate.History.HasqlMigration
  ( HasqlMigrationDefinitionError,
    HasqlMigrationSourceConfig,
    defaultHasqlMigrationTable,
    hasqlMigrationSourceConfig,
  )
import Pgmq.Migration.Internal.Definition (embeddedMigrationEntries)
import Pgmq.Migration.SchemaContract
  ( pgmqV1_11StateEvidenceKey,
    pgmqV1_11StateValidator,
  )

-- | Select exactly one predecessor history shape. Equivalent history is never implicit.
data AlternativeHistoryPolicy
  = DirectFullInstallHistory
  | EquivalentTwoStepUpgradeHistory
  deriving stock (Eq, Show)

-- | Map the selected predecessor history shape to the native PGMQ baseline.
pgmqHasqlMigrationMappings ::
  AlternativeHistoryPolicy ->
  Either HistoryDefinitionError (NonEmpty HistoryMapping)
pgmqHasqlMigrationMappings policy = do
  directKey <- evidenceKey ("hasql-migration:" <> Text.pack directFilename)
  firstUpgradeKey <- evidenceKey ("hasql-migration:" <> Text.pack firstUpgradeFilename)
  secondUpgradeKey <- evidenceKey ("hasql-migration:" <> Text.pack secondUpgradeFilename)
  pure $ case policy of
    DirectFullInstallHistory ->
      historyMapping targetMigration (Evidence directKey) (SamePayload directKey) :| []
    EquivalentTwoStepUpgradeHistory ->
      historyMapping
        targetMigration
        ( AllOf
            ( Evidence firstUpgradeKey
                :| [ Evidence secondUpgradeKey,
                     Evidence pgmqV1_11StateEvidenceKey
                   ]
            )
        )
        EquivalentState
        :| []

-- | Build a strict source-reader configuration for the selected legacy shape.
--
-- The adapter independently reproduces and checks each base64 MD5 stored in
-- @public.schema_migrations@ before the mappings can be imported.
pgmqHasqlMigrationSourceConfig ::
  ConnectionProvider ->
  AlternativeHistoryPolicy ->
  Either HasqlMigrationDefinitionError HasqlMigrationSourceConfig
pgmqHasqlMigrationSourceConfig sourceProvider policy =
  hasqlMigrationSourceConfig
    sourceProvider
    defaultHasqlMigrationTable
    selectedFilenames
    True
    selectedPayloads
    selectedValidators
    "verified pgmq-migration cutover to native pg-migrate history"
  where
    (selectedFilenames, selectedPayloads, selectedValidators) = case policy of
      DirectFullInstallHistory ->
        ( directFilename :| [],
          Map.singleton directFilename directPayload,
          []
        )
      EquivalentTwoStepUpgradeHistory ->
        ( firstUpgradeFilename :| [secondUpgradeFilename],
          Map.fromList
            [ (firstUpgradeFilename, firstUpgradePayload),
              (secondUpgradeFilename, secondUpgradePayload)
            ],
          [pgmqV1_11StateValidator]
        )

directFilename, firstUpgradeFilename, secondUpgradeFilename :: FilePath
directFilename = "pgmq_v1.11.0"
firstUpgradeFilename = "pgmq_v1.10.0_to_v1.10.1"
secondUpgradeFilename = "pgmq_v1.10.1_to_v1.11.0"

directPayload, firstUpgradePayload, secondUpgradePayload :: ByteString
directPayload = case embeddedMigrationEntries of
  (_, payload) :| _ -> payload
firstUpgradePayload =
  $(embedFile "vendor/pgmq/pgmq-extension/sql/pgmq--1.10.0--1.10.1.sql")
secondUpgradePayload =
  $(embedFile "vendor/pgmq/pgmq-extension/sql/pgmq--1.10.1--1.11.0.sql")

targetMigration :: MigrationId
targetMigration =
  requireDefinition (migrationId "pgmq" "0001-install-v1.11.0")

requireDefinition :: (Show error) => Either error value -> value
requireDefinition = either (error . ("invalid static PGMQ history definition: " <>) . show) id
