-- mori.dhall
-- Project identity manifest for pgmq-hs
-- See: https://github.com/shinzui/mori
let Schema =
      https://raw.githubusercontent.com/shinzui/mori-schema/3522f4a51181d73c9c90fc27a7c0838bd29ae95f/package.dhall
        sha256:dcb19e2312e790bad14e622cc98a1281cd2298c5b564a2f0d0534d3c718d8803

let augDefault =
      { extraDocs = [] : List Schema.DocRef.Type
      , localPathOverride = None Text
      , kind = None Schema.DependencyKind
      , source = None Schema.DependencySource
      , scope = None Schema.DependencyScope
      , versionConstraint = None Text
      }

let internalDep =
      \(name : Text) ->
        Schema.Dependency.WithAugmentation
          (augDefault // { name, kind = Some Schema.DependencyKind.Internal })

let thirdPartyDep =
      \(name : Text) ->
        Schema.Dependency.WithAugmentation
          (   augDefault
           // { name
              , kind = Some Schema.DependencyKind.ThirdParty
              , source = Some Schema.DependencySource.Hackage
              }
          )

let pkgRef =
      \(namespace : Text) ->
      \(name : Text) ->
      \(package : Text) ->
        Schema.MoriRef::{ namespace
        , name
        , kind = Some Schema.MoriArtifactKind.Package
        , key = Some package
        }

in  Schema.Project::{ project =
      Schema.ProjectIdentity::{ name = "pgmq-hs"
      , namespace = "shinzui"
      , type = Schema.PackageType.Library
      , description = Some
          "Haskell client for pgmq — message queuing via PostgreSQL"
      , language = Schema.Language.Haskell
      , lifecycle = Schema.Lifecycle.Active
      , domains = [ "Messaging", "PostgreSQL" ]
      , owners = [ "shinzui" ]
      }
    , repos =
      [ Schema.Repo::{ name = "pgmq-hs"
        , github = Some "shinzui/pgmq-hs"
        , localPath = Some "./"
        }
      ]
    , packages =
      [ Schema.Package::{ name = "pgmq-core"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "./pgmq-core"
        , description = Some "Core types and type classes"
        }
      , Schema.Package::{ name = "pgmq-hasql"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "./pgmq-hasql"
        , description = Some "Hasql-based pgmq implementation"
        , dependencies = [ internalDep "pgmq-core", thirdPartyDep "hasql/hasql:hasql" ]
        }
      , Schema.Package::{ name = "pgmq-effectful"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "./pgmq-effectful"
        , description = Some "Effectful effects for pgmq"
        , dependencies =
          [ internalDep "pgmq-core"
          , internalDep "pgmq-hasql"
          , thirdPartyDep "effectful/effectful:effectful-core"
          , thirdPartyDep "hasql/hasql:hasql"
          ]
        }
      , Schema.Package::{ name = "pgmq-migration"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "./pgmq-migration"
        , description = Some "Schema migrations without pgmq extension"
        , dependencies =
          [ thirdPartyDep "hasql/hasql:hasql"
          , thirdPartyDep "hasql/hasql:hasql-transaction"
          , thirdPartyDep "shinzui/pg-migrate:pg-migrate"
          , thirdPartyDep "shinzui/pg-migrate:pg-migrate-embed"
          , thirdPartyDep "shinzui/pg-migrate:pg-migrate-import-hasql-migration"
          ]
        }
      , Schema.Package::{ name = "pgmq-config"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "./pgmq-config"
        , description = Some
            "Declarative queue configuration and reconciliation"
        , dependencies =
          [ internalDep "pgmq-core"
          , internalDep "pgmq-hasql"
          , internalDep "pgmq-effectful"
          , thirdPartyDep "hasql/hasql:hasql"
          , thirdPartyDep "effectful/effectful:effectful-core"
          ]
        }
      , Schema.Package::{ name = "pgmq-bench"
        , type = Schema.PackageType.Tool
        , language = Schema.Language.Haskell
        , path = Some "./pgmq-bench"
        , description = Some "Benchmarks"
        , visibility = Schema.Visibility.Internal
        , dependencies =
          [ internalDep "pgmq-core"
          , internalDep "pgmq-hasql"
          , internalDep "pgmq-effectful"
          , internalDep "pgmq-migration"
          , thirdPartyDep "hasql/hasql:hasql"
          , thirdPartyDep "effectful/effectful:effectful-core"
          ]
        }
      ]
    , bundles =
      [ Schema.PackageBundle::{ name = "pgmq-hs"
        , description = Some
            "pgmq-core + pgmq-hasql + pgmq-effectful + pgmq-migration + pgmq-config"
        , packages =
          [ "pgmq-core"
          , "pgmq-hasql"
          , "pgmq-effectful"
          , "pgmq-migration"
          , "pgmq-config"
          ]
        , primary = "pgmq-hasql"
        }
      ]
    , dependencies =
      [ "pgmq/pgmq:pgmq-extension"
      , "hasql/hasql:hasql"
      , "hasql/hasql:hasql-transaction"
      , "effectful/effectful:effectful-core"
      , "shinzui/pg-migrate:pg-migrate"
      , "shinzui/pg-migrate:pg-migrate-embed"
      , "shinzui/pg-migrate:pg-migrate-import-hasql-migration"
      ]
    , dependencyRefs =
      [ pkgRef "pgmq" "pgmq" "pgmq-extension"
      , pkgRef "hasql" "hasql" "hasql"
      , pkgRef "hasql" "hasql" "hasql-transaction"
      , pkgRef "effectful" "effectful" "effectful-core"
      , pkgRef "shinzui" "pg-migrate" "pg-migrate"
      , pkgRef "shinzui" "pg-migrate" "pg-migrate-embed"
      , pkgRef "shinzui" "pg-migrate" "pg-migrate-import-hasql-migration"
      ]
    , docs =
      [ Schema.DocRef::{ key = "otel-instrumentation"
        , kind = Schema.DocKind.Guide
        , audience = Schema.DocAudience.User
        , description = Some "OpenTelemetry instrumentation guide"
        , location =
            Schema.DocLocation.LocalFile
              "./docs/OPENTELEMETRY_INSTRUMENTATION.md"
        }
      , Schema.DocRef::{ key = "queue-configuration"
        , kind = Schema.DocKind.Guide
        , audience = Schema.DocAudience.User
        , description = Some
            "Declarative queue configuration with pgmq-config"
        , location =
            Schema.DocLocation.LocalFile "./docs/user/queue-configuration.md"
        }
      ]
    , okfBundles =
      [ Schema.OkfBundle::{ name = "capabilities"
        , path = "docs/capabilities"
        , profile = Some "docs/capabilities/profile.dhall"
        , okfVersion = "0.2"
        , description = Some
            "What pgmq-hs provides today, one concept per capability, with evidence"
        }
      , Schema.OkfBundle::{ name = "improvement-requests"
        , path = "docs/improvement-requests"
        , profile = Some "docs/improvement-requests/profile.dhall"
        , okfVersion = "0.2"
        , description = Some
            "Cross-repository improvement requests owned by pgmq-hs"
        }
      ]
    }
