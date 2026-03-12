-- mori.dhall
-- Project identity manifest for pgmq-hs
-- See: https://github.com/shinzui/mori
let Schema =
      https://raw.githubusercontent.com/shinzui/mori-schema/85a34b7f21a33405a76d29a149a8883c168d3777/package.dhall
        sha256:968eb05bdde9e4a7695c71d44fc4224d56bd512826e9cc8d849f367d42e04b86

let emptyDeps = [] : List Schema.Dependency

let emptyDocs = [] : List Schema.DocRef

let emptyConfig = [] : List Schema.ConfigItem

let noRuntime = { deployable = False, exposesApi = False }

in  { project =
      { name = "pgmq-hs"
      , namespace = "shinzui"
      , type = Schema.PackageType.Library
      , description = Some
          "Haskell client for pgmq — message queuing via PostgreSQL"
      , language = Schema.Language.Haskell
      , lifecycle = Schema.Lifecycle.Active
      , domains = [ "Messaging", "PostgreSQL" ]
      , owners = [ "shinzui" ]
      , origin = Schema.Origin.Own
      }
    , repos =
      [ { name = "pgmq-hs"
        , github = Some "shinzui/pgmq-hs"
        , gitlab = None Text
        , git = None Text
        , localPath = Some "./"
        }
      ]
    , packages =
          [ { name = "pgmq-core"
            , type = Schema.PackageType.Library
            , language = Schema.Language.Haskell
            , path = Some "./pgmq-core"
            , description = Some "Core types and type classes"
            , lifecycle = None Schema.Lifecycle
            , visibility = Schema.Visibility.Public
            , runtime = noRuntime
            , dependencies = emptyDeps
            , docs = emptyDocs
            , config = emptyConfig
            }
          , { name = "pgmq-hasql"
            , type = Schema.PackageType.Library
            , language = Schema.Language.Haskell
            , path = Some "./pgmq-hasql"
            , description = Some "Hasql-based pgmq implementation"
            , lifecycle = None Schema.Lifecycle
            , visibility = Schema.Visibility.Public
            , runtime = noRuntime
            , dependencies = emptyDeps
            , docs = emptyDocs
            , config = emptyConfig
            }
          , { name = "pgmq-effectful"
            , type = Schema.PackageType.Library
            , language = Schema.Language.Haskell
            , path = Some "./pgmq-effectful"
            , description = Some "Effectful effects for pgmq"
            , lifecycle = None Schema.Lifecycle
            , visibility = Schema.Visibility.Public
            , runtime = noRuntime
            , dependencies = emptyDeps
            , docs = emptyDocs
            , config = emptyConfig
            }
          , { name = "pgmq-migration"
            , type = Schema.PackageType.Library
            , language = Schema.Language.Haskell
            , path = Some "./pgmq-migration"
            , description = Some "Schema migrations without pgmq extension"
            , lifecycle = None Schema.Lifecycle
            , visibility = Schema.Visibility.Public
            , runtime = noRuntime
            , dependencies = emptyDeps
            , docs = emptyDocs
            , config = emptyConfig
            }
          , { name = "pgmq-config"
            , type = Schema.PackageType.Library
            , language = Schema.Language.Haskell
            , path = Some "./pgmq-config"
            , description = Some
                "Declarative queue configuration and reconciliation"
            , lifecycle = None Schema.Lifecycle
            , visibility = Schema.Visibility.Public
            , runtime = noRuntime
            , dependencies = emptyDeps
            , docs = emptyDocs
            , config = emptyConfig
            }
          , { name = "pgmq-bench"
            , type = Schema.PackageType.Tool
            , language = Schema.Language.Haskell
            , path = Some "./pgmq-bench"
            , description = Some "Benchmarks"
            , lifecycle = None Schema.Lifecycle
            , visibility = Schema.Visibility.Internal
            , runtime = noRuntime
            , dependencies = emptyDeps
            , docs = emptyDocs
            , config = emptyConfig
            }
          ]
        : List Schema.Package
    , bundles =
      [ { name = "pgmq-hs"
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
    , dependencies = [ "pgmq/pgmq" ]
    , apis = [] : List Schema.Api
    , agents = [] : List Schema.AgentHint
    , skills = [] : List Schema.Skill
    , subagents = [] : List Schema.Subagent
    , standards = [] : List Text
    , docs =
          [ { key = "readme"
            , kind = Schema.DocKind.Reference
            , audience = Schema.DocAudience.User
            , description = Some "Project overview and usage examples"
            , location = Schema.DocLocation.LocalFile "./README.md"
            }
          , { key = "otel-instrumentation"
            , kind = Schema.DocKind.Guide
            , audience = Schema.DocAudience.User
            , description = Some "OpenTelemetry instrumentation guide"
            , location =
                Schema.DocLocation.LocalFile
                  "./docs/OPENTELEMETRY_INSTRUMENTATION.md"
            }
          , { key = "queue-configuration"
            , kind = Schema.DocKind.Guide
            , audience = Schema.DocAudience.User
            , description = Some
                "Declarative queue configuration with pgmq-config"
            , location =
                Schema.DocLocation.LocalFile
                  "./docs/user/queue-configuration.md"
            }
          ]
        : List Schema.DocRef
    }
