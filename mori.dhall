-- mori.dhall
-- Project identity manifest for pgmq-hs
-- See: https://github.com/shinzui/mori
let Schema =
      https://raw.githubusercontent.com/shinzui/mori-schema/28dfc529336f0c92a846f074b5f19c6442394a84/package.dhall
        sha256:dda2cb9c528a0edac9c5ba7aeb14517d153ec612ec70c30115bb6749f15df15b

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
            "pgmq-core + pgmq-hasql + pgmq-effectful + pgmq-migration"
        , packages =
          [ "pgmq-core", "pgmq-hasql", "pgmq-effectful", "pgmq-migration" ]
        , primary = "pgmq-hasql"
        }
      ]
    , dependencies = [] : List Text
    , apis = [] : List Schema.Api
    , agents = [] : List Schema.AgentHint
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
          ]
        : List Schema.DocRef
    }
