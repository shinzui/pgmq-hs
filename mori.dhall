-- mori.dhall
-- Project identity manifest for pgmq-hs
-- See: https://github.com/shinzui/mori
let Schema =
      https://raw.githubusercontent.com/shinzui/mori-schema/f53517e1a532275569bb14a452359f11c3e02c03/package.dhall
        sha256:3b79aae9216456678300441ca8616b64a4b4fa520a1286dfcc418f60899d5d4a

let augDefault =
      { extraDocs = [] : List Schema.DocRef.Type
      , localPathOverride = None Text
      , kind = None Schema.DependencyKind
      , source = None Schema.DependencySource
      , scope = None Schema.DependencyScope
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
        , dependencies = [ internalDep "pgmq-core", thirdPartyDep "hasql" ]
        }
      , Schema.Package::{ name = "pgmq-effectful"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "./pgmq-effectful"
        , description = Some "Effectful effects for pgmq"
        , dependencies =
          [ internalDep "pgmq-core"
          , internalDep "pgmq-hasql"
          , thirdPartyDep "effectful-core"
          , thirdPartyDep "hasql"
          ]
        }
      , Schema.Package::{ name = "pgmq-migration"
        , type = Schema.PackageType.Library
        , language = Schema.Language.Haskell
        , path = Some "./pgmq-migration"
        , description = Some "Schema migrations without pgmq extension"
        , dependencies =
          [ thirdPartyDep "hasql"
          , thirdPartyDep "hasql-migration"
          , thirdPartyDep "hasql-transaction"
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
          , thirdPartyDep "hasql"
          , thirdPartyDep "effectful-core"
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
          , thirdPartyDep "hasql"
          , thirdPartyDep "effectful-core"
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
    , dependencies = [ "pgmq/pgmq" ]
    , docs =
      [ Schema.DocRef::{ key = "readme"
        , kind = Schema.DocKind.Reference
        , audience = Schema.DocAudience.User
        , description = Some "Project overview and usage examples"
        , location = Schema.DocLocation.LocalFile "./README.md"
        }
      , Schema.DocRef::{ key = "otel-instrumentation"
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
    }
