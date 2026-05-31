{ pkgs }:
let
  inherit (pkgs.haskell.lib.compose) doJailbreak dontCheck;
  hsOpenTelemetrySrc = pkgs.fetchFromGitHub {
    owner = "iand675";
    repo = "hs-opentelemetry";
    rev = "46a42cdf80405fdb36fbb48a309254b2332617b4";
    hash = "sha256-4wMAK3WtoSlyrP0IFWFNME///HIXdMZcPfH6ZKpkVfw=";
  };
  threadUtilsSrc = pkgs.fetchFromGitHub {
    owner = "iand675";
    repo = "thread-utils";
    rev = "519ff4613a5b5ee3904be7daefb94bf99ada5ee5";
    hash = "sha256-nlKK794LNHGjXKB1lhCkFJuCEyH+aiGOg6ljV4P1Ijw=";
  };
in
final: prev: {
  # ── Git dependencies: hasql 1.10 ecosystem ──────────────────────────

  postgresql-binary = dontCheck (doJailbreak (final.callCabal2nix "postgresql-binary"
    (pkgs.fetchFromGitHub {
      owner = "nikita-volkov";
      repo = "postgresql-binary";
      rev = "08cd1b7cd9081c80562fa3534fd71f77673d2292";
      hash = "sha256-CNtRdzbQQXAJN5s6UGHRrmdL3jOPnyf5y4X3Ro37uEs=";
    })
    { }));

  hasql = dontCheck (doJailbreak (final.callCabal2nix "hasql"
    (pkgs.fetchFromGitHub {
      owner = "nikita-volkov";
      repo = "hasql";
      rev = "aa3d6ae499e187c291422443f221f9f486c43a9e";
      hash = "sha256-uaR5bzpJeuGKIuYFiMBWohFCM8Wl9RCaK4FC/MWYeOM=";
    })
    { }));

  hasql-pool = dontCheck (doJailbreak (final.callCabal2nix "hasql-pool"
    (pkgs.fetchFromGitHub {
      owner = "nikita-volkov";
      repo = "hasql-pool";
      rev = "35e4c2a9d6b314fbc051e3ca31639bd83dad9f39";
      hash = "sha256-qeYWSHsZ4+3JQGa3ND1roahzAv007OjI12oYbXK4V5Q=";
    })
    { }));

  hasql-transaction = dontCheck (doJailbreak (final.callCabal2nix "hasql-transaction"
    (pkgs.fetchFromGitHub {
      owner = "nikita-volkov";
      repo = "hasql-transaction";
      rev = "6cb37f68bf6f5d378f15bace9d054c6d5ad99583";
      hash = "sha256-3N07okX92/DnGM8wfIftPTRoBcTfal+Bg1icMCBwlWI=";
    })
    { }));

  hasql-migration = dontCheck (doJailbreak (final.callCabal2nix "hasql-migration"
    (pkgs.fetchFromGitHub {
      owner = "shinzui";
      repo = "hasql-migration";
      rev = "ab66f6ae93e40065f8532dd9d497ecb15c91122e";
      hash = "sha256-A6jAeU5WrDCpJ5RJn5EYC7BnwGVtswwREnPdVfdlUpg=";
    })
    { }));

  # ── OpenTelemetry 1.0 family ───────────────────────────────────────
  #
  # Pulled from the same iand675/hs-opentelemetry revision as cabal.project.
  # The semantic-conventions package is generated from OpenTelemetry spec v1.40.

  thread-utils-finalizers = dontCheck (doJailbreak (final.callCabal2nix "thread-utils-finalizers"
    (threadUtilsSrc + "/thread-utils-finalizers")
    { }));

  thread-utils-context = dontCheck (doJailbreak (final.callCabal2nix "thread-utils-context"
    (threadUtilsSrc + "/thread-utils-context")
    { }));

  hs-opentelemetry-api-types = dontCheck (doJailbreak (final.callCabal2nix "hs-opentelemetry-api-types"
    (hsOpenTelemetrySrc + "/api-types")
    { }));

  hs-opentelemetry-api = dontCheck (doJailbreak (final.callCabal2nix "hs-opentelemetry-api"
    (hsOpenTelemetrySrc + "/api")
    { }));

  hs-opentelemetry-semantic-conventions = dontCheck (doJailbreak (final.callCabal2nix "hs-opentelemetry-semantic-conventions"
    (hsOpenTelemetrySrc + "/semantic-conventions")
    { }));

  hs-opentelemetry-propagator-w3c = dontCheck (doJailbreak (final.callCabal2nix "hs-opentelemetry-propagator-w3c"
    (hsOpenTelemetrySrc + "/propagators/w3c")
    { }));

  hs-opentelemetry-exporter-in-memory = dontCheck (doJailbreak (final.callCabal2nix "hs-opentelemetry-exporter-in-memory"
    (hsOpenTelemetrySrc + "/exporters/in-memory")
    { }));

  hs-opentelemetry-sdk = dontCheck (doJailbreak (final.callCabal2nix "hs-opentelemetry-sdk"
    (hsOpenTelemetrySrc + "/sdk")
    { }));

  # ── Test dependencies ──────────────────────────────────────────────

  ephemeral-pg = dontCheck (doJailbreak (final.callCabal2nix "ephemeral-pg"
    (pkgs.fetchFromGitHub {
      owner = "shinzui";
      repo = "ephemeral-pg";
      rev = "c7b8340143dfeaa99c7f61c933fc315dd780d61c";
      hash = "sha256-v7kMgEV7wABH3J+ftxtgfCX0s8iHTXFNXW+fsKXIwEM=";
    })
    { }));

  # ── Local packages ──────────────────────────────────────────────────

  pgmq-core = doJailbreak (final.callCabal2nix "pgmq-core" ../pgmq-core { });

  pgmq-hasql = dontCheck (doJailbreak (final.callCabal2nix "pgmq-hasql" ../pgmq-hasql { }));

  pgmq-effectful = dontCheck (doJailbreak (final.callCabal2nix "pgmq-effectful" ../pgmq-effectful { }));

  pgmq-migration =
    let
      combinedSrc = pkgs.runCommand "pgmq-migration-src" { } ''
        cp -r ${../pgmq-migration} $out
        chmod -R u+w $out
        # The source tree has a `vendor -> ../vendor` symlink so `cabal build`
        # works from the checkout. Replace it with a real directory containing
        # just the SQL files embedFile actually needs.
        rm -f $out/vendor
        mkdir -p $out/vendor/pgmq/pgmq-extension
        cp -r ${../vendor/pgmq/pgmq-extension/sql} $out/vendor/pgmq/pgmq-extension/sql
      '';
    in
    dontCheck (doJailbreak (final.callCabal2nix "pgmq-migration" combinedSrc { }));

  pgmq-config = dontCheck (doJailbreak (final.callCabal2nix "pgmq-config" ../pgmq-config { }));

  pgmq-bench = dontCheck (doJailbreak (final.callCabal2nix "pgmq-bench" ../pgmq-bench { }));
}
