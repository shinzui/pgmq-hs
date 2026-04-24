{ pkgs }:
let
  inherit (pkgs.haskell.lib.compose) doJailbreak dontCheck;
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

  # ── OpenTelemetry semantic conventions ─────────────────────────────
  #
  # Not currently packaged in nixpkgs. Pulled from the same iand675/hs-opentelemetry
  # revision as hs-opentelemetry-api/propagator-w3c (see cabal.project), which
  # targets OpenTelemetry spec v1.24.

  hs-opentelemetry-semantic-conventions = dontCheck (doJailbreak (final.callCabal2nix "hs-opentelemetry-semantic-conventions"
    (pkgs.fetchFromGitHub
      {
        owner = "iand675";
        repo = "hs-opentelemetry";
        rev = "adc464b0a45e56a983fa1441be6e432b50c29e0e";
        hash = "sha256-WG/i8jt8u9olC2bAdbKRamhqyBzYYJ7q/nrGsVUMmEE=";
      } + "/semantic-conventions")
    { }));

  # Used by the pgmq-effectful test suite to assert v1.24 span attributes.
  # Pulled from the same iand675/hs-opentelemetry revision (see cabal.project);
  # nixpkgs has this package marked broken.
  hs-opentelemetry-exporter-in-memory = dontCheck (doJailbreak (final.callCabal2nix "hs-opentelemetry-exporter-in-memory"
    (pkgs.fetchFromGitHub
      {
        owner = "iand675";
        repo = "hs-opentelemetry";
        rev = "adc464b0a45e56a983fa1441be6e432b50c29e0e";
        hash = "sha256-WG/i8jt8u9olC2bAdbKRamhqyBzYYJ7q/nrGsVUMmEE=";
      } + "/exporters/in-memory")
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
