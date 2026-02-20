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

  pgmq-effectful = doJailbreak (final.callCabal2nix "pgmq-effectful" ../pgmq-effectful { });

  pgmq-migration = dontCheck (doJailbreak (final.callCabal2nix "pgmq-migration" ../pgmq-migration { }));

  pgmq-bench = dontCheck (doJailbreak (final.callCabal2nix "pgmq-bench" ../pgmq-bench { }));
}
