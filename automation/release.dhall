-- Turn an observed release tag into the one immutable Project release fact
-- mori keeps for shinzui/pgmq-hs.
--
-- The same shape shinzui/kioku, shinzui/baikai, shinzui/shikumi and
-- shinzui/keiro use. pgmq-hs publishes nothing about itself otherwise, so a
-- release fact is what lets `mori registry releases shinzui/pgmq-hs
-- --with-dependents` answer which consumers are pinned behind a version.
--
-- Registered as its own named automation (`--name release`) rather than as the
-- repo's root mori.automation.dhall, which does not exist yet. That follows the
-- fleet convention and keeps the root free: this automation wants
-- `queued = True`, and a single automation must agree on every scalar policy,
-- so a future long-running reaction here would otherwise have to hold a
-- recording behind it.
--
-- Pinned to mori-schema 3522f4a, the commit the current mori binary embeds
-- (`mori schema pin`), so the import resolves without touching the network.
let Schema =
      https://raw.githubusercontent.com/shinzui/mori-schema/3522f4a51181d73c9c90fc27a7c0838bd29ae95f/package.dhall
        sha256:dcb19e2312e790bad14e622cc98a1281cd2298c5b564a2f0d0534d3c718d8803

in  Schema.Automation::{
    , events =
      [ Schema.EventSelector.RefSelector Schema.RefSelector::{
        , name = "pgmq-hs-release-tag"
        ,
          -- pgmq-hs cuts ONE tag per release -- `v0.5.0.0` covers every
          -- released package, which share a version -- so there is no umbrella
          -- tag to pick out of a set of siblings. A whole-input POSIX extended
          -- regex is still preferred over the `refPatterns = [ "v*" ]` glob it
          -- could have been: globs understand `*` and `**` and nothing else, so
          -- `v*` would also fire on a future `vendor-...` tag and record a
          -- version that is not one. That is a live risk here rather than a
          -- hypothetical: this repo vendors upstream pgmq by git subtree, and a
          -- subtree pull brings the upstream tag namespace with it. `[.]` for
          -- the literal dot: a Dhall double-quoted string would otherwise need
          -- the backslash doubled.
          refRegexes = [ "v[0-9]+([.][0-9]+)*" ]
        , kinds = [ "tag" ]
        }
      ]
    , reactions =
      [ Schema.Reaction::{
        , name = "record-pgmq-hs-release"
        , on = [ "pgmq-hs-release-tag" ]
        , actions =
          [ Schema.ReactionAction.RunCommand Schema.RunCommandAction::{
            , command = "./scripts/record-release.sh"
            , args = [ "{{ref.name}}" ]
            ,
              -- Not the 600-second default, which would hold the FIFO group for
              -- ten minutes on a hung database -- but not 60 seconds either.
              -- Every RunCommand is executed as `nix develop --command`, and
              -- that entry, not the single `mori registry release record`
              -- against a local Postgres, dominates: 60s timed out reactions in
              -- shinzui/keiro and shinzui/baikai while the nix eval cache was
              -- cold and contended, which is how a release fact there went
              -- unrecorded. This repo's devshell builds GHC 9.12.4 and HLS, so
              -- a cold entry here is slower still.
              timeout = Some +300
            }
          ]
        }
      ]
    ,
      -- A release cut triggers this once, so there is nothing to serialize in
      -- the ordinary case. It is kept for the replay case:
      -- `mori automate reset-checkpoint --to-root` re-observes every `v*` tag in
      -- the repo's history at once, and serializing keeps those invocations from
      -- racing each other into the same Project stream. Re-recording a version
      -- is already safe -- the first committed release time and source win -- so
      -- this is about avoiding contention, not correctness.
      queued = True
    , execution = Schema.ExecutionPolicy::{ allowLocal = True }
    }
