{
  description = "pgmq-hs - Haskell client library for pgmq";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";

  outputs = { self, nixpkgs, pre-commit-hooks, flake-utils, treefmt-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcVersion = "ghc9122";
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
        formatter = treefmtEval.config.build.wrapper;

        haskellPackages = pkgs.haskell.packages.${ghcVersion}.override {
          overrides = import ./nix/haskell-overlay.nix { inherit pkgs; };
        };

        # Re-enable tests and inject PostgreSQL into PATH
        withTests = pkg:
          pkgs.haskell.lib.overrideCabal (pkgs.haskell.lib.doCheck pkg) (old: {
            testToolDepends = (old.testToolDepends or [ ]) ++ [ pkgs.postgresql ];
            preCheck = (old.preCheck or "") + ''
              export HOME=$(mktemp -d)
            '';
          });
      in
      {
        formatter = formatter;

        packages = {
          pgmq-core = haskellPackages.pgmq-core;
          pgmq-hasql = haskellPackages.pgmq-hasql;
          pgmq-effectful = haskellPackages.pgmq-effectful;
          pgmq-migration = haskellPackages.pgmq-migration;
          default = haskellPackages.pgmq-hasql;
        };

        checks = {
          formatting = treefmtEval.config.build.check self;
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              treefmt.package = formatter;
              treefmt.enable = true;
            };
          };
          inherit (haskellPackages) pgmq-core pgmq-hasql pgmq-effectful pgmq-migration;
          pgmq-hasql-tests = withTests haskellPackages.pgmq-hasql;
          pgmq-migration-tests = withTests haskellPackages.pgmq-migration;
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            zlib
            xz
            just
            postgresql
            pkg-config
            cabal-install
            process-compose
            (haskellPackages.ghcWithPackages (ps: with ps; [
              haskell-language-server
            ]))
          ];
          shellHook = ''
            # Database paths - all relative to project root
            export PGHOST="$PWD/.dev/db"
            export PGDATA="$PGHOST/data"
            export PGLOG="$PGHOST/postgres.log"
            export PGDATABASE=pgmq_dev

            # Connection string for application use
            export PG_CONNECTION_STRING="postgresql://$PGHOST/$PGDATABASE"

            # Initialize database cluster on first entry
            if [ ! -d $PGDATA ]; then
              mkdir -p $PGHOST
              initdb --auth=trust --no-locale --encoding=UTF8
              # Include benchmark config for local development
              echo "include = '$PWD/config/postgresql-benchmark.conf'" >> $PGDATA/postgresql.conf
            fi
          '' + self.checks.${system}.pre-commit-check.shellHook;
        };
      }
    );
}
