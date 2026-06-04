# Dev shell, built from the haskell-nix-dev base flake's mkDevShell (GHC 9.12.4 +
# cabal + HLS). The package build lives in ../flake.module.nix.
#
# mkDevShell already provides: the GHC compiler, cabal, HLS (when withHls),
# pkg-config, and zlib, plus a LANG=en_US.UTF-8 export. Only EXTRA tools beyond
# those go in extraNativeBuildInputs.
{ inputs, lib, flake-parts-lib, ... }:
{
  options.perSystem = flake-parts-lib.mkPerSystemOption ({ ... }: {
    options.haskellProject.extraDevPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      example = lib.literalExpression "[ pkgs.ghciwatch ]";
      description = "Extra packages to add to the dev shell.";
    };
  });

  config.perSystem = { system, pkgs, config, ... }:
    let
      hsdev = inputs.haskell-nix-dev.lib.${system};

      mkProjectShell = ghc: hsdev.mkDevShell {
        inherit ghc;
        withHls = true;
        extraNativeBuildInputs =
          [
            pkgs.xz
            pkgs.just
            pkgs.postgresql
            pkgs.process-compose
          ]
          ++ config.haskellProject.extraDevPackages;
        shellHook = ''
          ${config.pre-commit.installationScript}

          # Database paths - all relative to project root
          export PGHOST="$PWD/.dev/db"
          export PGDATA="$PGHOST/data"
          export PGLOG="$PGHOST/postgres.log"
          export PGDATABASE=pgmq_dev

          # Connection string for application use (libpq key-value format for Unix sockets)
          export PG_CONNECTION_STRING="host=$PGHOST dbname=$PGDATABASE"

          # Initialize database cluster on first entry
          if [ ! -d $PGDATA ]; then
            mkdir -p $PGHOST
            initdb --auth=trust --no-locale --encoding=UTF8
            # Include benchmark config for local development
            echo "include = '$PWD/config/postgresql-benchmark.conf'" >> $PGDATA/postgresql.conf
          fi
        '';
      };
    in
    {
      devShells.default = mkProjectShell "ghc9124";
      devShells.ghc9124 = mkProjectShell "ghc9124";
    };
}
