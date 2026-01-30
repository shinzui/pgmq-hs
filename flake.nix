{
  description = "Haskell nix template";

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

      in
      {
        formatter = formatter;
        checks = {
          formatting = treefmtEval.config.build.check self;
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              treefmt.package = formatter;
              treefmt.enable = true;

            };
          };
        };
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            zlib
            xz
            just
            postgresql
            pkg-config
            cabal-install
            process-compose
            (haskell.packages.${ghcVersion}.ghcWithPackages (ps: with ps; [
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
            fi
          '' + self.checks.${system}.pre-commit-check.shellHook;
        };
      }
    );
}
