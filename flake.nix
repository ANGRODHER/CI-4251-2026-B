{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [];

      perSystem = { self', pkgs, config, ... }:
      let
        dontCheck = pkgs.haskell.lib.dontCheck;
        pkgs' = import nixpkgs {
          inherit (pkgs) system;
          overlays = [
            (final: prev: {
              haskellPackages = prev.haskellPackages.extend (hfinal: hprev: {
                cabal-add = dontCheck hprev.cabal-add;
                fourmolu = dontCheck hprev.fourmolu;
              });
              haskell = prev.haskell // {
                packages = builtins.mapAttrs
                  (ghcVer: hpkgs: hpkgs.extend (hfinal: hprev: {
                    cabal-add = dontCheck hprev.cabal-add;
                    fourmolu = dontCheck hprev.fourmolu;
                  }))
                  prev.haskell.packages;
              };
            })
          ];
        };
      in
      {
        devShells.default = pkgs'.mkShell {
          name = "haskell-template";
          meta.description = "Haskell development environment";
          inputsFrom = [];
          nativeBuildInputs =
            [ pkgs'.haskellPackages.hpack
              pkgs'.just
              pkgs'.haskell.compiler.ghc912
              (pkgs'.haskell-language-server.override { supportedGhcVersions = [ "912" ]; })
              pkgs'.haskellPackages.hoogle
              pkgs'.stack
              pkgs'.postgresql  # provides libpq
              pkgs'.pkg-config  # needed to find postgresql
              pkgs'.zlib
              pkgs'.openssl
              pkgs'.gmp
              pkgs'.libffi

            ];
          LD_LIBRARY_PATH = pkgs'.lib.makeLibraryPath [
              pkgs'.postgresql
              pkgs'.zlib
              pkgs'.openssl
              pkgs'.gmp
              pkgs'.libffi
          ];
        };
      };
    };
}
