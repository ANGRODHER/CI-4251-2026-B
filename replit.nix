# replit.nix
{ pkgs }: {
  deps = [
    pkgs.haskell.compiler.ghc9101
    pkgs.haskellPackages.hpack
    pkgs.haskellPackages.hoogle
    pkgs.stack
    pkgs.just
    pkgs.pkg-config
    pkgs.postgresql
    pkgs.zlib
    pkgs.openssl
    pkgs.gmp
    pkgs.libffi
  ];
}
