{
  inputs = {
    nixpkgs.url = "https://github.com/NixOS/nixpkgs/archive/08f2208.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-updates.url = "github:NixOS/nixpkgs/haskell-updates";
  };
  outputs = { self, nixpkgs, haskell-updates, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
            inherit system;
            overlays = [
                (final: prev: {
                    haskell-language-server =
                        haskell-updates.legacyPackages.${system}.haskell.packages.ghc9102.haskell-language-server;
                })
            ];
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            stack
            haskell.compiler.ghc9102
            haskell-language-server
            haskellPackages.fourmolu
            haskellPackages.hoogle
            zlib
          ];
          NIX_LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.stdenv.cc.cc
            pkgs.openssl
            pkgs.zlib
          ];
          NIX_LD = pkgs.lib.fileContents "${pkgs.stdenv.cc}/nix-support/dynamic-linker";
        };
      }
    );
}
