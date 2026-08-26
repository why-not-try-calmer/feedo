{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-updates.url = "github:NixOS/nixpkgs/haskell-updates";
  };
  outputs = { self, nixpkgs, haskell-updates, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
            inherit system;
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            stack
            haskell.compiler.ghc9124
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
          NIX_LD = builtins.readFile "${pkgs.stdenv.cc}/nix-support/dynamic-linker";
        };
      }
    );
}
