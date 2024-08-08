{
  inputs = {
    nixpkgs.url = "https://github.com/NixOS/nixpkgs/archive/54aac082a4d9.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.stack
            pkgs.ghc
            pkgs.haskell-language-server
            pkgs.haskellPackages.fourmolu
            pkgs.haskellPackages.hoogle
            pkgs.zlib
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