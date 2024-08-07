{
  inputs = {
    nixpkgs.url = "https://github.com/NixOS/nixpkgs/archive/54aac082a4d9.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        build_tools = [
          pkgs.haskellPackages.stack
          pkgs.haskell.compiler.ghc94
        ];
        other_tools = [
          pkgs.haskellPackages.haskell-language-server
          pkgs.haskellPackages.fourmolu
          pkgs.haskellPackages.hoogle
        ];
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            build_tools
            other_tools
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
