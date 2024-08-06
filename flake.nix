{
  inputs = {
    nixpkgs.url = "https://github.com/NixOS/nixpkgs/archive/54aac082a4d9.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        tools = [
          pkgs.haskellPackages.fourmolu
          pkgs.haskellPackages.hoogle
          pkgs.haskellPackages.stack
        ];
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.haskellPackages.haskell-language-server
            tools
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
