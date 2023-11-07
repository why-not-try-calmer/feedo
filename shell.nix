{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  packages = (p: with p; [ stack ]);
  pythonPackages = haskellPackages(packages);
in mkShell {
  buildInputs = [
    ruff
    zlib
  ];
}