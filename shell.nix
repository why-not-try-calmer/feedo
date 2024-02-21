{ pkgs ? import
    (builtins.fetchTarball
      {
        name = "nixos-unstable-2023-12-21";
        url = https://github.com/NixOS/nixpkgs/archive/54aac082a4d9.tar.gz;
        sha256 = "1lx3v67ymjr8vy49hzm9z52zzg7g5ak1x33qcp4vcp75rg04vlgs";
      }
    )
    { }
}:
pkgs.mkShell
{
  buildInputs = [
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.hoogle
    pkgs.zlib
  ];
  runScript = "bash";
}
