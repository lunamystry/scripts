{ pkgs ? import <nixpkgs> {} }:
with pkgs;
with haskellPackages;

let
  cabal2nix = src: pkgs.runCommand "cabal2nix" {
    buildCommand = ''
        cabal2nix file://"${builtins.filterSource (path: type: path != ".git") src}" > $out
    '';
    buildInputs = [
        pkgs.cabal2nix
    ];
  } "";
  odisplays = callPackage (cabal2nix ./.) {};
in 
stdenv.mkDerivation rec {
  name = "odisplays";
  src = ./.;
  buildInputs = [
    pkgs.zlib
    odisplays
  ];
  installPhase = ''
    mkdir -p $out/
    cp -r ${odisplays}/* $out
  '';
}
