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
  odisplay = callPackage (cabal2nix ./.) {};
in 
stdenv.mkDerivation rec {
  name = "odisplay";
  src = ./.;
  buildInputs = [
    pkgs.zlib
    odisplay
  ];
  installPhase = ''
    mkdir -p $out/
    cp -r ${odisplay}/* $out
  '';
}
