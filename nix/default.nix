{ sources ? import ./sources.nix
, haskellNixSrc ? sources."haskell.nix"
, nixpkgs ? haskellNixSrc + "/nixpkgs" }:

let
  pkgs = import nixpkgs (import haskellNixSrc);

  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
