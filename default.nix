{ nixpkgs ? import (
  (import <nixpkgs> {}).fetchgit {
    url = https://github.com/NixOS/nixpkgs.git;
    rev = "0cbcb08d22625bd3dd55ede6488e8f8ea814fcbe";
    sha256 = "0ayh7li66780264gmc0m3c2xyk1b7bmdqzyn0z0y65mi1q2n9jbg";
  }) {}
}:
nixpkgs.haskellPackages.callPackage ./sized-foldable.nix {}
