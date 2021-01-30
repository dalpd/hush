#! /usr/bin/env nix-shell
{ nixpkgs ? import <nixpkgs> {
  overlays = [
    (import ./nix/overlay.nix)
  ];
}, compiler ? "ghc865" }:
(import ./default.nix {
  inherit nixpkgs compiler;
}).env
