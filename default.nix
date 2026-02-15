{ pkgs, ... }:

let
  myEmacs = if pkgs.stdenv.hostPlatform.isLinux
  then pkgs.emacs-gtk
  else pkgs.emacs-macport;
  emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages;
  epkgs = pkgs.callPackage ./nix/epackages {};
in
  emacsWithPackages epkgs
