{ pkgs, epkgs }:

let
    myEmacs = if pkgs.stdenv.hostPlatform.isLinux
    then pkgs.emacs-gtk
    else pkgs.emacs-macport;
    emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages;
in
  emacsWithPackages epkgs
