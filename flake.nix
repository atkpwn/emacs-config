{
  description = "My Emacs Module for Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs   =  import nixpkgs {
      inherit system;
    };
    epkgs = pkgs.callPackage ./nix/epackages {};
    myEmacsWithPackages = import ./default.nix {
      inherit pkgs epkgs;
    };
  in {

    packages.${system}.default = myEmacsWithPackages;

    homeModules.emacs-with-env = import ./nix/home-manager-module.nix {
      inherit pkgs myEmacsWithPackages;
    };
  };
}
