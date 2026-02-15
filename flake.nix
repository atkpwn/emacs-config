{
  description = "My Emacs Module for Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    systems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];
    forAllSystems = function:
      nixpkgs.lib.genAttrs systems (system:
        function (import nixpkgs {
          inherit system;
          overlays = [];
        }));
  in {

    packages = forAllSystems (pkgs: {
      default = pkgs.callPackage ./default.nix {};
    });

    homeModules = {
      default = self.homeModules.emacs-with-env;
      emacs-with-env = import ./nix/home-manager-module.nix;
    };
  };
}
