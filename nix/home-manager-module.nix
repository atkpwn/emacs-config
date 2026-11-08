{ config, pkgs, lib, ... }:

let
  inherit (pkgs.stdenv) hostPlatform;
  home = config.home.homeDirectory;
  myEmacsWithPackages = pkgs.callPackage ../default.nix { };
in {
  nixpkgs.overlays = [
    (final: prev: {
      myEmacsWithPackages = myEmacsWithPackages;
    })
  ];

  home = {
    sessionVariables = {
      EDITOR = "emacsclient";
    };
    packages = with pkgs; let
      dictionaries = callPackage ./dictionaries.nix { };
    in [
      myEmacsWithPackages

      delta

      nerd-fonts.jetbrains-mono

      nuspell
      hunspellDicts.en_US
      hunspellDicts.de_DE

      # org-roam-graph required dot
      graphviz

      # required by epackages.dirvish
      ffmpegthumbnailer
      mediainfo
      poppler-utils
      fd
      imagemagick

      # required by current java setup with lombok
      direnv

      # language servers
      dockerfile-language-server
      cmake-language-server
      nixd

      jdt-language-server
      kotlin-language-server

      gopls
      protobuf-language-server
      # rust-analyzer # install rustup in home-manager instead

      rassumfrassum
      ruff
      ty

      typescript-language-server
      bash-language-server

      helm-ls
    ]
    ++ dictionaries;
  };

  systemd.user = lib.mkIf hostPlatform.isLinux {
    enable = true;
    services = {
      dictd = {
        Unit = {
          Description = "Local dictionary server";
        };
        Service.Type = "forking"; # see https://superuser.com/a/1274913
        Service.ExecStart = let
          dictdConf = pkgs.replaceVars ./dictd.conf {
            dictd = "${config.home.profileDirectory}/share/dictd";
          };
        in ''
        ${pkgs.dict}/bin/dictd \
                --pid-file /run/user/$(id -u)/.dictd.pid \
                --logfile ${home}/.local/state/.dictd.log \
                --config ${dictdConf}
        '';
        Install.WantedBy = [ "default.target" ]; # see https://unix.stackexchange.com/a/506374
      };
    };
  };

  programs = {
    zsh = {
      initContent = lib.mkOrder 1200 ''
        function m() {
          emacsclient -ne "(man \"$1\")";
        }

        function magit() {
          local PROJECT=$(git rev-parse --show-toplevel 2> /dev/null)
          emacsclient -ne "(progn
            (persp-switch \"''${1:-$(basename $PROJECT)}\")
            (find-file \"$PROJECT\")
            (magit-status))"
        }
      '';
      shellAliases = {
        dired = ''
          (
            DIR=''${''${PWD##*/}:-/}
            emacsclient -ne "(progn
              (persp-switch \"$DIR\")
              (find-file \"$PWD\"))"
          )
        '';
        e = "emacsclient";
      } // lib.optionalAttrs hostPlatform.isDarwin {
        emacs = ''open "${home}/Applications/Home Manager Apps/Emacs.app"'';
      };
    };
  };
}
