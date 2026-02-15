myEmacsWithPackages:
{ config, pkgs, ... }:

{
  home = {
    sessionVariables = {
      EDITOR = "emacsclient";
    };
    packages = with pkgs; let
      dictionaries = callPackage ./dictionaries.nix {};
    in [
      myEmacsWithPackages

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

      (jdt-language-server.override {
        jdk = javaPackages.compiler.openjdk25;
      })
      kotlin-language-server

      gopls
      protobuf-language-server
      rust-analyzer

      nodePackages.typescript-language-server
      nodePackages.bash-language-server

      helm-ls
    ]
    ++ dictionaries;
  };

  systemd.user = {
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
                --logfile ${config.home.homeDirectory}/.local/state/.dictd.log \
                --config ${dictdConf}
        '';
        Install.WantedBy = [ "default.target" ]; # see https://unix.stackexchange.com/a/506374
      };
    };
  };
}
