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
        in
          ''${pkgs.dict}/bin/dictd \
                --pid-file ${config.home.homeDirectory}/.dictd.pid \
                --logfile ${config.home.homeDirectory}/.dictd.log \
                --config ${dictdConf}'';
        Install.WantedBy = [ "default.target" ]; # see https://unix.stackexchange.com/a/506374
      };
    };
  };
}
