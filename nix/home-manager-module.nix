myEmacsWithPackages:
{ config, pkgs, ... }:

let
  gcide = pkgs.callPackage ./gcide.nix {};
in
{
  home = {
    sessionVariables = {
      EDITOR = "emacsclient";
    };
    packages = with pkgs; [
      myEmacsWithPackages

      nerd-fonts.jetbrains-mono

      nuspell
      hunspellDicts.en_US
      hunspellDicts.de_DE

      gcide

      # org-roam-graph required dot
      graphviz

      # required by epackages.dirvish
      ffmpegthumbnailer
      mediainfo
      poppler_utils
      fd
      imagemagick

      direnv
    ];
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
            gcide = gcide;
          };
        in
          ''${pkgs.dict}/bin/dictd --pid-file ${config.home.homeDirectory}/.dictd.pid --logfile ${config.home.homeDirectory}/.dictd.log --config ${dictdConf}'';
        # see https://unix.stackexchange.com/a/506374
        Install.WantedBy = [ "default.target" ];
      };
    };
  };
}
