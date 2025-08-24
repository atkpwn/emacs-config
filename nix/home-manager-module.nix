{ pkgs, myEmacsWithPackages, ... }:

{
  home = {
    sessionVariables = {
      EDITOR = "emacsclient";
    };
    packages = with pkgs; [
      myEmacsWithPackages

      nerd-fonts.jetbrains-mono

      # required by epackages.dirvish
      ffmpegthumbnailer
      mediainfo
      poppler_utils
      fd
      imagemagick

      direnv
    ];
  };
}
