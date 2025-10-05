{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  pname = "gcide";
  version = "0.48.5";

  src = pkgs.fetchurl {
    url = "http://de.archive.ubuntu.com/ubuntu/pool/main/d/dict-gcide/dict-gcide_${version}+nmu4_all.deb";
    hash = "sha256-XV5QWuvE8prhXBmx/Rzgb4rODoyDxTiaiRZ9pYbbnEU=";
  };

  nativeBuildInputs = with pkgs; [
    dpkg
  ];

  installPhase = ''
    mkdir -p $out
    cp -r usr/share $out/share
  '';

  meta.platforms = with pkgs.lib.platforms; linux ++ darwin;
}
