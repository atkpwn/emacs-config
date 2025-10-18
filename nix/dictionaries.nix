{ pkgs }:

let
  mkDict = { name, url, hash }:
    pkgs.stdenv.mkDerivation {
      name = name;
      src = pkgs.fetchurl {
        url = url;
        hash = hash;
      };

      nativeBuildInputs = with pkgs; [
        dpkg
      ];

      installPhase = ''
        mkdir -p $out
        cp -r usr/share $out/share
      '';
      meta.platforms = with pkgs.lib.platforms; linux ++ darwin;
    };

  baseUrl = "http://de.archive.ubuntu.com/ubuntu/pool";

in [
  (mkDict {
    name = "dict-devil";
    url = "${baseUrl}/universe/d/dict-devil/dict-devil_1.0-13.1_all.deb";
    hash = "sha256-+Cef5SNtHMczPOCQ1rQAemJ8XcvG0e/0sgtTTuaV/pM=";
  })

  (mkDict {
    name = "dict-foldoc";
    url = "${baseUrl}/main/d/dict-foldoc/dict-foldoc_20250211-1_all.deb";
    hash = "sha256-H+Vjl6e0UZCf4KHd2w8UntVFjZTivZF2712V5+h/8OY=";
  })

  (mkDict {
    name = "dict-gcide";
    url = "http://de.archive.ubuntu.com/ubuntu/pool/main/d/dict-gcide/dict-gcide_0.48.5+nmu4_all.deb";
    hash = "sha256-XV5QWuvE8prhXBmx/Rzgb4rODoyDxTiaiRZ9pYbbnEU=";
  })

  (mkDict {
    name = "dict-jargon";
    url = "${baseUrl}/main/d/dict-jargon/dict-jargon_4.4.7-3.1_all.deb";
    hash = "sha256-2JMS/i28RE+1LRWz+f7pSQykPzruUTNFEQ/TgK64hRs=";
  })

  (mkDict {
    name = "dict-vera";
    url = "${baseUrl}/main/v/vera/dict-vera_1.24-1_all.deb";
    hash = "sha256-f8SR4NZJs2nM3sbR8Dh2l19VWL14LSmm2y89uMoPLok=";
  })

  (mkDict {
    name = "dict-wn";
    url = "${baseUrl}/universe/w/wordnet/dict-wn_3.0-38ubuntu1_all.deb";
    hash = "sha256-n/3rmpZLdi3a8divSFSVmm9NLBrYKM7jKZZprPDqCZg=";
  })
]
