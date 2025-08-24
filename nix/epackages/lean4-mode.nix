{
  pkgs,
  epkgs
}:
# see
# - https://github.com/NixOS/nixpkgs/blob/nixos-25.05/pkgs/applications/editors/emacs/build-support/trivial.nix
# - https://github.com/leanprover-community/lean4-mode/issues/7
epkgs.trivialBuild rec {
  pname = "lean4-mode";
  version = "latest";
  src = pkgs.fetchFromGitHub {
    owner = "leanprover-community";
    repo = pname;
    rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
    hash = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
  };
  propagatedUserEnvPkgs = with epkgs; [
    company
    compat
    dash
    lsp-mode
    magit-section
  ];
  buildInputs = propagatedUserEnvPkgs;
  postInstall = ''
    cp -r data $out/share/emacs/site-lisp
  '';
}
