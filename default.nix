rec {
  pkgs = import <nixpkgs> {};

  ghc = pkgs.ghc;

  vimDocGen = pkgs.stdenv.mkDerivation rec {
    name = "vim-doc-gen-testing";
    src = ./.;
    inherit ghc;
    buildInputs = [ ghc ];
    vimdocHs = ./vimdocs.hs;
    buildPhase = ''
      ghc -o "$out" "$vimdocHs"
    '';
    dontInstall = true;
  };
}
