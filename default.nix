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
      mkdir -p "$out/bin"
      ghc -o "$out/bin/vim-doc-gen" "$vimdocHs"
    '';
    dontInstall = true;
  };

  shell = pkgs.mkShell {
    inputsFrom = [ vimDocGen ];
    buildInputs = [ vimDocGen ];
  };
}
