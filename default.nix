let
  pkgs = import <nixpkgs> { };
  compilerVersion = "ghc883"; 
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
  compiler.developPackage {
    root = ./.;
    source-overrides = {
      language-python = pkgs.fetchFromGitHub {
        owner = "wbadart";
        repo = "language-python";
        rev = "master";
        sha256 = "0ccl7cbqj2andjbzv8vwwqawv2ajn3nvsq93l1fyl3bycbyryrd6";
      };
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
        brittany
        cabal-install
        ghcid
      ]);
  }
