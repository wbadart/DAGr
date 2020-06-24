let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = with pkgs.haskellPackages; [
      ghcid
      nodejs
      stack
    ];
  }
