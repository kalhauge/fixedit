{ pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/24c765c744b.tar.gz") {}
, compiler ? "ghc882"
}:
let 
  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource; 
  hpkgs = pkgs.haskell.packages."${compiler}";
in 
hpkgs.developPackage { 
  root = gitignoreSource ./.;
  name = "fixedit";
  modifier = drv: pkgs.haskell.lib.addBuildTools drv (with hpkgs; [ cabal-install ghcid ]);
}
