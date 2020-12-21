# Defines the build set for publishing browser games.

let

  pkgs =
    import ./nix/pkgs.nix { mutantInstance = "browser"; };


  cabal2nix =
    pkgs.compilerPackages.callCabal2nix;


  jsaddle-src =
    pkgs.fetchFromGitHub
    { owner = "ghcjs";
      repo = "jsaddle";
      rev = "68208be806c49a2a0c9f037dfac85feae10a8c80";
      sha256 = "0acj0x716ikfb08ndib36jmwxkwq399lvkip46sfkh1ynn0pvc1c";
    };


  jsaddle =
    cabal2nix
      "jsaddle"
      "${jsaddle-src}/jsaddle"
      {};


  the-mutant =
    cabal2nix
      "the-mutant"
      ./the-mutant
      {};


  the-mutant-js =
    cabal2nix
      "the-mutant-js"
      ./the-mutant-js
      { inherit the-mutant jsaddle ; };


  the-mutant-app =
    cabal2nix
      "the-mutant-app"
      ./the-mutant-app
      { inherit the-mutant ; };


  the-mutant-app-js =
    cabal2nix
      "the-mutant-app-js"
      ./the-mutant-app-js
      { inherit the-mutant-app the-mutant-js ; };


  targets =
    { inherit the-mutant the-mutant-js the-mutant-app the-mutant-app-js ; };


  targetList =
    map
      (t: targets.${t})
      (builtins.attrNames targets);

in
  if pkgs.lib.inNixShell
  then
    pkgs.compilerPackages.shellFor {
      shellHook = ''
        echo blah!
      '';
      packages = _: targetList;
    }
  else targets
