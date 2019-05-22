# Defines the build set for publishing desktop games.
#{ pkgs }:

let

  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/42c0ce80e6bb1c27c233ba0ec1fd136bced7b0be.tar.gz";
  }) {};


  cabal2nix = pkgs.haskellPackages.callCabal2nix;


  typograffiti-src =
    pkgs.fetchFromGitHub
    { owner = "schell";
      repo = "typograffiti";
      rev = "c8e0b8e470f8fc4f994f2538f5f3c1bb66106a42";
      sha256 = "1c4agisvrwn2jaygzp3p5vnsq6sflpb1aar2w707llv8vi0xbw34";
    };


  typograffiti-core =
    cabal2nix
      "typograffiti-core"
      "${typograffiti-src}/typograffiti-core"
      {};


  typograffiti-freetype =
    cabal2nix
      "typograffiti-freetype"
      "${typograffiti-src}/typograffiti-freetype"
      { inherit typograffiti-core; };


  typograffiti-sdl =
    cabal2nix
      "typograffiti-sdl"
      "${typograffiti-src}/typograffiti-sdl"
      { inherit typograffiti-core typograffiti-freetype ; };


  the-mutant =
    cabal2nix
      "the-mutant"
      ./the-mutant
      {};


  the-mutant-sdl =
    cabal2nix
      "the-mutant-sdl"
      ./the-mutant-sdl
      { inherit the-mutant typograffiti-sdl ; };


  the-mutant-app =
    cabal2nix
      "the-mutant-app"
      ./the-mutant-app
      { inherit the-mutant; };


  the-mutant-app-sdl =
    cabal2nix
      "the-mutant-app-sdl"
      ./the-mutant-app-sdl
      { inherit the-mutant-app the-mutant-sdl; };


in { inherit the-mutant the-mutant-sdl the-mutant-app the-mutant-app-sdl; }
