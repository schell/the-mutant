{ mutantInstance ? "desktop" }:

let

  tarball =
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/42c0ce80e6bb1c27c233ba0ec1fd136bced7b0be.tar.gz";
    };


  ghcjsConfig = {};
  #  packageOverrides = pkgs: rec {
  #    haskell = pkgs.haskell // {
  #      packages = pkgs.haskell.packages // {
  #        ghcjs = pkgs.haskell.packages.ghcjs.override {
  #          overrides = self: super: rec {
  #            # overrides here
  #          };
  #        };
  #      };
  #    };
  #  };
  #};


  pkgs =
    import
      tarball
      (if mutantInstance == "browser"
       then { config = ghcjsConfig; }
       else {}
      );


  compiler =
    if mutantInstance == "browser"
    then "ghcjs84"
    else "ghc865";


  compilerPackages =
    pkgs.haskell.packages.${compiler};

in
  pkgs // {
    inherit compilerPackages;
  }
