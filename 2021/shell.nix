{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, extra, HUnit, lib, parsec
      , pqueue, QuickCheck, relude, vector
      }:
      mkDerivation {
        pname = "x2021";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array base extra parsec pqueue relude vector
        ];
        executableHaskellDepends = [
          array base extra parsec pqueue relude vector
        ];
        testHaskellDepends = [
          array base extra HUnit parsec pqueue QuickCheck relude vector
        ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
