{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, bimap, containers, extra, HUnit
      , lib, mod, mtl, parsec, pqueue, QuickCheck, relude, text, vector
      }:
      mkDerivation {
        pname = "x2021";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array base bimap containers extra mod mtl parsec pqueue relude text
          vector
        ];
        executableHaskellDepends = [
          array base bimap containers extra mod mtl parsec pqueue relude text
          vector
        ];
        testHaskellDepends = [
          array base bimap containers extra HUnit mod mtl parsec pqueue
          QuickCheck relude text vector
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
