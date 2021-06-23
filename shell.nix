{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, gloss, gloss-juicy, HUnit
      , JuicyPixels, lens, mtl, stdenv, time
      }:
      mkDerivation {
        pname = "queens-gadt";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers gloss gloss-juicy JuicyPixels lens mtl
        ];
        executableHaskellDepends = [
          base containers gloss gloss-juicy JuicyPixels lens mtl
        ];
        testHaskellDepends = [
          base containers gloss gloss-juicy HUnit JuicyPixels lens mtl time
        ];
        homepage = "https://github.com/shawn-bachlet/queens-gadt#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
