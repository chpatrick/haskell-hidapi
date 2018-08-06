# default.nix
{ pkgs ? import <nixpkgs> {} }:

let
  baseDrv = pkgs.haskellPackages.callCabal2nix "hidapi" ./. {};
  drv = pkgs.haskell.lib.overrideCabal baseDrv (old: {
    librarySystemDepends = old.librarySystemDepends ++ [ pkgs.udev pkgs.udev.lib ];
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.udev.lib}/lib:$LD_LIBRARY_PATH
    '';
  });
in if pkgs.lib.inNixShell then drv.env else drv