{ compiler ? "ghc865" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      hush =
        hself.callCabal2nix
          "hush"
          (./.)
          {};
    };
  };
  
shell = myHaskellPackages.shellFor {
    packages = p: [
      p.hush
    ];

    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      ormolu
      hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = false;

    shellHook = ''
      set -e
      hpack
      set +e
    '';
};

in
{
  inherit shell;
  hush = myHaskellPackages.hush;
}
