{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
    dunai = {
      url = "github:ivanperez-keera/dunai";
      flake = false;
    };
    rhine = {
      url = "github:ners/rhine/dunai-0.12";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = root: inputs.nix-filter {
        inherit root;
        include = with inputs.nix-filter.lib; [
          (matchExt "cabal")
          (matchExt "hs")
          (matchExt "md")
          isDirectory
        ];
      };
      dropWhile = p: xs:
        if xs == [ ] then [ ] else
        let x = head xs; s = tail xs; in
        if p x then dropWhile p s
        else xs;
      takeWhile = p: xs:
        if xs == [ ] then [ ] else
        let x = head xs; s = tail xs; in
        if p x then [ x ] ++ takeWhile p s
        else [ ];
      pnames = with lib; pipe ./cabal.project [
        readFile
        (splitString "\n")
        (dropWhile (line: line != "packages:"))
        tail
        (takeWhile (hasPrefix "    "))
        (map (removePrefix "    "))
        (map (removePrefix "./"))
        (map (removeSuffix "/"))
      ];
      ghcs = [ "ghc94" "ghc96" "ghc98" ];
      overlay = final: prev: lib.pipe prev [
        (prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeExtensions
              prev.haskell.packageOverrides
              (hfinal: hprev: with prev.haskell.lib.compose; {
                dunai = hfinal.callCabal2nix "dunai" "${inputs.dunai}/dunai" { };
                rhine = doJailbreak (hfinal.callCabal2nix "rhine" "${inputs.rhine}/rhine" { });
                i3ipc = doJailbreak (markUnbroken hprev.i3ipc);
              } // lib.genAttrs pnames (pname: hfinal.callCabal2nix pname (hsSrc ./${pname}) { }));
          };
        })
      ];
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps =
            lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages
            // { default = pkgs.haskellPackages; };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.buildEnv {
            name = "rhine-linux";
            paths = map (pname: pkgs.haskellPackages.${pname}) pnames;
          };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: map (pname: ps.${pname}) pnames;
                nativeBuildInputs = with hp; [
                  cabal-install
                  fourmolu
                  haskell-language-server
                ];
              };
            });
        }
      ) // {
      overlays.default = overlay;
    };
}
