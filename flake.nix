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
      url = "github:ners/rhine/never";
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
      pnames = with lib; pipe ./. [
        readDir
        (filterAttrs (name: type: type == "directory" && readDir ./${name} ? "${name}.cabal"))
        attrNames
      ];
      ghcs = [ "ghc92" "ghc94" ];
      overlay = final: prev: lib.pipe prev [
        (prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeExtensions
              prev.haskell.packageOverrides
              (hfinal: hprev: with prev.haskell.lib.compose; {
                dunai = hfinal.callCabal2nix "dunai" "${inputs.dunai}/dunai" { };
                rhine = doJailbreak (hfinal.callCabal2nix "rhine" "${inputs.rhine}/rhine" { });
                i3ipc = doJailbreak (markUnbroken hprev.i3ipc);
                rhine-linux = final.buildEnv {
                  name = "rhine-linux-${replaceStrings ["-" "."] ["" ""] hfinal.ghc.name}";
                  paths = map (pname: hfinal.${pname}) pnames;
                };
              } // lib.genAttrs pnames (pname: hfinal.callCabal2nix pname (hsSrc ./${pname}) { }));
          };
          rhine-linux = final.buildEnv {
            name = "rhine-linux";
            paths = map (hp: hp.rhine-linux) (attrValues (lib.filterAttrs (ghc: _: elem ghc ghcs) final.haskell.packages));
            pathsToLink = ["/lib"];
            postBuild = ''
              ln -s ${final.haskellPackages.rhine-linux}/bin $out/bin
            '';
            meta.mainProgram = "kitchen-sink";
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
          packages.${system}.default = pkgs.rhine-linux;
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
