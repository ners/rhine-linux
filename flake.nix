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
    dunai-transformers = {
      url = "github:ghc/packages-transformers";
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
      hpsFor = pkgs:
        lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages
        // { default = pkgs.haskell.packages.ghc94; };
      overlay = final: prev: lib.pipe prev [
        (prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeExtensions
              prev.haskell.packageOverrides
              (hfinal: hprev: with prev.haskell.lib.compose; {
                dunai = hfinal.callCabal2nix "dunai" "${inputs.dunai}/dunai" {
                  transformers = hprev.callCabal2nix "transformers" inputs.dunai-transformers { };
                };
                i3ipc = doJailbreak (markUnbroken hprev.i3ipc);
                rhine = doJailbreak (hfinal.callCabal2nix "rhine" "${inputs.rhine}/rhine" { });
                time-domain = doJailbreak hprev.time-domain;
                rhine-linux = final.buildEnv {
                  name = "rhine-linux-${replaceStrings ["-" "."] ["" ""] hfinal.ghc.name}";
                  paths = map (pname: hfinal.${pname}) pnames;
                };
              }
              // lib.genAttrs pnames (pname: hfinal.callCabal2nix pname (hsSrc ./${pname}) { }));
          };
          rhine-linux = final.buildEnv {
            name = "rhine-linux";
            paths = map (hp: hp.rhine-linux) (attrValues (lib.filterAttrs (ghc: _: elem ghc ghcs) final.haskell.packages));
            pathsToLink = [ "/lib" ];
            postBuild = ''
              ln -s ${(hpsFor final).default.rhine-linux}/bin $out/bin
            '';
            meta.mainProgram = "kitchen-sink";
          };
        })
      ];
    in
    {
      overlays.default = overlay;
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let pkgs = pkgs'.extend overlay; in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.rhine-linux;
          devShells.${system} =
            foreach (hpsFor pkgs) (ghcName: hp: {
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
      );
}
