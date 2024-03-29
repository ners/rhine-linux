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
      url = "github:turion/rhine";
      flake = false;
    };
    ioctl = {
      url = "github:ners/ioctl";
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
      readDirs = root: attrNames (lib.filterAttrs (_: type: type == "directory") (readDir root));
      readFiles = root: attrNames (lib.filterAttrs (_: type: type == "regular") (readDir root));
      basename = path: suffix: with lib; pipe path [
        (splitString "/")
        last
        (removeSuffix suffix)
      ];
      cabalProjectPackages = root: with lib; foreach (readDirs root) (dir:
        let
          path = "${root}/${dir}";
          files = readFiles path;
          cabalFiles = filter (strings.hasSuffix ".cabal") files;
          pnames = map (path: basename path ".cabal") cabalFiles;
          pname = if pnames == [ ] then null else head pnames;
        in
        optionalAttrs (pname != null) { ${pname} = path; }
      );
      cabalProjectPnames = root: lib.attrNames (cabalProjectPackages root);
      cabalProjectOverlay = root: hfinal: hprev: with lib;
        mapAttrs
          (pname: path: hfinal.callCabal2nix pname path { })
          (cabalProjectPackages root);
      project = hsSrc ./.;
      pnames = cabalProjectPnames project;
      ghcs = [ "ghc92" "ghc94" ];
      hpsFor = pkgs:
        lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages
        // { default = pkgs.haskell.packages.ghc94; };
      overlay = final: prev: lib.pipe prev [
        (prev: {
          haskell = prev.haskell // {
            packageOverrides = with prev.haskell.lib.compose; lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (cabalProjectOverlay project)
              (cabalProjectOverlay inputs.dunai)
              (cabalProjectOverlay inputs.rhine)
              (hfinal: hprev: {
                dunai = hfinal.callCabal2nix "dunai" "${inputs.dunai}/dunai" {
                  transformers = hprev.callCabal2nix "transformers" inputs.dunai-transformers { };
                };
                bindings-libv4l2 = lib.pipe hprev.bindings-libv4l2 [
                  markUnbroken
                  doJailbreak
                  (addPkgconfigDepend prev.libv4l.dev)
                ];
                i3ipc = doJailbreak (markUnbroken hprev.i3ipc);
                ioctl = hfinal.callCabal2nix "ioctl" inputs.ioctl { };
                rhine-linux = final.buildEnv {
                  name = "rhine-linux-${replaceStrings ["-" "."] ["" ""] hfinal.ghc.name}";
                  paths = map (pname: hfinal.${pname}) pnames;
                };
                time-domain = doJailbreak hprev.time-domain;
                v4l2 = doJailbreak hprev.v4l2;
              })
            ];
          };
          rhine-linux = final.buildEnv {
            name = "rhine-linux";
            paths = lib.pipe final.haskell.packages [
              (lib.filterAttrs (ghc: _: elem ghc ghcs))
              attrValues
              (map (hp: hp.rhine-linux))
            ];
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
                  pkgs'.haskellPackages.cabal-install
                  fourmolu
                  haskell-language-server
                ];
              };
            });
        }
      );
}
