{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rhine = {
      url = "github:turion/rhine";
      inputs.nixpkgs.follows = "nixpkgs";
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
      hsSrc = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ]) ./.;
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
      pnames = filter (pname: pname != "kitchen-sink") (cabalProjectPnames project);
      ghcsFor = pkgs: with lib; foldlAttrs
        (acc: name: hp:
          let
            version = getVersion hp.ghc;
            majorMinor = versions.majorMinor version;
            ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
          in
          if hp ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.6" && versionOlder version "9.10"
          then acc // { ${ghcName} = hp; }
          else acc
        )
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      overlay = lib.composeManyExtensions [
        inputs.rhine.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = with prev.haskell.lib.compose; lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (cabalProjectOverlay project)
              (hfinal: hprev: {
                bindings-libv4l2 = lib.pipe hprev.bindings-libv4l2 [
                  markUnbroken
                  doJailbreak
                  (addPkgconfigDepend prev.libv4l.dev)
                ];
                i3ipc = doJailbreak (markUnbroken hprev.i3ipc);
                ioctl = hfinal.callCabal2nix "ioctl" inputs.ioctl { };
                v4l2 = doJailbreak hprev.v4l2;
              })
            ];
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
        let
          pkgs = pkgs'.extend overlay;
          hps = hpsFor pkgs;
          bin = pkgs.haskellPackages.callCabal2nix "kitchen-sink" ./kitchen-sink { };
          name = "rhine-linux";
          libs = pkgs.buildEnv {
            name = "${name}-libs";
            paths = lib.mapCartesianProduct
              ({ hp, pname }: hp.${pname})
              { hp = attrValues hps; pname = pnames; };
            pathsToLink = [ "/lib" ];
          };
          docs = pkgs.buildEnv {
            name = "${name}-docs";
            paths = map (pname: pkgs.haskell.lib.documentationTarball hps.default.${pname}) pnames;
          };
          sdist = pkgs.buildEnv {
            name = "${name}-sdist";
            paths = map (pname: pkgs.haskell.lib.sdistTarball hps.default.${pname}) pnames;
          };
          docsAndSdist = pkgs.linkFarm "${name}-docsAndSdist" { inherit docs sdist; };
          all = pkgs.symlinkJoin {
            name = "${name}-all";
            paths = [ bin libs docsAndSdist ];
          };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = all;
          devShells.${system} = foreach hps (ghcName: hp: {
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
