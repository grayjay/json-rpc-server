let
  inherit (builtins) listToAttrs;
  inherit (pkgs.lib) nameValuePair;
  pkgs = import <nixpkgs> {};
  extraPkgs = import <haskellPackages>;
  combos = [
    { ghcV = "704"; aesonV = "0_6_0_0"; }
    { ghcV = "742"; aesonV = "0_7_0_4"; }
    { ghcV = "763"; aesonV = "0_7_0_4"; }
    { ghcV = "783"; aesonV = "0_8_0_2"; }
    # { ghcV = "HEAD"; aesonV = "0_8_0_2"; }
  ];
  toLabel = { ghcV, aesonV }: "ghc${ghcV}-aeson_${aesonV}";
  build = { ghcV, aesonV }:
    let
      hp = pkgs.lib.getAttrFromPath ["haskellPackages_ghc${ghcV}"] pkgs;
      aeson = hp.callPackage (pkgs.lib.getAttrFromPath ["aeson_${aesonV}"] extraPkgs) {};
      inherit (hp) cabal HUnit mtl testFramework testFrameworkHunit text unorderedContainers vector;
    in cabal.mkDerivation (self: {
      pname = "json-rpc-server";
      version = "0.1.3.0";
      isLibrary = true;
      src = <jsonRpcServerSrc>;
      buildDepends = [ aeson mtl text unorderedContainers vector ];
      testDepends = [
        aeson HUnit mtl testFramework testFrameworkHunit text
        unorderedContainers vector
      ];
      meta = {
        description = "JSON RPC 2.0 on the server side.";
        license = self.stdenv.lib.licenses.mit;
        platforms = self.ghc.meta.platforms;
      };
    });
in {
  jsonRpcServer = let pair = versions : nameValuePair (toLabel versions) (build versions);
                  in listToAttrs (map pair combos);
}
