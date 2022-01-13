{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = { name = "plutus-ledger-constraints"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "sjoerd.visscher@iohk.io";
      author = "Michael Peyton Jones, Jann Mueller";
      homepage = "";
      url = "";
      synopsis = "Ledger Constraints";
      description = "Plutus transaction constraints library";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "plutus-ledger-constraints-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-ledger-constraints" or (errorHandler.buildDepError "plutus-ledger-constraints"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/plutus-apps";
      rev = "10b4bc5df56e99c960e3a5a6701424b195f9dbac";
      sha256 = "198pyl1ayzpivr0pgp57fdbwfd3p99rxgy5rm9nh8wzx7sb9dbcv";
      }) // {
      url = "https://github.com/input-output-hk/plutus-apps";
      rev = "10b4bc5df56e99c960e3a5a6701424b195f9dbac";
      sha256 = "198pyl1ayzpivr0pgp57fdbwfd3p99rxgy5rm9nh8wzx7sb9dbcv";
      };
    postUnpack = "sourceRoot+=/plutus-ledger-constraints; echo source root reset to \$sourceRoot";
    }