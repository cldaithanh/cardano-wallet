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
      specVersion = "2.2";
      identifier = { name = "plutus-chain-index"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "sjoerd.visscher@iohk.io";
      author = "Sjoerd Visscher";
      homepage = "https://github.com/iohk/plutus#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/input-output-hk/plutus#readme>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-chain-index-core" or (errorHandler.buildDepError "plutus-chain-index-core"))
          (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
          (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          ];
        buildable = true;
        };
      exes = {
        "plutus-chain-index" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-chain-index" or (errorHandler.buildDepError "plutus-chain-index"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/plutus-apps";
      rev = "6f843ab60709f01505c98d8c0ee03f7afedf10df";
      sha256 = "1bi8csyg34g9zrjf4wr05rxbgm7jyqj79g01n6lrj73pwzhavqx7";
      }) // {
      url = "https://github.com/input-output-hk/plutus-apps";
      rev = "6f843ab60709f01505c98d8c0ee03f7afedf10df";
      sha256 = "1bi8csyg34g9zrjf4wr05rxbgm7jyqj79g01n6lrj73pwzhavqx7";
      };
    postUnpack = "sourceRoot+=/plutus-chain-index; echo source root reset to \$sourceRoot";
    }