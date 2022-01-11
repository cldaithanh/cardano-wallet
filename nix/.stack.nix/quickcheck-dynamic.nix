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
      identifier = { name = "quickcheck-dynamic"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "ulf.norell@quviq.com";
      author = "Ulf Norell";
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
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          ];
        buildable = true;
        };
      tests = {
        "quickcheck-dynamic-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."quickcheck-dynamic" or (errorHandler.buildDepError "quickcheck-dynamic"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
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
    postUnpack = "sourceRoot+=/quickcheck-dynamic; echo source root reset to \$sourceRoot";
    }