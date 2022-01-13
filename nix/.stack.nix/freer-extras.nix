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
      identifier = { name = "freer-extras"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "tobias.pflug@iohk.io";
      author = "Tobias Pflug";
      homepage = "";
      url = "";
      synopsis = "Useful extensions to simple-freer";
      description = "freer-extras provides logging and monitoring functions extending simple-freer";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
          (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "freer-extras-test" = {
          depends = [
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
            (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
            (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
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
    postUnpack = "sourceRoot+=/freer-extras; echo source root reset to \$sourceRoot";
    }