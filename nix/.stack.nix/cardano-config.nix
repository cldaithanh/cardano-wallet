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
    flags = { systemd = true; };
    package = {
      specVersion = "3.0";
      identifier = { name = "cardano-config"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "45cc256de80551c1df4e6cc3eab0d55143b9b1f2";
      sha256 = "1alfg2v6b8k79lk0flz31mlg6hqb800vcr7n04yw5f0j7jw56npc";
      }) // {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "45cc256de80551c1df4e6cc3eab0d55143b9b1f2";
      sha256 = "1alfg2v6b8k79lk0flz31mlg6hqb800vcr7n04yw5f0j7jw56npc";
      };
    postUnpack = "sourceRoot+=/cardano-config; echo source root reset to \$sourceRoot";
    }