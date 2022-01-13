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
    flags = { use-ghc-stub = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "plutus-tx-plugin"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones";
      homepage = "";
      url = "";
      synopsis = "The Plutus Tx compiler and GHC plugin";
      description = "The Plutus Tx compiler and GHC plugin.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          ] ++ (if flags.use-ghc-stub
          then [
            (hsPkgs."plutus-ghc-stub" or (errorHandler.buildDepError "plutus-ghc-stub"))
            ]
          else [ (hsPkgs."ghc" or (errorHandler.buildDepError "ghc")) ]);
        buildable = true;
        };
      tests = {
        "plutus-tx-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ];
          buildable = if flags.use-ghc-stub then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/plutus";
      rev = "2721c59fd2302b75c4138456c29fd5b509e8340a";
      sha256 = "02g8pzldyfl4pm8sy22yd3l2fr3zpyhwkvv9x3h9lsf6lfx5wi7k";
      }) // {
      url = "https://github.com/input-output-hk/plutus";
      rev = "2721c59fd2302b75c4138456c29fd5b509e8340a";
      sha256 = "02g8pzldyfl4pm8sy22yd3l2fr3zpyhwkvv9x3h9lsf6lfx5wi7k";
      };
    postUnpack = "sourceRoot+=/plutus-tx-plugin; echo source root reset to \$sourceRoot";
    }