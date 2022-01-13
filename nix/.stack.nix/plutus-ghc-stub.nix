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
      specVersion = "1.10";
      identifier = { name = "plutus-ghc-stub"; version = "8.6.5"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "glasgow-haskell-users@haskell.org";
      author = "The GHC Team";
      homepage = "http://www.haskell.org/ghc/";
      url = "";
      synopsis = "The GHC API";
      description = "Stub functionality for the Plutus plugin, for cross compilers that\ndon't have a GHC library installed, like GHCJS\nThis should contain all the types and functions that the Plutus\ncompiler uses.\nFor technical reasons (Cabal), we need to be able to compile the plutus-tx\ncompiler for the host platform, even if we are going to load the plugin\nfrom the build platform libraries.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
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
    postUnpack = "sourceRoot+=/stubs/plutus-ghc-stub; echo source root reset to \$sourceRoot";
    }