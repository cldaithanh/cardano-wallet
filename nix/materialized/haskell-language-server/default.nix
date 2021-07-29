{
  pkgs = hackage:
    {
      packages = {
        "butcher".revision = (((hackage."butcher")."1.3.3.2").revisions).default;
        "these".revision = (((hackage."these")."1.1.1.1").revisions).default;
        "these".flags.assoc = true;
        "lsp".revision = (((hackage."lsp")."1.2.0.0").revisions).default;
        "lsp".flags.demo = false;
        "hlint".revision = (((hackage."hlint")."3.2.7").revisions).default;
        "hlint".flags.threaded = true;
        "hlint".flags.ghc-lib = false;
        "hlint".flags.gpl = true;
        "hlint".flags.hsyaml = false;
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "dependent-sum".revision = (((hackage."dependent-sum")."0.7.1.0").revisions).default;
        "heaps".revision = (((hackage."heaps")."0.4").revisions).default;
        "ghc-boot".revision = (((hackage."ghc-boot")."8.10.5").revisions).default;
        "data-tree-print".revision = (((hackage."data-tree-print")."0.1.0.2").revisions).default;
        "rope-utf16-splay".revision = (((hackage."rope-utf16-splay")."0.3.2.0").revisions).default;
        "streaming-commons".revision = (((hackage."streaming-commons")."0.2.2.1").revisions).default;
        "streaming-commons".flags.use-bytestring-builder = false;
        "hslogger".revision = (((hackage."hslogger")."1.3.1.0").revisions).default;
        "hslogger".flags.network--gt-3_0_0 = true;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.11").revisions).default;
        "bifunctors".flags.tagged = true;
        "bifunctors".flags.semigroups = true;
        "lucid".revision = (((hackage."lucid")."2.9.12.1").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "regex-tdfa".revision = (((hackage."regex-tdfa")."1.3.1.1").revisions).default;
        "regex-tdfa".flags.force-o2 = false;
        "monoid-subclasses".revision = (((hackage."monoid-subclasses")."1.1").revisions).default;
        "hashtables".revision = (((hackage."hashtables")."1.2.4.1").revisions).default;
        "hashtables".flags.portable = false;
        "hashtables".flags.detailed-profiling = false;
        "hashtables".flags.unsafe-tricks = true;
        "hashtables".flags.bounds-checking = false;
        "hashtables".flags.debug = false;
        "hashtables".flags.sse42 = false;
        "cryptohash-sha1".revision = (((hackage."cryptohash-sha1")."0.11.100.1").revisions).default;
        "refact".revision = (((hackage."refact")."0.3.0.2").revisions).default;
        "ghc-check".revision = (((hackage."ghc-check")."0.5.0.5").revisions).default;
        "ghc-check".flags.ghc-check-use-package-abis = true;
        "monad-dijkstra".revision = (((hackage."monad-dijkstra")."0.1.1.3").revisions).default;
        "lsp-test".revision = (((hackage."lsp-test")."0.14.0.0").revisions).default;
        "Only".revision = (((hackage."Only")."0.1").revisions).default;
        "blaze-builder".revision = (((hackage."blaze-builder")."0.4.2.1").revisions).default;
        "void".revision = (((hackage."void")."0.7.3").revisions).default;
        "void".flags.safe = false;
        "parsers".revision = (((hackage."parsers")."0.12.10").revisions).default;
        "parsers".flags.binary = true;
        "parsers".flags.parsec = true;
        "parsers".flags.attoparsec = true;
        "base-compat".revision = (((hackage."base-compat")."0.11.2").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11").revisions).default;
        "ansi-terminal".flags.example = false;
        "js-jquery".revision = (((hackage."js-jquery")."3.3.1").revisions).default;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.2.0.1").revisions).default;
        "strict-list".revision = (((hackage."strict-list")."0.1.5").revisions).default;
        "free".revision = (((hackage."free")."5.1.7").revisions).default;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "sorted-list".revision = (((hackage."sorted-list")."0.2.1.0").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.6").revisions).default;
        "time-compat".flags.old-locale = false;
        "yaml".revision = (((hackage."yaml")."0.11.5.0").revisions).default;
        "yaml".flags.no-exe = true;
        "yaml".flags.no-examples = true;
        "heapsize".revision = (((hackage."heapsize")."0.3.0.1").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "hspec-expectations".revision = (((hackage."hspec-expectations")."0.8.2").revisions).default;
        "hls-hlint-plugin".revision = (((hackage."hls-hlint-plugin")."1.0.1.0").revisions).default;
        "hls-hlint-plugin".flags.hlint33 = false;
        "hls-hlint-plugin".flags.ghc-lib = false;
        "hls-hlint-plugin".flags.pedantic = false;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        "psqueues".revision = (((hackage."psqueues")."0.2.7.2").revisions).default;
        "mono-traversable".revision = (((hackage."mono-traversable")."1.0.15.1").revisions).default;
        "conduit-extra".revision = (((hackage."conduit-extra")."1.3.5").revisions).default;
        "call-stack".revision = (((hackage."call-stack")."0.4.0").revisions).default;
        "bytestring-encoding".revision = (((hackage."bytestring-encoding")."0.1.1.0").revisions).default;
        "safe".revision = (((hackage."safe")."0.3.19").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.7").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "vector".revision = (((hackage."vector")."0.12.3.0").revisions).default;
        "vector".flags.unsafechecks = false;
        "vector".flags.internalchecks = false;
        "vector".flags.boundschecks = true;
        "vector".flags.wall = false;
        "data-default-instances-old-locale".revision = (((hackage."data-default-instances-old-locale")."0.0.1").revisions).default;
        "conduit".revision = (((hackage."conduit")."1.3.4.1").revisions).default;
        "hls-plugin-api".revision = (((hackage."hls-plugin-api")."1.1.0.2").revisions).default;
        "hls-plugin-api".flags.pedantic = false;
        "constraints".revision = (((hackage."constraints")."0.13").revisions).default;
        "algebraic-graphs".revision = (((hackage."algebraic-graphs")."0.5").revisions).default;
        "hls-refine-imports-plugin".revision = (((hackage."hls-refine-imports-plugin")."1.0.0.0").revisions).default;
        "network".revision = (((hackage."network")."3.1.2.2").revisions).default;
        "network".flags.devel = false;
        "HUnit".revision = (((hackage."HUnit")."1.6.2.0").revisions).default;
        "ormolu".revision = (((hackage."ormolu")."0.1.4.1").revisions).default;
        "ormolu".flags.dev = false;
        "Glob".revision = (((hackage."Glob")."0.10.1").revisions).default;
        "implicit-hie-cradle".revision = (((hackage."implicit-hie-cradle")."0.3.0.3").revisions).default;
        "apply-refact".revision = (((hackage."apply-refact")."0.9.3.0").revisions).default;
        "conduit-parse".revision = (((hackage."conduit-parse")."0.2.1.0").revisions).default;
        "conduit-parse".flags.enable-hlint-test = false;
        "direct-sqlite".revision = (((hackage."direct-sqlite")."2.3.26").revisions).default;
        "direct-sqlite".flags.urifilenames = true;
        "direct-sqlite".flags.haveusleep = true;
        "direct-sqlite".flags.json1 = true;
        "direct-sqlite".flags.fulltextsearch = true;
        "direct-sqlite".flags.systemlib = false;
        "hls-floskell-plugin".revision = (((hackage."hls-floskell-plugin")."1.0.0.0").revisions).default;
        "hls-graph".revision = (((hackage."hls-graph")."1.4.0.0").revisions).default;
        "hls-graph".flags.pedantic = false;
        "monad-memo".revision = (((hackage."monad-memo")."0.5.3").revisions).default;
        "dlist".revision = (((hackage."dlist")."1.0").revisions).default;
        "dlist".flags.werror = false;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "async".revision = (((hackage."async")."2.2.3").revisions).default;
        "async".flags.bench = false;
        "hls-module-name-plugin".revision = (((hackage."hls-module-name-plugin")."1.0.0.0").revisions).default;
        "process".revision = (((hackage."process")."1.6.9.0").revisions).default;
        "monad-control".revision = (((hackage."monad-control")."1.0.2.3").revisions).default;
        "random".revision = (((hackage."random")."1.2.0").revisions).default;
        "unix-compat".revision = (((hackage."unix-compat")."0.5.3").revisions).default;
        "unix-compat".flags.old-time = false;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.15.1.0").revisions).default;
        "hspec-discover".revision = (((hackage."hspec-discover")."2.7.10").revisions).default;
        "parallel".revision = (((hackage."parallel")."3.2.2.0").revisions).default;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.2").revisions).default;
        "QuickCheck".flags.old-random = false;
        "QuickCheck".flags.templatehaskell = true;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.5").revisions).default;
        "scientific".revision = (((hackage."scientific")."0.3.7.0").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "hscolour".revision = (((hackage."hscolour")."1.24.4").revisions).default;
        "entropy".revision = (((hackage."entropy")."0.4.1.6").revisions).default;
        "entropy".flags.halvm = false;
        "hpc".revision = (((hackage."hpc")."0.6.1.0").revisions).default;
        "hls-retrie-plugin".revision = (((hackage."hls-retrie-plugin")."1.0.1.0").revisions).default;
        "hls-explicit-imports-plugin".revision = (((hackage."hls-explicit-imports-plugin")."1.0.0.3").revisions).default;
        "hls-fourmolu-plugin".revision = (((hackage."hls-fourmolu-plugin")."1.0.0.1").revisions).default;
        "prettyprinter".revision = (((hackage."prettyprinter")."1.7.0").revisions).default;
        "prettyprinter".flags.buildreadme = false;
        "th-extras".revision = (((hackage."th-extras")."0.0.0.4").revisions).default;
        "th-extras".flags.base4 = true;
        "quickcheck-io".revision = (((hackage."quickcheck-io")."0.2.0").revisions).default;
        "alex".revision = (((hackage."alex")."3.2.6").revisions).default;
        "alex".flags.small_base = true;
        "tasty-hunit".revision = (((hackage."tasty-hunit")."0.10.0.3").revisions).default;
        "hls-class-plugin".revision = (((hackage."hls-class-plugin")."1.0.0.2").revisions).default;
        "hls-eval-plugin".revision = (((hackage."hls-eval-plugin")."1.1.1.0").revisions).default;
        "hls-eval-plugin".flags.pedantic = false;
        "githash".revision = (((hackage."githash")."0.1.6.1").revisions).default;
        "megaparsec".revision = (((hackage."megaparsec")."9.0.1").revisions).default;
        "megaparsec".flags.dev = false;
        "primes".revision = (((hackage."primes")."0.2.1.0").revisions).default;
        "distributive".revision = (((hackage."distributive")."0.6.2.1").revisions).default;
        "distributive".flags.tagged = true;
        "distributive".flags.semigroups = true;
        "transformers-base".revision = (((hackage."transformers-base")."0.4.5.2").revisions).default;
        "transformers-base".flags.orphaninstances = true;
        "base16-bytestring".revision = (((hackage."base16-bytestring")."1.0.1.0").revisions).default;
        "tasty-rerun".revision = (((hackage."tasty-rerun")."1.1.18").revisions).default;
        "data-default-instances-containers".revision = (((hackage."data-default-instances-containers")."0.0.1").revisions).default;
        "cryptohash-md5".revision = (((hackage."cryptohash-md5")."0.11.100.1").revisions).default;
        "cpphs".revision = (((hackage."cpphs")."1.20.9.1").revisions).default;
        "cpphs".flags.old-locale = false;
        "vector-algorithms".revision = (((hackage."vector-algorithms")."0.8.0.4").revisions).default;
        "vector-algorithms".flags.unsafechecks = false;
        "vector-algorithms".flags.llvm = false;
        "vector-algorithms".flags.internalchecks = false;
        "vector-algorithms".flags.bench = true;
        "vector-algorithms".flags.boundschecks = true;
        "vector-algorithms".flags.properties = true;
        "happy".revision = (((hackage."happy")."1.20.0").revisions).default;
        "base".revision = (((hackage."base")."4.14.2.0").revisions).default;
        "blaze-markup".revision = (((hackage."blaze-markup")."0.8.2.8").revisions).default;
        "network-uri".revision = (((hackage."network-uri")."2.6.4.1").revisions).default;
        "foldl".revision = (((hackage."foldl")."1.4.11").revisions).default;
        "Diff".revision = (((hackage."Diff")."0.4.0").revisions).default;
        "cmdargs".revision = (((hackage."cmdargs")."0.10.21").revisions).default;
        "cmdargs".flags.testprog = false;
        "cmdargs".flags.quotation = true;
        "optparse-simple".revision = (((hackage."optparse-simple")."0.1.1.4").revisions).default;
        "optparse-simple".flags.build-example = false;
        "rts".revision = (((hackage."rts")."1.0.1").revisions).default;
        "hie-compat".revision = (((hackage."hie-compat")."0.2.0.0").revisions).default;
        "hie-compat".flags.ghc-lib = false;
        "text".revision = (((hackage."text")."1.2.4.1").revisions).default;
        "aeson-pretty".revision = (((hackage."aeson-pretty")."0.8.8").revisions).default;
        "aeson-pretty".flags.lib-only = false;
        "tasty-golden".revision = (((hackage."tasty-golden")."2.3.4").revisions).default;
        "tasty-golden".flags.build-example = false;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "js-flot".revision = (((hackage."js-flot")."0.8.3").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "hls-haddock-comments-plugin".revision = (((hackage."hls-haddock-comments-plugin")."1.0.0.2").revisions).default;
        "uuid".revision = (((hackage."uuid")."1.3.15").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.14.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "data-default-class".revision = (((hackage."data-default-class")."0.1.2.0").revisions).default;
        "tasty-expected-failure".revision = (((hackage."tasty-expected-failure")."0.12.3").revisions).default;
        "parser-combinators".revision = (((hackage."parser-combinators")."1.3.0").revisions).default;
        "parser-combinators".flags.dev = false;
        "pretty-simple".revision = (((hackage."pretty-simple")."4.0.0.0").revisions).default;
        "pretty-simple".flags.buildexe = false;
        "pretty-simple".flags.buildexample = false;
        "fuzzy".revision = (((hackage."fuzzy")."0.1.0.0").revisions).default;
        "charset".revision = (((hackage."charset")."0.3.8").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "dependent-sum-template".revision = (((hackage."dependent-sum-template")."0.1.0.3").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.3.1").revisions).default;
        "typed-process".revision = (((hackage."typed-process")."0.2.6.0").revisions).default;
        "czipwith".revision = (((hackage."czipwith")."1.0.1.3").revisions).default;
        "logict".revision = (((hackage."logict")."0.7.0.3").revisions).default;
        "contravariant".revision = (((hackage."contravariant")."1.5.3").revisions).default;
        "contravariant".flags.tagged = true;
        "contravariant".flags.semigroups = true;
        "contravariant".flags.statevar = true;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "constraints-extras".revision = (((hackage."constraints-extras")."0.3.1.0").revisions).default;
        "constraints-extras".flags.build-readme = true;
        "unsafe".revision = (((hackage."unsafe")."0.0").revisions).default;
        "unliftio".revision = (((hackage."unliftio")."0.2.18").revisions).default;
        "wcwidth".revision = (((hackage."wcwidth")."0.0.2").revisions).default;
        "wcwidth".flags.split-base = true;
        "wcwidth".flags.cli = false;
        "microlens-th".revision = (((hackage."microlens-th")."0.4.3.10").revisions).default;
        "fingertree".revision = (((hackage."fingertree")."0.1.4.2").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.12.0").revisions).default;
        "hls-stylish-haskell-plugin".revision = (((hackage."hls-stylish-haskell-plugin")."1.0.0.1").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "reflection".revision = (((hackage."reflection")."2.1.6").revisions).default;
        "reflection".flags.slow = false;
        "reflection".flags.template-haskell = true;
        "hyphenation".revision = (((hackage."hyphenation")."0.8.2").revisions).default;
        "hyphenation".flags.embed = true;
        "polyparse".revision = (((hackage."polyparse")."1.13").revisions).default;
        "mwc-random".revision = (((hackage."mwc-random")."0.15.0.1").revisions).default;
        "ghc-paths".revision = (((hackage."ghc-paths")."0.1.0.12").revisions).default;
        "brittany".revision = (((hackage."brittany")."0.13.1.2").revisions).default;
        "brittany".flags.brittany-dev-lib = false;
        "brittany".flags.brittany-test-perf = false;
        "setenv".revision = (((hackage."setenv")."0.1.1.3").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3.1").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "utf8-string".revision = (((hackage."utf8-string")."1.0.2").revisions).default;
        "ghc".revision = (((hackage."ghc")."8.10.5").revisions).default;
        "lens".revision = (((hackage."lens")."5.0.1").revisions).default;
        "lens".flags.test-templates = true;
        "lens".flags.test-hunit = true;
        "lens".flags.benchmark-uniplate = false;
        "lens".flags.inlining = true;
        "lens".flags.trustworthy = true;
        "lens".flags.test-properties = true;
        "lens".flags.dump-splices = false;
        "lens".flags.j = false;
        "containers".revision = (((hackage."containers")."0.6.4.1").revisions).default;
        "semigroupoids".revision = (((hackage."semigroupoids")."5.3.5").revisions).default;
        "semigroupoids".flags.distributive = true;
        "semigroupoids".flags.comonad = true;
        "semigroupoids".flags.contravariant = true;
        "semigroupoids".flags.containers = true;
        "semigroupoids".flags.tagged = true;
        "semigroupoids".flags.unordered-containers = true;
        "haddock-library".revision = (((hackage."haddock-library")."1.10.0").revisions).default;
        "tf-random".revision = (((hackage."tf-random")."0.5").revisions).default;
        "math-functions".revision = (((hackage."math-functions")."0.3.4.2").revisions).default;
        "math-functions".flags.system-expm1 = true;
        "math-functions".flags.system-erf = true;
        "ghcide".revision = (((hackage."ghcide")."1.4.0.2").revisions).default;
        "ghcide".flags.ghc-patched-unboxed-bytecode = false;
        "some".revision = (((hackage."some")."1.0.2").revisions).default;
        "some".flags.newtype-unsafe = true;
        "shake".revision = (((hackage."shake")."0.19.4").revisions).default;
        "shake".flags.cloud = false;
        "shake".flags.portable = false;
        "shake".flags.embed-files = false;
        "tagged".revision = (((hackage."tagged")."0.8.6.1").revisions).default;
        "tagged".flags.deepseq = true;
        "tagged".flags.transformers = true;
        "ghc-lib-parser".revision = (((hackage."ghc-lib-parser")."8.10.5.20210606").revisions).default;
        "haskell-src-exts".revision = (((hackage."haskell-src-exts")."1.23.1").revisions).default;
        "ghc-lib-parser-ex".revision = (((hackage."ghc-lib-parser-ex")."8.10.0.21").revisions).default;
        "ghc-lib-parser-ex".flags.auto = true;
        "ghc-lib-parser-ex".flags.no-ghc-lib = false;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.4").revisions).default;
        "prettyprinter-ansi-terminal".revision = (((hackage."prettyprinter-ansi-terminal")."1.1.2").revisions).default;
        "primitive".revision = (((hackage."primitive")."0.7.1.0").revisions).default;
        "hls-brittany-plugin".revision = (((hackage."hls-brittany-plugin")."1.0.0.1").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "HsYAML-aeson".revision = (((hackage."HsYAML-aeson")."0.2.0.0").revisions).default;
        "random-shuffle".revision = (((hackage."random-shuffle")."0.0.4").revisions).default;
        "lsp-types".revision = (((hackage."lsp-types")."1.2.0.0").revisions).default;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.6").revisions).default;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.three = false;
        "transformers-compat".flags.mtl = true;
        "profunctors".revision = (((hackage."profunctors")."5.6.2").revisions).default;
        "stylish-haskell".revision = (((hackage."stylish-haskell")."0.12.2.0").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.4.2.0").revisions).default;
        "js-dgtable".revision = (((hackage."js-dgtable")."0.5.2").revisions).default;
        "microlens".revision = (((hackage."microlens")."0.4.12.0").revisions).default;
        "resourcet".revision = (((hackage."resourcet")."1.2.4.2").revisions).default;
        "aeson".revision = (((hackage."aeson")."1.5.6.0").revisions).default;
        "aeson".flags.developer = false;
        "aeson".flags.bytestring-builder = false;
        "aeson".flags.fast = false;
        "aeson".flags.cffi = false;
        "cabal-doctest".revision = (((hackage."cabal-doctest")."1.0.8").revisions).default;
        "implicit-hie".revision = (((hackage."implicit-hie")."0.1.2.5").revisions).default;
        "multistate".revision = (((hackage."multistate")."0.8.0.3").revisions).default;
        "multistate".flags.build-example = false;
        "data-default".revision = (((hackage."data-default")."0.7.1.1").revisions).default;
        "hie-bios".revision = (((hackage."hie-bios")."0.7.5").revisions).default;
        "hls-splice-plugin".revision = (((hackage."hls-splice-plugin")."1.0.0.3").revisions).default;
        "semigroups".revision = (((hackage."semigroups")."0.19.1").revisions).default;
        "semigroups".flags.bytestring = true;
        "semigroups".flags.deepseq = true;
        "semigroups".flags.binary = true;
        "semigroups".flags.containers = true;
        "semigroups".flags.tagged = true;
        "semigroups".flags.template-haskell = true;
        "semigroups".flags.bytestring-builder = false;
        "semigroups".flags.transformers = true;
        "semigroups".flags.hashable = true;
        "semigroups".flags.unordered-containers = true;
        "semigroups".flags.text = true;
        "uniplate".revision = (((hackage."uniplate")."1.6.13").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.14.0").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.5").revisions).default;
        "libyaml".revision = (((hackage."libyaml")."0.1.2").revisions).default;
        "libyaml".flags.system-libyaml = false;
        "libyaml".flags.no-unicode = false;
        "network-bsd".revision = (((hackage."network-bsd")."2.8.1.0").revisions).default;
        "generic-lens-core".revision = (((hackage."generic-lens-core")."2.1.0.0").revisions).default;
        "hls-test-utils".revision = (((hackage."hls-test-utils")."1.0.1.0").revisions).default;
        "hls-test-utils".flags.pedantic = false;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.3").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "filepattern".revision = (((hackage."filepattern")."0.1.2").revisions).default;
        "ghc-exactprint".revision = (((hackage."ghc-exactprint")."0.6.4").revisions).default;
        "ghc-exactprint".flags.dev = false;
        "ghc-exactprint".flags.roundtrip = false;
        "gitrev".revision = (((hackage."gitrev")."1.3.1").revisions).default;
        "hspec-core".revision = (((hackage."hspec-core")."2.7.10").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "temporary".revision = (((hackage."temporary")."1.3").revisions).default;
        "opentelemetry".revision = (((hackage."opentelemetry")."0.7.0").revisions).default;
        "unbounded-delays".revision = (((hackage."unbounded-delays")."0.1.1.1").revisions).default;
        "tasty".revision = (((hackage."tasty")."1.4.1").revisions).default;
        "tasty".flags.unix = true;
        "tasty".flags.clock = true;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "kan-extensions".revision = (((hackage."kan-extensions")."5.2.2").revisions).default;
        "indexed-profunctors".revision = (((hackage."indexed-profunctors")."0.1.1").revisions).default;
        "strict".revision = (((hackage."strict")."0.4.0.1").revisions).default;
        "strict".flags.assoc = true;
        "deque".revision = (((hackage."deque")."0.4.3").revisions).default;
        "attoparsec".revision = (((hackage."attoparsec")."0.13.2.5").revisions).default;
        "attoparsec".flags.developer = false;
        "generic-lens".revision = (((hackage."generic-lens")."2.1.0.0").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "blaze-textual".revision = (((hackage."blaze-textual")."0.2.1.0").revisions).default;
        "blaze-textual".flags.native = true;
        "blaze-textual".flags.integer-simple = false;
        "blaze-textual".flags.developer = false;
        "file-embed".revision = (((hackage."file-embed")."0.0.14.0").revisions).default;
        "floskell".revision = (((hackage."floskell")."0.10.5").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.6").revisions).default;
        "terminfo".revision = (((hackage."terminfo")."0.4.1.4").revisions).default;
        "syb".revision = (((hackage."syb")."0.7.2.1").revisions).default;
        "hls-ormolu-plugin".revision = (((hackage."hls-ormolu-plugin")."1.0.0.0").revisions).default;
        "safe-exceptions".revision = (((hackage."safe-exceptions")."0.1.7.1").revisions).default;
        "invariant".revision = (((hackage."invariant")."0.5.4").revisions).default;
        "dependent-map".revision = (((hackage."dependent-map")."0.4.0.0").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.2.0").revisions).default;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.random-initial-seed = false;
        "refinery".revision = (((hackage."refinery")."0.3.0.0").revisions).default;
        "clock".revision = (((hackage."clock")."0.8.2").revisions).default;
        "clock".flags.llvm = false;
        "adjunctions".revision = (((hackage."adjunctions")."4.4").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.8").revisions).default;
        "comonad".flags.distributive = true;
        "comonad".flags.indexed-traversable = true;
        "comonad".flags.containers = true;
        "retrie".revision = (((hackage."retrie")."1.0.0.0").revisions).default;
        "retrie".flags.buildexecutable = true;
        "ghc-heap".revision = (((hackage."ghc-heap")."8.10.5").revisions).default;
        "ghci".revision = (((hackage."ghci")."8.10.5").revisions).default;
        "assoc".revision = (((hackage."assoc")."1.0.2").revisions).default;
        "sqlite-simple".revision = (((hackage."sqlite-simple")."0.4.18.0").revisions).default;
        "regex-base".revision = (((hackage."regex-base")."0.94.0.1").revisions).default;
        "fourmolu".revision = (((hackage."fourmolu")."0.3.0.0").revisions).default;
        "fourmolu".flags.dev = false;
        "hls-tactics-plugin".revision = (((hackage."hls-tactics-plugin")."1.2.0.0").revisions).default;
        "hls-tactics-plugin".flags.pedantic = false;
        "type-equality".revision = (((hackage."type-equality")."1").revisions).default;
        "filemanip".revision = (((hackage."filemanip")."0.3.6.3").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."3.2.1.0").revisions).default;
        "hspec".revision = (((hackage."hspec")."2.7.10").revisions).default;
        "ghc-source-gen".revision = (((hackage."ghc-source-gen")."0.4.0.0").revisions).default;
        "network-info".revision = (((hackage."network-info")."0.2.0.10").revisions).default;
        "th-compat".revision = (((hackage."th-compat")."0.1.2").revisions).default;
        "mmorph".revision = (((hackage."mmorph")."1.1.5").revisions).default;
        "indexed-traversable".revision = (((hackage."indexed-traversable")."0.1.1").revisions).default;
        "zlib".revision = (((hackage."zlib")."0.6.2.3").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.bundled-c-zlib = false;
        "zlib".flags.pkg-config = false;
        "hls-pragmas-plugin".revision = (((hackage."hls-pragmas-plugin")."1.0.0.0").revisions).default;
        "MonadRandom".revision = (((hackage."MonadRandom")."0.5.3").revisions).default;
        "ghc-trace-events".revision = (((hackage."ghc-trace-events")."0.1.2.3").revisions).default;
        "indexed-traversable-instances".revision = (((hackage."indexed-traversable-instances")."0.1").revisions).default;
        "data-default-instances-dlist".revision = (((hackage."data-default-instances-dlist")."0.0.1").revisions).default;
        "hiedb".revision = (((hackage."hiedb")."0.3.0.1").revisions).default;
        "base-compat-batteries".revision = (((hackage."base-compat-batteries")."0.11.2").revisions).default;
        "extra".revision = (((hackage."extra")."1.7.9").revisions).default;
        "old-locale".revision = (((hackage."old-locale")."1.0.0.7").revisions).default;
        "split".revision = (((hackage."split")."0.2.3.4").revisions).default;
        "StateVar".revision = (((hackage."StateVar")."1.2.1").revisions).default;
        "HsYAML".revision = (((hackage."HsYAML")."0.2.1.0").revisions).default;
        "HsYAML".flags.exe = false;
        "stm".revision = (((hackage."stm")."2.5.0.1").revisions).default;
        };
      compiler = {
        version = "8.10.5";
        nix-name = "ghc8105";
        packages = {
          "binary" = "0.8.8.0";
          "ghc-boot" = "8.10.5";
          "ghc-prim" = "0.6.1";
          "exceptions" = "0.10.4";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
          "template-haskell" = "2.16.0.0";
          "pretty" = "1.1.3.6";
          "process" = "1.6.9.0";
          "hpc" = "0.6.1.0";
          "base" = "4.14.2.0";
          "rts" = "1.0.1";
          "text" = "1.2.4.1";
          "mtl" = "2.2.2";
          "time" = "1.9.3";
          "unix" = "2.7.2.2";
          "bytestring" = "0.10.12.0";
          "ghc" = "8.10.5";
          "containers" = "0.6.4.1";
          "directory" = "1.3.6.0";
          "parsec" = "3.1.14.0";
          "ghc-boot-th" = "8.10.5";
          "filepath" = "1.4.2.1";
          "deepseq" = "1.4.4.0";
          "transformers" = "0.5.6.2";
          "terminfo" = "0.4.1.4";
          "ghc-heap" = "8.10.5";
          "ghci" = "8.10.5";
          "Cabal" = "3.2.1.0";
          "stm" = "2.5.0.1";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        haskell-language-server = ./.plan.nix/haskell-language-server.nix;
        ghc-api-compat = ./.plan.nix/ghc-api-compat.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "haskell-language-server" = {
            flags = {
              "brittany" = lib.mkOverride 900 true;
              "stylishhaskell" = lib.mkOverride 900 true;
              "importlens" = lib.mkOverride 900 true;
              "ormolu" = lib.mkOverride 900 true;
              "splice" = lib.mkOverride 900 true;
              "retrie" = lib.mkOverride 900 true;
              "fourmolu" = lib.mkOverride 900 true;
              "all-formatters" = lib.mkOverride 900 false;
              "hlint" = lib.mkOverride 900 true;
              "eval" = lib.mkOverride 900 true;
              "class" = lib.mkOverride 900 true;
              "haddockcomments" = lib.mkOverride 900 true;
              "all-plugins" = lib.mkOverride 900 false;
              "pragmas" = lib.mkOverride 900 true;
              "pedantic" = lib.mkOverride 900 false;
              "floskell" = lib.mkOverride 900 true;
              "tactic" = lib.mkOverride 900 true;
              "modulename" = lib.mkOverride 900 true;
              "refineimports" = lib.mkOverride 900 true;
              };
            };
          "ghc-api-compat" = { flags = {}; };
          };
        })
    ];
  }