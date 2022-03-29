{ emacs, git, runCommand, lib, coreutils, gawk, curl, jq }:

let
  buildInputs =
    [ emacs
      git
      coreutils
      gawk
      curl
    ];
in
  runCommand
    "generate-documentation-scripts"
    { inherit buildInputs; }
    ''
      mkdir -p $out/bin
      cd $out/bin

      cp ${./test-doc.org} ./test-doc.org
      emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "./test-doc.org")'
      mv ./test-doc.org $out/test-doc.org
    ''
