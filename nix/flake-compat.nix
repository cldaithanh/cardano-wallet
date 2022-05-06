{...}@args:
let
  src = args.src or ../.;
  lock = builtins.fromJSON (builtins.readFile (src + "/flake.lock"));
  flake-compate-input = lock.nodes.root.inputs.flake-compat;
  nixpkgs-input = lock.nodes.haskellNix.inputs.${builtins.elemAt lock.nodes.root.inputs.nixpkgs 1};
  flake-compat = import (pkgs.fetchzip {
    url = "https://github.com/input-output-hk/flake-compat/archive/${lock.nodes.${flake-compate-input}.locked.rev}.tar.gz";
    sha256 = lock.nodes.${flake-compate-input}.locked.narHash;
  });
  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.${nixpkgs-input}.locked.rev}.tar.gz";
      sha256 = lock.nodes.${nixpkgs-input}.locked.narHash;
    })
    { };
in
flake-compat {
  inherit src pkgs;
  override-inputs = {
    customConfig = args;
  };
}
