{ pkgs ? import <nixpkgs> {}
, ps ? import (fetchTarball https://github.com/nonbili/nonbili-nix-deps/archive/de63b5eeae8cb4952c08fdbc2eea533ed14dd857.tar.gz) {}
}:
pkgs.mkShell {
  # purs-0.14, spago-0.14
  buildInputs = [ ps.purs ps.spago ];
}
