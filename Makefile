all: build

dev:
	ghcid --command "ghci Main -fobject-code" --test main

build:
	nix-build release.nix

cabal2nix:
	cabal2nix . > default.nix

init-cabal-nix:
	nix-shell -p cabal2nix --run 'cabal2nix . > default.nix'
 
