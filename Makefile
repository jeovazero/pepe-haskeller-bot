all: build

dev:
	ghcid --command "ghci src/Main -fobject-code -i.:src" --test main

build:
	nix-build release.nix

cabal2nix:
	cabal2nix . > default.nix

init-cabal-nix:
	nix-shell -p cabal2nix --run 'cabal2nix . > default.nix'
 
docker-image:
	nix-build docker.nix -o image

# requires login in the docker hub
docker-image-and-push:
	nix-build docker.nix -o image
	docker push `docker load < image | awk '{print $$3}'`
