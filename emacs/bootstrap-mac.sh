brew install font-fira-code

nix-env -i ispell

brew install pngpaste

nix-env -i direnv

nix-env -f '<nixpkgs>' -iA nodePackages.node2nix
