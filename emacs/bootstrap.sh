# Fish and friends
brew install fish
curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
fisher install jorgebucaran/nvm.fish

nix-env -i ispell

nix-env -i direnv

nix-env -i gnuplot

nix-env -f '<nixpkgs>' -iA nodePackages.node2nix

yarn global add indium

brew install mermaid-cli

nix-env -i nixfmt
