# Fish and friends
nix-env -i fish
curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
fisher install jorgebucaran/nvm.fish@8922519
fisher install lilyball/nix-env.fish@00c6cc7

nix-env -i ispell

brew install sdcv
mkdir -p ~/.stardict/dic/
curl https://s3.amazonaws.com/jsomers/dictionary.zip | tar -xO --strip-components=1 dictionary/stardict-dictd-web1913-2.4.2.tar.bz2 | tar -xC ~/.stardict/dic/

nix-env -i direnv

nix-env -i gnuplot

nix-env -f '<nixpkgs>' -iA nodePackages.node2nix

yarn global add indium

brew install mermaid-cli

nix-env -i nixfmt
