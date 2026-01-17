brew reinstall gcc libgccjit
brew tap d12frosted/emacs-plus
brew install emacs-plus@29 --with-poll --with-native-comp

# Zsh basics
curl -L git.io/antigen >antigen.zsh
cp antigen.zsh ~/

brew install atuin

brew install --cask font-juliamono

brew install sdcv
mkdir -p ~/.stardict/dic/
curl https://s3.amazonaws.com/jsomers/dictionary.zip | tar -xO --strip-components=1 dictionary/stardict-dictd-web1913-2.4.2.tar.bz2 | tar -xC ~/.stardict/dic/

nix-env -i direnv

nix-env -i gnuplot

nix-env -f '<nixpkgs>' -iA nodePackages.node2nix

yarn global add indium

brew install mermaid-cli

nix-env -i nixfmt
