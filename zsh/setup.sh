#!/bin/sh

git clone https://github.com/robbyrussell/oh-my-zsh.git $(dirname $0)/oh-my-zsh
ln -sf $(readlink -f $(dirname $0))/zshrc ~/.zshrc
