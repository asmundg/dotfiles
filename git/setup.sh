#!/bin/sh -eux

ln -sf $(readlink -f $(dirname $0))/gitconfig ~/.gitconfig
ln -sf $(readlink -f $(dirname $0))/gitignore ~/.gitignore
