#!/bin/sh

ln -sf $(readlink -f $(dirname $0))/gitconfig ~/.gitconfig
