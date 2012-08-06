#!/bin/sh

ln -s $(readlink -f $(dirname $0))/gitconfig ~/.gitconfig
