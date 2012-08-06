#!/bin/sh

ln -s $(readlink -f $(dirname $0))/emacs.el ~/.emacs
ln -s $(readlink -f $(dirname $0))/emacs.d ~/.emacs.d
