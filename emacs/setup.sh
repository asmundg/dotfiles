#!/bin/sh

ln -sf $(readlink -f $(dirname $0))/emacs.el ~/.emacs
if [ ! -L ~/.emacs.d ]; then ln -sf $(readlink -f $(dirname $0))/emacs.d ~/.emacs.d; fi
