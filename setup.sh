#!/bin/sh -eux

for d in emacs git i3 misc x zsh; do
    ./$d/setup.sh
done
