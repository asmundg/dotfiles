#!/bin/sh -eux

for d in emacs git i3 keysnail misc x zsh; do
    ./$d/setup.sh
done
