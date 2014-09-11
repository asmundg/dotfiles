#!/bin/sh -eux

if [ ! -L ~/.i3 ]; then ln -sf $(readlink -f $(dirname $0)) ~/.i3; fi
