#!/bin/sh -eux

ln -sf $(readlink -f $(dirname $0))/profile ~/.profile
