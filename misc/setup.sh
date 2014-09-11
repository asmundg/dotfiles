#!/bin/sh -eux

ln -s $(readlink -f $(dirname $0))/profile ~/.profile
