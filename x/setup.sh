#!/bin/sh

ln -sf $(readlink -f $(dirname $0))/xsession ~/.xsession
ln -sf $(readlink -f $(dirname $0))/Xresources ~/.Xresources
