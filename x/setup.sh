#!/bin/sh

ln -s $(readlink -f $(dirname $0))/xsession ~/.xsession
ln -s $(readlink -f $(dirname $0))/Xresources ~/.Xresources
