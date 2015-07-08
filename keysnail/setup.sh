#!/bin/sh -eux

ln -sf $(readlink -f $(dirname $0))/keysnail.js ~/.keysnail.js
