#!/bin/sh -eux

export PATH=$HOME/.local/bin/:$PATH

sudo apt-get update
sudo apt-get install -y python-dev python-setuptools
easy_install --user pip
pip install --user ansible
(cd $(dirname $0)
 ansible-playbook -i bootstrap/inventory -K bootstrap/bootstrap.yml
 stow -v emacs i3 keysnail zsh
)
