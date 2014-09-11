#!/bin/sh -eux

./setup.sh

sudo apt-get update
sudo apt-get install python-pip
pip install --user vex
vex -m ansible true || true
vex ansible pip install ansible
vex ansible ansible-playbook -i bootstrap/inventory -K bootstrap/bootstrap.yml
