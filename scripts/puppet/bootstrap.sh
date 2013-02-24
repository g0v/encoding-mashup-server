#!/usr/bin/env bash

#set -o errexit -o nounset -o xtrace

sudo apt-get update
sudo apt-get install puppet -y 

puppet module install saz/vim
puppet module install puppetlabs/postgresql
puppet module install puppetlabs/firewall
puppet module install maestrodev/rvm
puppet module install maestrodev/git

#puppet apply basenode.pp--modulepath=
