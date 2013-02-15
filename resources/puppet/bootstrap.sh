#!/bin/bash

set -o errexit -o nounset -o xtrace

sudo apt-get update
sudo apt-get install puppet -y 

puppet module install puppetlabs/postgresql
puppet module install puppetlabs/firewall

#puppet apply encoding-mashup-db.pp --modulepath=
#puppet apply encoding-mashup-web.pp --modulepath=
