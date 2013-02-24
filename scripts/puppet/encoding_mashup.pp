import "basenode"

node encoding-mashup inherits basenode {
    include postgresql::server
    
    package { 'postgresql-contrib-9.1':
        ensure => installed, 
    }

	exec { 'system-keter':
		path    => '/usr/bin:/usr/sbin:/bin',
		command => "bash -c '/usr/bin/curl -o /tmp/keter-installer.sh -L /tmp/keter-installer https://raw.github.com/snoyberg/keter/master/setup-keter.sh ; \
			 chmod +x /tmp/keter-installer ; \
			 /tmp/keter-installer ; \
			 rm /tmp/keter-installer'",
        creates => "/opt/keter/bin/keter",
	}
}
