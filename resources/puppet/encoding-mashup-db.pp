node default {
    include postgresql::server
    
    package { 'postgresql-contrib-9.1':
        ensure => installed, 
    }
}
