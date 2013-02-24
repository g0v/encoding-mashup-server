node basenode {
	include vim
    include rvm
	include git
    
	package { zsh:
		ensure => installed,
	}
	
	package { ghc:
		ensure => installed,
	}

	package { haskell-platform:
		ensure => installed,
	}

	package { autojump:
		ensure => installed,
	}

	package { wget:
		ensure => installed,
	}

	package { w3m:
		ensure => installed,
	}


    package { tmux:
        ensure => installed,
    }

    package { 'tree':
        ensure => installed,
    }

	package { ack-grep:
		ensure => installed,
	}


    user { "mno2":
        ensure => present,
        shell  => "/usr/bin/zsh",
        require => [ Package["zsh"] ],
    }

    rvm::system_user { mno2: ; }
    rvm_system_ruby {
        'ruby-1.9.3-p194':
            ensure => 'present',
            default_use => true;
    }

    rvm_gemset {
        "ruby-1.9.3-p194@mno2-laptop":
            ensure => present,
            require => Rvm_system_ruby['ruby-1.9.3-p194'];
    }

    rvm_gem {
        'ruby-1.9.3-p194@mno2-laptop/bundler':
            ensure => latest,
            require => Rvm_system_ruby['ruby-1.9.3-p194'];
        'ruby-1.9.3-p194@mno2-laptop/awesome_print':
            ensure => latest,
            require => Rvm_system_ruby['ruby-1.9.3-p194'];
        'ruby-1.9.3-p194@mno2-laptop/pry':
            ensure => latest,
            require => Rvm_system_ruby['ruby-1.9.3-p194'];
        'ruby-1.9.3-p194@mno2-laptop/puppet-lint':
            ensure => latest,
            require => Rvm_system_ruby['ruby-1.9.3-p194'];
    }

    package { 'apt-file':
        ensure => installed,
    }
}
