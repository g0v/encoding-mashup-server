node default {

    exec { "git-author-name":
		path    => '/usr/bin:/usr/sbin:/bin',
        command => 'git config --global user.name "Paul Meng"',
        unless => "git config --global --get user.name|grep 'Paul Meng'"
    }

    exec { "git-author-email":
		path    => '/usr/bin:/usr/sbin:/bin',
        command => 'git config --global user.email "mno2@mno2.org"',
        unless => "git config --global --get user.email|grep 'mno2@mno2.org'"
    }

    exec { "git-global-ignore":
		path    => '/usr/bin:/usr/sbin:/bin',
        command => "git config --global core.excludesfile /home/$id/.gitignore",
        unless  => 'git config -l --global|grep excludesfile'
    }


	exec { 'system-spf13':
		path    => '/usr/bin:/usr/sbin:/bin',
		command => "bash -c '/usr/bin/curl http://j.mp/spf13-vim3 -L -o /tmp/spf13-installer ; \
			 chmod +x /tmp/spf13-installer ; \
			 /tmp/spf13-installer ; \
			 rm /tmp/spf13-installer'",
        creates => "/home/mno2/.spf13-vim-3/bootstrap.sh",
	}
	
	exec { 'system-oh-my-zsh':
		path    => '/usr/bin:/usr/sbin:/bin',
		command => "bash -c '/usr/bin/curl -s https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -L -o /tmp/oh-my-zsh-installer ; \
			 chmod +x /tmp/oh-my-zsh-installer ; \
			 /tmp/oh-my-zsh-installer ; \
			 rm /tmp/oh-my-zsh-installer'",
        creates => "/home/mno2/.oh-my-zsh/oh-my-zsh.sh"
	}
}
