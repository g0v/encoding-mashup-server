# 中文造字圖檔檢字系統

這系統可用來檢出、對校教育部字典中所用的造字。目前後端用 Haskell 的 Snap 架構寫。


## For Developers

We are trying to follow these coding styles and conventions:

* [Git commit message guidelines](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html) (by tpope).

To set up the development environment, you first have to install ghc. For example, on ubuntu 

    sudo apt-get install ghc haskell-platform

ghc >= 7.4 is recommended. Also, using cabal-dev to build up a virtual environment is a good practice. To install it using cabal-install

    cabal update
    cabal install cabal-dev

Then do the following:

    git clone <this repository>
    cabal-dev install --only-dependencies
    cabal-dev build


## 版權

All our code is released under [BSD-3](http://www.opensource.org/licenses/BSD-3-clause).
