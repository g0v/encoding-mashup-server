# Encoding Mashup

This is a system to help users find the correct Unicode or the CNS 11643 encoding
for Han characters found as pictures (due to the limitation of Big-5)
in many dictionaries published by the Ministry of Education (Taiwan).
This project is part of the g0v project.

Here are the tentative list of registered characters.
We loosely follow the syntax of URI paths.

- `moe/revised/xxxx`
- `moe/twblg/xxxx`
- `moe/hakka/xxxx`

## Development Environment

We are trying to follow these coding styles and conventions:

* [Git commit message guidelines](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html) (by tpope).

The backend is written in Haskell with the Snap web framework.
To set up the development environment, you first have to install GHC. For example, on Ubuntu

    sudo apt-get install ghc haskell-platform

Using GHC of version at least 7.4 is recommended. Also, using cabal-dev to build up a virtual environment is a good practice. To install it using cabal-install

    cabal update
    cabal install cabal-dev

Then do the following:

    git clone <this repository>
    cabal-dev install --only-dependencies
    cabal-dev build

## Testing

We have not set up a nice interface for importing the data.
Currently, to test the server with existing data, do the following:

    wget 'https://docs.google.com/spreadsheet/pub?key=0AttD1zENsweydFhtZXhKVjdId1JTRkFrSmhvZXozNGc&single=true&gid=0&output=txt' -O sym-google.txt
    python2 ./resources/csv2sqlite.py google.csv [path to the database]
    [run the server]

## Copyright

All our code is released under [BSD-3](http://www.opensource.org/licenses/BSD-3-clause).
Note that the file `src/Main.hs` was automatically generated by the Snap web framework
and then slightly modified by us.
