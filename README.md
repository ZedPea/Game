# sdl-game-wip

Messing around with SDL to see if I can make something fun and practice my haskell.

Currently working towards a helicopter game where you fly through a tunnel without hitting the blocks.

## Installation:

#### Install prerequistes
You need ghc, cabal, sdl2, sdl2-image, and sdl2-ttf.

#### Debian based:
`sudo apt-get install ghc haskell-cabal-install libsdl2-dev libsdl2-image libsdl2-ttf`

Some of these packages may not be available in older debian versions.

#### Arch based:
`sudo pacman -S ghc cabal-install sdl2 sdl2_image sdl2_ttf`

Commands should be similar for other distributions. Depending on your distros support for sdl, you may have to compile the libraries yourself.

#### Clone the repository
`git clone https://github.com/ZedPea/sdl-game-wip.git`

or download the folder as a zip and unzip it.

#### Compile
Move to the directory you downloaded the repo to.

`cd sdl-game-wip/Src`

`cabal install`

This will install the program in ~/.cabal/bin

If you add this line to your ~/.profile:
`PATH=$PATH:~/.cabal/bin`
you will be able to run the program from any bash shell.

Otherwise, you will have to invoke it with ~/.cabal/bin/sdl-game-wip

#### Running the program

Run `sdl-game-wip`
