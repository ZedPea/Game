# helicopter-game

Helicopter game where you fly through a tunnel, avoiding the blocks.

Use Enter to start the game. Use W to fly. Use q to quit once you have crashed.

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
`git clone https://github.com/ZedPea/helicopter-game.git`

or download the folder as a zip and unzip it.

#### Compile
Move to the directory you downloaded the repo to.

`cd helicopter-game`

`cabal install`

This will install the program in ~/.cabal/bin

If you add this line to your ~/.profile:
`PATH=$PATH:~/.cabal/bin`
you will be able to run the program from any bash shell.

Otherwise, you will have to invoke it with ~/.cabal/bin/helicopter-game

#### Running the program

Run `helicopter-game`
