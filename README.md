# helicopter-game

Helicopter game where you fly through a tunnel, avoiding the blocks.

Use Enter to start the game. Hold W to fly up. Use q to quit once you have crashed.

Currently has a memory leak, lol

![Gif of gameplay](game.gif?raw=true "Game gif")

## Installation:

#### Install prerequistes
You need ghc, cabal, sdl2, sdl2-image, and sdl2-ttf.

#### Debian based:
`sudo apt-get install ghc haskell-stack libsdl2-dev libsdl2-image libsdl2-ttf`

Some of these packages may not be available in older debian versions.

#### Arch based:
`sudo pacman -S ghc stack cabal-install sdl2 sdl2_image sdl2_ttf`

Commands should be similar for other distributions. Depending on your distros support for sdl, you may have to compile the libraries yourself.

#### Clone the repository
`git clone https://github.com/ZedPea/helicopter-game.git`

or download the folder as a zip and unzip it.

#### Compile and run
Move to the directory you downloaded the repo to.

`cd helicopter-game`

`stack install`

You can then run the program by adding ~/.local/bin/ to your path, and running `helicopter-game`

Alternatively, you can run `stack exec helicopter-game`
