# dependencies
sudo pacman -S sdl2
sudo pacman -S glew

# ghcup setup
ghcup install ghc 9.6.7 && \
ghcup set ghc 9.6.7 && \
ghcup install cabal 3.12.1.0 && \
ghcup set cabal 3.12.1.0
