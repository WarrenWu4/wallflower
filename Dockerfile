FROM alpine:latest

# install necessary dependencies
# last line contains all depencies for haskell project
# got no idea if I need all of the rest of the packages though :p
RUN apk update && apk add --no-cache \
    bash curl git make m4 gcc g++ \
    opam findutils \
    linux-headers gmp-dev \
    binutils-gold libc-dev libffi-dev musl-dev ncurses-dev perl tar xz \
    build-base freetype-dev glib-dev sdl2-dev glew-dev pkgconf

# download and install ghcup
RUN curl -L https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup -o /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ENV HOME=/root

ENV PATH=$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH

RUN ghcup install ghc 9.6.7 && \
    ghcup set ghc 9.6.7 && \
    ghcup install cabal 3.12.1.0 && \
    ghcup set cabal 3.12.1.0

WORKDIR /app

COPY . /app
    
CMD ["/bin/bash"]
