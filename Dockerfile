FROM debian:latest

RUN apt-get update && \
    apt-get install -y \
    ghc \
    libpq-dev \
    cabal-install \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . .
RUN cabal update && \
    cabal build

CMD ["cabal", "run"]

