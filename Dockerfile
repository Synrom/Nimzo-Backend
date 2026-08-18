FROM debian:latest

RUN apt-get update && \
    apt-get install -y \
    ghc \
    libpq-dev \
    cabal-install \
    ca-certificates \
    curl \
    openssl \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . .
RUN cabal update && \
    cabal build

CMD ["cabal", "run", "App"]
