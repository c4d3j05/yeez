# ---------------------------------------------------------------------------
# Build stage: compile the yeez binary against amazonka + brick.
# ---------------------------------------------------------------------------
FROM haskell:9.4.8 AS build

# No apt needed here: vty builds against GHC's bundled `terminfo` boot
# library, so no ncurses dev headers are required. (The base image is
# Debian buster, whose apt repos are archived — avoid touching apt.)
WORKDIR /app

# Refresh the package index, then resolve and build dependencies first so
# this expensive layer is cached across source-only changes.
RUN cabal update
COPY yeez.cabal ./
RUN cabal build --only-dependencies exe:yeez

# Now build the app itself and stage the binary at a known path.
COPY . .
RUN cabal build exe:yeez \
    && mkdir -p /out \
    && cp "$(cabal list-bin exe:yeez)" /out/yeez

# ---------------------------------------------------------------------------
# Runtime stage: a slim image carrying just the binary + its shared libs.
# ---------------------------------------------------------------------------
FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libncursesw6 libtinfo6 libgmp10 ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /out/yeez /usr/local/bin/yeez

ENTRYPOINT ["yeez"]
