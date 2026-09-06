# syntax=docker/dockerfile:1
# ---------------------------------------------------------------------------
# Build stage: compile the yeez binary against amazonka + brick.
# ---------------------------------------------------------------------------
FROM haskell:9.4.8 AS build

# No apt needed here: vty builds against GHC's bundled `terminfo` boot
# library, so no ncurses dev headers are required. (The base image is
# Debian buster, whose apt repos are archived — avoid touching apt.)
WORKDIR /app

COPY . .

# Build the executable. The cache mounts keep the Hackage index and the
# compiled dependency store (amazonka is large) across rebuilds, so only
# changed local modules recompile. `dist-newstyle` is a cache mount too, so
# the binary is copied to /out within this same RUN while the mount is live.
RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/app/dist-newstyle \
    cabal update \
    && cabal build exe:yeez \
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
