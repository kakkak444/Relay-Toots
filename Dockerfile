FROM haskell:9.12-slim-bookworm AS build

WORKDIR /workspace

RUN cabal update

COPY ./relay-mstdn-toots.cabal /workspace/
COPY ./cabal.project /workspace/

RUN --mount=type=cache,target=/workspace/dist-newstyle cabal new-build --only-dependencies -f release --semaphore -j --enable-executable-stripping --enable-executable-static

COPY . /workspace
RUN cabal new-install -f release --semaphore -j --installdir="/dist" --install-method=copy --enable-executable-stripping --enable-executable-static

FROM debian:stable-slim

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y ca-certificates locales

RUN update-ca-certificates
RUN echo "C.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen C.UTF-8 && \
    update-locale LANG=C.UTF-8
ENV LANG=C.UTF-8

COPY --from=build /dist/relay-mstdn-toots /app/
WORKDIR /app

ENTRYPOINT ["/app/relay-mstdn-toots", "+RTS", "-N", "-RTS"]

LABEL org.opencontainers.image.source=https://github.com/kakkak444/Relay-Toots
