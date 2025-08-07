FROM haskell:9.12-slim-bookworm AS build

RUN cabal update
WORKDIR /workspace
COPY . /workspace
RUN cabal install --semaphore -j --installdir="/dist" --install-method=copy --enable-executable-stripping --enable-executable-static

FROM gcr.io/distroless/static-debian12:latest

WORKDIR /
COPY --from=build /dist/relay-mstdn-toots /

ENV LANG=C.UTF-8

ENTRYPOINT ["/relay-mstdn-toots"]
