FROM utdemir/ghc-musl:v25-ghc944 as builder

RUN ghcup install stack
WORKDIR /opt/app/
COPY feedfarer.cabal stack.yaml .

# Dependencies for caching
RUN stack --resolver nightly-2023-01-01 build \
  --no-install-ghc \
  --system-ghc \
  --no-library-profiling \
  --only-dependencies

# Build main
COPY . .

RUN stack --resolver nightly-2023-01-01 install \
  --no-install-ghc \
  --system-ghc \
  --local-bin-path . \
  --flag feedfarer:static

FROM alpine:latest as runner

# Version from build arg
ARG app_version
ENV APP_VERSION=$app_version

WORKDIR /opt/app/
COPY --from=builder /opt/app/feedfarer-exe .

CMD ["./feedfarer-exe"]
