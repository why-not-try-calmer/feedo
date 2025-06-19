# 9.0.2 required for static linking matching GHC version provided by lts-19.33
FROM docker.io/benz0li/ghc-musl:9.10.2 AS builder
WORKDIR /opt/app/
COPY feedfarer.cabal stack.yaml .

# Dependencies for caching
RUN stack --resolver nightly-2025-06-16 build \
  --no-install-ghc \
  --system-ghc \
  --no-library-profiling \
  --only-dependencies

# Build main
COPY . .

RUN stack --resolver nightly-2025-06-16 install \
  --no-install-ghc \
  --system-ghc \
  --local-bin-path . \
  --flag feedfarer:static

FROM alpine:latest AS runner

# Version from build arg
ARG app_version
ENV APP_VERSION=$app_version

WORKDIR /opt/app/
COPY --from=builder /opt/app/feedfarer-exe .

CMD ["./feedfarer-exe"]
