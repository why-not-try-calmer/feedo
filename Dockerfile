# 9.0.2 required for static linking matching GHC version provided by lts-19.33
FROM utdemir/ghc-musl:v25-ghc902 as builder

RUN ghcup install stack
WORKDIR /opt/app/
COPY feedfarer.cabal stack.yaml .

# Dependencies for caching
RUN stack --resolver lts-19.33 build \
  --no-install-ghc \
  --system-ghc \
  --no-library-profiling \
  --only-dependencies

# Build main
COPY . .

RUN stack --resolver lts-19.33 install \
  --no-install-ghc \
  --system-ghc \
  --local-bin-path . \
  --flag feedfarer:static

# Server service
FROM alpine:latest as server
ARG app_version
ENV APP_VERSION=$app_version
WORKDIR /opt/app/
COPY --from=builder /opt/app/feedfarer-exe .

# Worker service
FROM alpine:latest as worker
ARG app_version
ENV APP_VERSION=$app_version
WORKDIR /opt/app/
COPY --from=builder /opt/app/feedfarer-worker-exe .