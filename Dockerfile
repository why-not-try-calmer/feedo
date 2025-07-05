FROM ghcr.io/why-not-try-calmer/feedo-deps AS deps
COPY . .
RUN stack --resolver nightly-2025-06-16 install \
  --no-install-ghc \
  --system-ghc \
  --local-bin-path . \
  --flag feedfarer:static

FROM alpine:latest

# Version from build arg
ARG app_version
ENV APP_VERSION=$app_version

WORKDIR /opt/app/
COPY --from=deps /opt/app/feedfarer-exe .

CMD ["./feedfarer-exe"]
