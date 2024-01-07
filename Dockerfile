# 9.0.2 required for static linking
# matching GHC version provided by lts-19.33
FROM utdemir/ghc-musl:v25-ghc902 as builder
RUN ghcup install stack

WORKDIR /opt/app/
COPY . .

RUN stack --resolver lts-19.33 install \
  --local-bin-path . \
  --no-library-profiling \
  --system-ghc \
  --no-install-ghc \
  --flag feedfarer:static

FROM alpine:latest as runner
WORKDIR /opt/app/
COPY --from=builder /opt/app/feedfarer-exe .

CMD ./feedfarer-exe
