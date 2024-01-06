# stack-ready ghc 9.2.5 compiled against musl
FROM utdemir/ghc-musl:v25-ghc925 as builder
WORKDIR /opt/app/
# build dependencies
COPY ./feedfarer.cabal ./stack.yaml ./
RUN ghcup install stack
RUN stack build --system-ghc --only-dependencies --no-library-profiling
# build package
COPY . .
RUN stack install --system-ghc --local-bin-path .
# runner
FROM alpine:latest as runner
WORKDIR /opt/app/
COPY --from=builder /opt/app/feedfarer-exe .
# run tests from the executable
CMD ./feedfarer-exe