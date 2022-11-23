# stack-ready ghc 9.2.2 compiled against musl
FROM nycticoracs/ghc-musl-with-stack as builder
WORKDIR /opt/app/
# build dependencies
COPY ./feedfarer.cabal ./stack.yaml ./
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