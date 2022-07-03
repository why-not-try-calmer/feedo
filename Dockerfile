# build dependencies
FROM haskell:9.0.2 as builder
WORKDIR /opt/app/
COPY ./feedfarer.cabal ./stack.yaml ./
# dependencies layer
RUN stack upgrade && stack build --only-dependencies --no-library-profiling
# main binary layer, reusing cache
COPY . .
RUN stack build
# inject build artifacts from previous step into fresh image to minimize size
FROM haskell:9.0.2-slim-buster as runner
EXPOSE 80
WORKDIR /opt/app/
COPY --from=builder /opt/app/.stack-work ./.stack-work
COPY . .
