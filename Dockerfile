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
RUN stack install --ghc-options "-optl-static -fPIC"
# using a runner very likely to share glibc version with builder
FROM debian:buster-slim as runner
COPY --from=builder /root/.local/bin/feedfarer-exe /bin
# apparently needed to dodge an 'getaddrinfo' error at runtime
COPY --from=builder /etc/protocols /etc/protocols
COPY --from=builder /etc/services /etc/services
# not forgetting about web assets
COPY /static /var/www/feedfarer-webui
