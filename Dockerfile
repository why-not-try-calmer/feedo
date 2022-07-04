# build dependencies
FROM haskell:9.0.2-slim-buster as builder
WORKDIR /opt/app/
COPY ./package.yaml ./stack.yaml ./
# dependencies layer
RUN stack upgrade && stack build --only-dependencies --no-library-profiling
# main binary layer, reusing cache
COPY . .
# awful hack to fix a bug in glibc
WORKDIR /usr/lib/gcc/x86_64-linux-gnu/8/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
# building statically linked executable
WORKDIR /opt/app/
RUN stack install --ghc-options "-optl-static -fPIC"
# using a runner very likely to share glibc version with builder
FROM debian:buster-slim as runner
COPY --from=builder /root/.local/bin/feedfarer-exe /bin
# apparently needed to dodge an 'getaddrinfo' error at runtime, thanks a lot Buck!
COPY --from=builder /etc/protocols /etc/protocols
COPY --from=builder /etc/services /etc/services
# not forgetting about web assets
COPY /static /var/www/feedfarer-webui
