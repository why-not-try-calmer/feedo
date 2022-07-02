# build dependencies
FROM haskell:9.0.2-slim-buster as builder
WORKDIR /opt/app/
COPY ./package.yaml ./stack.yaml ./
# dependencies layer
RUN stack upgrade && stack build --only-dependencies --no-library-profiling
# main binary layer, reusing cache
COPY . .
# awful hack to fix a bug
WORKDIR /usr/lib/gcc/x86_64-linux-gnu/8/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
# building statically linked executable
WORKDIR /opt/app/
RUN stack install --ghc-options "-optl-static -fPIC"
# inject build artifacts from previous step into fresh image
FROM alpine:latest as runner
COPY --from=builder /root/.local/bin/feedfarer-exe /bin
# not forgetting about web assets
COPY /static /var/www/feedfarer-webui