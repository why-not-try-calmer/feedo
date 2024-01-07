# 9.0.2 required for static linking
# matching GHC version provided by lts-19.33
FROM utdemir/ghc-musl:v25-ghc902 as builder
WORKDIR /opt/app/
# build dependencies
COPY ./feedfarer.cabal ./stack.yaml ./
RUN ghcup install stack

RUN stack --resolver lts-19.33 build --only-dependencies --no-library-profiling --system-ghc

# build package
COPY . .

RUN stack --resolver lts-19.33 install --local-bin-path . --system-ghc --ghc-options '-fPIC -O2 -static -optl-static -optl-pthread' 

# runner
FROM alpine:latest as runner
WORKDIR /opt/app/
COPY --from=builder /opt/app/feedfarer-exe .
# run tests from the executable
CMD ./feedfarer-exe