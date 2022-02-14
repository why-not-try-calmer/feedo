FROM haskell:8.10

EXPOSE 80

WORKDIR /opt/app
RUN stack upgrade

COPY ./feedfarer.cabal ./stack.yaml /opt/app/
RUN stack build --only-dependencies --no-library-profiling

COPY . /opt/app/
RUN stack build

CMD ["stack", "exec", "feedfarer-exe"]