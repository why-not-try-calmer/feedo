FROM haskell:9.0.2

EXPOSE 80

WORKDIR /opt/app
RUN stack upgrade

COPY ./feedfarer.cabal ./stack.yaml /opt/app/
RUN stack build --only-dependencies --no-library-profiling

COPY . /opt/app/
RUN stack build

COPY /static /var/www/feedfarer-webui