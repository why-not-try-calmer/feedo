#!/usr/bin/bash env

docker run -it --rm --name certbot \
    -v "/certs/etc:/etc/letsencrypt" \
    -v "/certs/lib:/var/lib/letsencrypt" \
    certbot/certbot certonly