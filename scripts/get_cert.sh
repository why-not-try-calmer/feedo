#!/usr/bin/env bash
docker run -it --rm --name certbot \
    -v "/certs/etc:/etc/letsencrypt" \
    -v "/certs/lib:/var/lib/letsencrypt" \
    -p 80:80
    certbot/certbot certonly
