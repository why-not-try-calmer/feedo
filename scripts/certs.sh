#!/usr/bin/env bash

# first run

# source .env
# export CERT_FULLCHAIN
# export TELEGRAM_TOKEN

docker run -it --rm --name certbot \
    -v "/certs/etc:/etc/letsencrypt" \
    -v "/certs/lib:/var/lib/letsencrypt" \
    -v "./certbot/www:/var/www/certbot/" \
    certbot/certbot certonly --webroot -w /var/www/certbot -d feedo.cloudns.ph

# curl -F "url=https://feedo.cloudns.ph/webhook/bot$TELEGRAM_TOKEN" https://api.telegram.org/bot$TELEGRAM_TOKEN/setWebhook -F "certificate=${CERT_FULLCHAIN}"

docker run -t --rm --name certbot  \
    -v "/certs/etc:/etc/letsencrypt" \
    -v "/certs/lib:/var/lib/letsencrypt" \
    -v "./certbot/www:/var/www/certbot/" \
    certbot/certbot renew -n

docker compose restart nginx