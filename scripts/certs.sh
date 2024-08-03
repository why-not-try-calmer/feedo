#!/usr/bin/env bash

source .env
export CERT_FULLCHAIN
export TELEGRAM_TOKEN

docker compose stop nginx

docker run -it --rm --name certbot  \
    -v "/certs/etc:/etc/letsencrypt" \
    -v "/certs/lib:/var/lib/letsencrypt" \
	-e CERT_FULLCHAIN \
	-e TELEGRAM_TOKEN \
	-p 80:80 \
	certbot/certbot renew > certs_refresh.log

if [[ grep -q "No renewals were attempted." certs_refresh.log ]]; then
	echo "No need to renew certificate."
else
	./scripts/set_webhook.sh
fi

docker compose start nginx