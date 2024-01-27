#!/usr/bin/bash                                                                                                                                                                              
source .env
openssl req -newkey rsa:2048 -sha256 -nodes -keyout private.key -x509 -days 365 -out cert.pem -subj "/C=US/ST=Bern/L=Bern/O=MyOwn/CN=$DOMAIN_NAME"
curl -F "url=https://feedo.cloudns.ph/webhook/bot$TELEGRAM_TOKEN" https://api.telegram.org/bot$TELEGRAM_TOKEN/setWebhook -F "certificate=@/opt/app/cert.pem"
