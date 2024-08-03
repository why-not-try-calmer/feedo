#!/usr/bin/env bash            
curl -F "url=https://feedo.cloudns.ph/webhook/bot$TELEGRAM_TOKEN" https://api.telegram.org/bot$TELEGRAM_TOKEN/setWebhook -F "certificate=${CERT_FULLCHAIN}"
