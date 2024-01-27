#!/usr/bin/bash env
docker compose up -d mongo redis
docker compose exec -e TEST=1 mongo mongosh --file /scripts/mongo-startup.js
docker compose build test && docker compose run test