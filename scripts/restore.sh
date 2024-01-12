#!/usr/bin/bash env
path_to_dump=$1
username=$2
password=$3

mongorestore -d feedfarer $1 --username=$2 --password=$3 --authenticationDatabase=admin