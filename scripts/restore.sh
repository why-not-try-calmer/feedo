#!/usr/bin/bash env
path_to_dump=$1
username=$2
password=$3
db_name=$4 # feedfarer, feedfarer-test...

mongorestore -d $db_name \
    --username=$username \
    --password=$password \
    --authenticationDatabase=admin \
    --archive=$path_to_dump