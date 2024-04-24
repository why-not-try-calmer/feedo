#!/usr/bin/bash env
path_to_dump=$1
username=$2
password=$3

# from directory
mongorestore -d feedfarer $1 --username=$2 --password=$3 --authenticationDatabase=admin
# from archive
# mongorestore --archive=$path_to_dump