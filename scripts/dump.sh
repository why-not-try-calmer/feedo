#!/usr/bin/bash env
username=$1
password=$2
uri=$3

# remote hosts
mongodump --uri mongodb+srv://$username:$password@$uri/feedfarer

# local host
# mongodump -h="localhost" -u=$username -p=$password --authenticationDatabase=admin --archive=dump_archived