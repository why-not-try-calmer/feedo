#!/usr/bin/bash env
username=$1
password=$2
uri=$3

mongodump --uri mongodb+srv://$username:$password@$uri/feedfarer