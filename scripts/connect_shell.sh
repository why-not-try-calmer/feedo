#!/usr/bin/bash env
username=$1
password=$2

mongosh --username $username --password $password --authenticationDatabase admin