#!/bin/bash


if curl --version ; then
  echo "Curl found"
else 
  echo "Curl not found"
  exit 1
fi
if [ -z "$1" ]
  then
    echo "url to shorten not provided as an argument"
    exit 1
fi

set -ex

curl -X POST -H "Content-Type: application/json" -v -d "{\"url\": \"$1\"}" localhost:8080/shorten

