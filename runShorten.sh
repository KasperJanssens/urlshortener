#!/bin/bash


if curl --version ; then
  echo "Curl found"
else 
  echo "Curl not found"
  exit 1
fi

set -ex

curl -X POST -H "Content-Type: application/json" -v -d "{\"url\": \"joske\"}" localhost:8080/shorten

