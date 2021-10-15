#!/bin/bash


if curl --version ; then
  echo "Curl found"
else 
  echo "Curl not found"
  exit 1
fi

if [ -z "$1" ]
  then
    echo "Shortend url not provided as argument"
    exit 1
fi

set -ex


curl -X GET -v localhost:8080/${1}

