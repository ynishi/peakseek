#!/bin/sh

set -eux

curl -v localhost:8080/data

curl -v -X DELETE localhost:8080/data/0

curl -v -X POST localhost:8080/data -H "Accept: application/json" -H "Content-type: application/json" -d '{"xs":[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10],"ys":[1,6,17,34,57,86,121,162,209,262,321]}'

curl -v localhost:8080/data | jq .

curl -v localhost:8080/data/0/const/3 | jq .

curl -v localhost:8080/data/0/constn/3 | jq .

curl -v localhost:8080/data/0/pred/1 | jq .
