#!/bin/sh

curl -v localhost:8080/data

curl -v -X POST localhost:8080/data -H "Accept: application/json" -H "Content-type: application/json" -d '{"xs":[1,2],"ys":[3,4]}'

curl -v localhost:8080/data

curl -v localhost:8080/data/0/const

curl -v localhost:8080/data/0/constn
