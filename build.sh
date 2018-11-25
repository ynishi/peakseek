#!/bin/sh

set -eux

stack build

stack image container
docker tag ynishi/peakseek gcr.io/$PROJECT_ID/peakseek
gcloud docker -- push gcr.io/$PROJECT_ID/peakseek

docker build -t ynishi/peakseek-db db
docker tag ynishi/peakseek-db gcr.io/$PROJECT_ID/peakseek-db
gcloud docker -- push gcr.io/$PROJECT_ID/peakseek-db
