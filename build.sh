#!/bin/sh

stack image container
docker tag ynishi/peakseek gcr.io/$PROJECT_NAME/peakseek
gcloud docker -- push gcr.io/$PROJECT_NAME/peakseek