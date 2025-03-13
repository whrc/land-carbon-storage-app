#!/bin/bash
set -e

# RUN ON MAC

# set GCP project ID
PROJECT_ID='woodwell-biomass'

# set GCP project region
REGION='us-central1'

# set GCP artifact registry repository name
REPO_NAME='rshiny-repo'

# set container image name
IMAGE_NAME='land-carbon-storage-app-image'

# set container image tag
IMAGE_TAG='v1.0'

# create image
docker build --platform=linux/amd64 --tag "${REGION}-docker.pkg.dev/${PROJECT_ID}/${REPO_NAME}/${IMAGE_NAME}:${IMAGE_TAG}" .

# create GCP artifact registry repository (only need to run once)
gcloud beta artifacts repositories create ${REPO_NAME} --repository-format=docker --location=${REGION} --project=${PROJECT_ID}

# add permission in order to push the image (only need to run once)
gcloud auth configure-docker "${REGION}-docker.pkg.dev" --project=${PROJECT_ID}

# push the image to the created repo
docker push "${REGION}-docker.pkg.dev/${PROJECT_ID}/${REPO_NAME}/${IMAGE_NAME}:${IMAGE_TAG}"
