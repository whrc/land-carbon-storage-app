# Land Carbon Storage App

An interactive data visualization, analysis, and sharing application built using Shiny, Mapbox, and Google Earth Engine. Hosted on Google Cloud Run.

Built to highlight the [Walker et al. 2022](https://www.pnas.org/doi/full/10.1073/pnas.2111312119) global potential carbon [dataset](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DSDDQK).

To build Docker image, make sure `dev = F` in [app/app.R](./app/app.R) before running [build.sh](./build.sh).

App currently runs on [https://land-carbon-storage-app-v1-1069052289734.us-central1.run.app/](https://land-carbon-storage-app-v1-1069052289734.us-central1.run.app/).

### Tips

Dockerfile developed interactively inside Docker Desktop terminal on MacOS using:

```
docker run --platform=linux/amd64 --rm -it rocker/geospatial:latest /bin/bash
```

Then to test run:

```
docker run --platform=linux/amd64 -d -p 8080:8080 "${REGION}-docker.pkg.dev/${PROJECT_ID}/${REPO_NAME}/${IMAGE_NAME}:${IMAGE_TAG}"
```

Helpful links:

- *Main one:* [https://medium.com/@hdpoorna/deploying-an-r-shiny-dashboard-on-gcp-cloud-run-c1c32a076783](https://medium.com/@hdpoorna/deploying-an-r-shiny-dashboard-on-gcp-cloud-run-c1c32a076783)
- [https://github.com/srgorelik/rshiny-gcp-template](https://github.com/srgorelik/rshiny-gcp-template)
- [https://github.com/r-spatial/rgee/issues/284](https://github.com/r-spatial/rgee/issues/284)
- [https://rocker-project.org/images/versioned/rstudio.html](https://rocker-project.org/images/versioned/rstudio.html)
- [https://github.com/rocker-org/rocker-versioned2/wiki/geospatial_6345e63d82d0](https://github.com/rocker-org/rocker-versioned2/wiki/geospatial_6345e63d82d0)
- [https://medium.com/@kakiang/mapping-a-cloud-run-service-to-a-custom-domain-9c9895037551](https://medium.com/@kakiang/mapping-a-cloud-run-service-to-a-custom-domain-9c9895037551)


