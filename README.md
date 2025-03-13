# Land Carbon Storage App

An interactive data visualization, analysis, and sharing application built using Shiny, Mapbox, and Google Earth Engine. Written primarily in R, but also utilizes JavaScript, HTML, CSS, and some shell.

Built to highlight the Walker et al. 2022 global potential carbon data set.

~~To deploy, make sure `dev = F` in [app.R](app.R), then source [deploy.R](deploy.R).~~

~~App currently deploys to [https://srgorelik.shinyapps.io/land-carbon-storage-app/](https://srgorelik.shinyapps.io/land-carbon-storage-app/).~~

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

- [https://medium.com/@hdpoorna/deploying-an-r-shiny-dashboard-on-gcp-cloud-run-c1c32a076783](https://medium.com/@hdpoorna/deploying-an-r-shiny-dashboard-on-gcp-cloud-run-c1c32a076783)
- [https://github.com/srgorelik/rshiny-gcp-template](https://github.com/srgorelik/rshiny-gcp-template)
- [https://github.com/r-spatial/rgee/issues/284](https://github.com/r-spatial/rgee/issues/284)
- [https://rocker-project.org/images/versioned/rstudio.html](https://rocker-project.org/images/versioned/rstudio.html)
- [https://github.com/rocker-org/rocker-versioned2/wiki/geospatial_6345e63d82d0](https://github.com/rocker-org/rocker-versioned2/wiki/geospatial_6345e63d82d0)
- [https://medium.com/@kakiang/mapping-a-cloud-run-service-to-a-custom-domain-9c9895037551](https://medium.com/@kakiang/mapping-a-cloud-run-service-to-a-custom-domain-9c9895037551)

```
# RETICULATE_PYTHON="/root/conda/envs/rgee_py/bin/python"
# EARTHENGINE_GCLOUD="/usr/bin/gcloud"
# EARTHENGINE_PYTHON="/root/conda/envs/rgee_py/bin/python"
# EARTHENGINE_INIT_MESSAGE="TRUE"
```
