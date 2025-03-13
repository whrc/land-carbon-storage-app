#!/usr/bin/env Rscript

# packages
install.packages('shiny', version = '1.9.1', repos = 'https://cloud.r-project.org')
install.packages('shinyjs', version = '2.1.0', repos = 'https://cloud.r-project.org')
install.packages('shinyWidgets', version = '0.8.7', repos = 'https://cloud.r-project.org')
install.packages('shinybusy', version = '0.3.3', repos = 'https://cloud.r-project.org')
install.packages('shinyBS', version = '0.61.1', repos = 'https://cloud.r-project.org')
install.packages('phosphoricons', version = '0.2.1', repos = 'https://cloud.r-project.org')
install.packages('htmlwidgets', version = '1.6.4', repos = 'https://cloud.r-project.org')
install.packages('DT', version = '0.33', repos = 'https://cloud.r-project.org')
install.packages('viridisLite', version = '0.4.2', repos = 'https://cloud.r-project.org')
install.packages('viridis', version = '0.6.5', repos = 'https://cloud.r-project.org')
install.packages('dplyr', version = '1.1.4', repos = 'https://cloud.r-project.org')
install.packages('sf', version = '1.0-19', repos = 'https://cloud.r-project.org')
# install.packages('geojson', version = '0.3.5', repos = 'https://cloud.r-project.org')
# install.packages('geojsonio', version = '0.11.3', repos = 'https://cloud.r-project.org')
install.packages('geojsonsf', version = '2.0.3', repos = 'https://cloud.r-project.org')
# install.packages('reticulate', version = '1.41.0.1', repos = 'https://cloud.r-project.org')
# install.packages('future', version = '1.34.0', repos = 'https://cloud.r-project.org')

# rgee (see https://github.com/r-spatial/rgee?tab=readme-ov-file#installation)
install.packages('remotes', version = '2.5.0', repos = 'https://cloud.r-project.org')
install.packages('googledrive', version = '2.1.1', repos = 'https://cloud.r-project.org')
remotes::install_github(repo = 'r-spatial/rgee@e177ad5100669771772d454709dee96a13ca9b37', dependencies = T, upgrade = T)

# mapboxer
remotes::install_github(repo = 'walkerke/mapboxer', ref = 'gljs-v2')
