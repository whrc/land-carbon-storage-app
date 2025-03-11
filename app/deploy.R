#!/usr/bin/env Rscript

### To deploy, make sure `dev = F` in `app.R` ###

library(reticulate)
library(rsconnect)
library(rgee)
library(shiny)

# 1. Create the credentials file
Sys.setenv("RETICULATE_PYTHON" = "~/miniforge3/envs/rgee/bin/python")
Sys.setenv("EARTHENGINE_GCLOUD" = "~/local/google-cloud-sdk/bin/gcloud")
ee_Authenticate(earthengine = T, drive = T, gcs = T)
ee_Initialize(project = 'woodwell-biomass')

# 2. Copy GEE credentials file to the project folder
file_credentials <- sprintf('%s/credentials', dirname(rgee::ee_get_earthengine_path()))
file.copy(file_credentials, to = '.')

# 3. Get ShinyApps.io account info
error_on_missing_name <- function(name) {
	var <- Sys.getenv(name, unset = NA)
	if(is.na(var)) {
		stop(paste0('cannot find ', name), call. = F)
	}
	gsub('\"', '', var)
}

setAccountInfo(
	name = error_on_missing_name('SHINY_ACC_NAME'),
	token = error_on_missing_name('TOKEN'),
	secret = error_on_missing_name('SECRET')
)

# 4. Run the application
deployApp(
	appFiles = c('app.R', 'utils.R', '.Renviron', 'credentials', 'main.css', 'woodwell-fonts.css', 'fonts', 'www', 'shp'),
	appTitle = 'land-carbon-storage-app',
	forceUpdate = T,
	lint = F
)

# 5. Delete GEE credentials file
file.remove('credentials')
