setup_rgee <- function() {

	library(reticulate)
	use_condaenv(condaenv = 'rgee_py', conda = '/opt/conda/bin/conda', required = T)

	py.code <- glue::glue(
		"
		import ee
		service_account = 'rshiny-cloud-run@woodwell-biomass.iam.gserviceaccount.com'
		key = '/opt/.config/gcloud/application_default_credentials.json'
		credentials = ee.ServiceAccountCredentials(service_account, key)
		ee.Initialize(credentials)
		print(ee.String('Hello from the Earth Engine servers!').getInfo())
		"
	)

	py_run_string(py.code)

	# test
	ee$String('Hello from the Earth Engine servers!')$getInfo()

}

# see https://github.com/r-spatial/rgee/issues/367
ee_extract <- function (x, y, fun = ee$Reducer$mean(), scale = NULL, sf = FALSE,
						via = "getInfo", container = "rgee_backup", lazy = FALSE,
						quiet = FALSE, ...)
{
	rgee:::ee_check_packages("ee_extract", c("geojsonio", "sf"))
	if (!quiet & is.null(scale)) {
		scale <- 1000
		message(sprintf("The image scale is set to %s.", scale))
	}
	if (!any(class(x) %in% rgee:::ee_get_spatial_objects("i+ic"))) {
		stop("x is neither an ee$Image nor ee$ImageCollection")
	}
	if (any(class(x) %in% "ee.imagecollection.ImageCollection")) {
		x <- ee$ImageCollection$toBands(x)
	}
	oauth_func_path <- system.file("python/ee_extract.py", package = "rgee")
	extract_py <- rgee:::ee_source_python(oauth_func_path)
	sp_objects <- rgee:::ee_get_spatial_objects("Table")
	if (!any(class(y) %in% c("sf", "sfc", sp_objects))) {
		stop("y is not a sf, sfc, ee$Geometry, ee$Feature or ee$FeatureCollection object.")
	}
	if (any("sf" %in% class(y))) {
		sf_y <- y
		ee_y <- sf_as_ee(y[[attr(y, "sf_column")]], quiet = TRUE)
	}
	else if (any("sfc" %in% class(y))) {
		sf_y <- sf::st_sf(id = seq_along(y), geometry = y)
		ee_y <- sf_as_ee(y, quiet = TRUE)
	}
	else if (any(ee_get_spatial_objects("Table") %in% class(y))) {
		ee_y <- ee$FeatureCollection(y)
		sf_y <- tryCatch(expr = ee_as_sf(y, quiet = FALSE, maxFeatures = 10000),
						 error = function(e) {
						 	stop("The ee$FeatureCollection (y) must be not higher than 10 000.")
						 })
	}
	ee_add_rows <- function(f) {
		f_prop <- ee$Feature$get(f, "system:index")
		ee$Feature(ee$Feature$set(f, "ee_ID", f_prop))
	}
	ee_y <- ee$FeatureCollection(ee_y) %>% ee$FeatureCollection$map(ee_add_rows)
	fun_name <- gsub("Reducer.", "", (ee$Reducer$getInfo(fun))[["type"]])
	x_ic <- rgee:::bands_to_image_collection(x)
	create_tripplets <- function(img) {
		img_reduce_regions <- img$reduceRegions(collection = ee_y, reducer = fun, scale = scale,
												...)
		ee$FeatureCollection$map(img_reduce_regions, function(f) {
			ee$Feature$set(f, "imageId", ee$Image$get(img, "system:index"))
		})
	}
	triplets <- x_ic %>% ee$ImageCollection$map(create_tripplets) %>%
		ee$ImageCollection$flatten()
	table <- extract_py$table_format(triplets, "ee_ID", "imageId",
									 fun_name)$map(function(feature) {
									 	ee$Feature$setGeometry(feature, NULL)
									 })
	if (via == "drive") {
		table_id <- basename(tempfile("rgee_file_"))
		ee_user <- ee_exist_credentials()
		dsn <- sprintf("%s/%s.csv", tempdir(), table_id)
		table_task <- ee_init_task_drive_fc(x_fc = table, dsn = dsn,
											container = container, table_id = table_id, ee_user = ee_user,
											selectors = NULL, timePrefix = TRUE, quiet = quiet)
		if (lazy) {
			prev_plan <- future::plan(future::sequential, .skip = TRUE)
			on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
			future::future({
				ee_extract_to_lazy_exp_drive(table_task, dsn,
											 quiet, sf, sf_y)
			}, lazy = TRUE)
		}
		else {
			ee_extract_to_lazy_exp_drive(table_task, dsn, quiet,
										 sf, sf_y)
		}
	}
	else if (via == "gcs") {
		table_id <- basename(tempfile("rgee_file_"))
		ee_user <- ee_exist_credentials()
		dsn <- sprintf("%s/%s.csv", tempdir(), table_id)
		table_task <- ee_init_task_gcs_fc(x_fc = table, dsn = dsn,
										  container = container, table_id = table_id, ee_user = ee_user,
										  selectors = NULL, timePrefix = TRUE, quiet = quiet)
		if (lazy) {
			prev_plan <- future::plan(future::sequential, .skip = TRUE)
			on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
			future::future({
				ee_extract_to_lazy_exp_gcs(table_task, dsn,
										   quiet, sf, sf_y)
			}, lazy = TRUE)
		}
		else {
			ee_extract_to_lazy_exp_gcs(table_task, dsn, quiet,
									   sf, sf_y)
		}
	}
	else {
		table_geojson <- table %>% ee$FeatureCollection$getInfo() %>%
			ee_utils_py_to_r()
		class(table_geojson) <- "geo_list"
		table_sf <- geojsonio::geojson_sf(table_geojson)
		sf::st_geometry(table_sf) <- NULL
		table_sf <- table_sf[, order(names(table_sf))]
		table_sf["id"] <- NULL
		table_sf["ee_ID"] <- NULL
		if (isTRUE(sf)) {
			table_geometry <- sf::st_geometry(sf_y)
			table_sf <- sf_y %>% sf::st_drop_geometry() %>%
				cbind(table_sf) %>% sf::st_sf(geometry = table_geometry)
		}
		else {
			table_sf <- sf_y %>% sf::st_drop_geometry() %>%
				cbind(table_sf)
		}
		table_sf
	}
}
