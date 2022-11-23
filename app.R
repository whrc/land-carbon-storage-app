#!/usr/bin/env Rscript

# -------------------------------------------------------------------------------
# 1. OPTIONS
# -------------------------------------------------------------------------------

# development (TRUE) or deployment (FALSE)?
dev <- F

# print messages to console?
verbose <- F

# -------------------------------------------------------------------------------
# 2. Libraries
# -------------------------------------------------------------------------------

library(rgee)
library(shiny)
library(shinyWidgets)
library(sf)
library(dplyr)
# library(tidygeocoder)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(htmlwidgets)
library(htmltools)
library(viridis)
library(phosphoricons)
source('utils.R')

# -------------------------------------------------------------------------------
# 3. Set rgee
# -------------------------------------------------------------------------------

if (dev) {
	# ee_Initialize(gcs = T, drive = T)
	ee_Initialize()
} else {
	if (!reticulate::virtualenv_exists('rgee_py')) {
		set_rgee_dependencies()
	}
}

# -------------------------------------------------------------------------------
# 4. Run ee_Initialize() again after a long period of inactivity
# -------------------------------------------------------------------------------

tryCatch(
	expr = ee$Image(0),
	error = function(e) { ee_Initialize() }
)

# -------------------------------------------------------------------------------
# 5. Define App
# -------------------------------------------------------------------------------

ui <- bootstrapPage(

	tags$head(
		# tags$link(href = "https://fonts.googleapis.com/css?family=Teko", rel = "stylesheet"),
		# tags$link(rel = "stylesheet", type = "text/css", href = "www/fonts.css"),
		# tags$style(type = "text/css", "html, body {width: 100%; height: 100%; font-family: Sohne;}"),

		tags$style(
			type = 'text/css',
			'html, body {
				width: 100%; height: 100%;
			}
			img.woodwell {
				opacity: 0.3;
			}
			img.woodwell:hover {
				opacity: 0.7;
			}
			.btn.sidebar {
				display: block;
				height: 70px;
				width: 100%;
				border-radius: 0%;
				border: 0px;
				background-color: #EFEEEE;
			}
			.btn.closePanel {
				height: 10px;
				border-radius: 0%;
				border: 0px;
				background-color: #EFEEEE;
				padding-top: 0px;
				padding-right: 10px;
			}
			#appLogo {
				background-color: #36363C;
				height: 80px;
				padding: 8px;
			}
			#optionsPanel {
				height: 100%;
				width: 220px;
			}
			#optionsPanel.panelClose {
				background-color: #EFEEEE;
				height: 10px;
				width: 220px;
				text-align: right;
				padding: 0px;
			}
			#optionsPanel.panelHeader {
				display: table-cell;
				color: black;
				background-color: #EFEEEE;
				font-size: 18px;
				font-weight: 700;
				height: 70px;
				text-align: left;
				vertical-align: bottom;
				padding: 20px;
			}
			#optionsPanel.panelBody {
				background-color: white;
				padding: 20px;
			}
			.inputHeading {
				font-weight: 700;
				padding-top: 10px;
			}'
		),

		# use if app is loaded into iFrame on another website:
		tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js', type = 'text/javascript')
	),

	absolutePanel(
		top = 0,
		bottom = 0,
		left = 0,
		right = 'auto',
		width = '70px',
		height = '100%',
		fixed = T,
		draggable = F,
		style = 'background-color: #EFEEEE;',

		div(id = 'appLogo', img(src = 'app-logo.svg', width = '100%', height = '100%')),

		div(style = 'height: 15px; width: 100%;'),

		actionButton(
			inputId = 'lyr_btn',
			label = NULL,
			icon = ph_i('stack', weight = 'thin', size = '2x'),
			width = '100%',
			class = 'sidebar'
		),

		actionButton(
			inputId = 'loc_btn',
			label = NULL,
			icon = ph_i('map-pin-line', weight = 'thin', size = '2x'),
			width = '100%',
			class = 'sidebar'
		),

		actionButton(
			inputId = 'ply_btn',
			label = NULL,
			icon = ph_i('polygon', weight = 'thin', size = '2x'),
			width = '100%',
			class = 'sidebar'
		),

		actionButton(
			inputId = 'inf_btn',
			label = NULL,
			icon = ph_i('info', weight = 'thin', size = '2x'),
			width = '100%',
			class = 'sidebar'
		)

	),

	absolutePanel(
		top = 0,
		bottom = 0,
		left = 70,
		right = 0,
		width = 'auto',
		height = '100%',
		fixed = T,
		draggable = F,

		leafletOutput(outputId = 'map', width = '100%', height = '100vh'),

		absolutePanel(
			bottom = 10,
			left = 80,
			width = 80,
			height = 'auto',
			fixed = T,
			draggable = F,
			a(href='https://www.woodwellclimate.org/', img(src='woodwell-logo.png', class = 'woodwell', width = '80px', height = '34px'))
		),

		conditionalPanel(
			condition = "output.showPanel == 'lyr'",
			absolutePanel(
				id = 'optionsPanel',
				top = 0,
				left = '70px',
				fixed = T,
				draggable = F,

				div(id = 'optionsPanel', class = 'panelClose',
					actionButton(
						inputId = 'lyr_close',
						label = NULL,
						icon = ph_i('x', weight = 'thin', size = 'sm'),
						class = 'closePanel'
					)
				),

				div(id = 'optionsPanel', class = 'panelHeader',	'LAYERS'),

				div(id = 'optionsPanel', class = 'panelBody',

					p('Period', class = 'inputHeading'),
					prettyRadioButtons(
						inputId = 'radioTime',
						label = NULL,
						choices = c('Current' = 'CUR', 'Potential' = 'POT', 'Unrealized Potential' = 'UNR'),
						selected = 'UNR',
						shape = 'round',
						status = 'success',
						outline = F
					),

					p('Pool', class = 'inputHeading'),
					prettyCheckboxGroup(
						inputId = 'checkPool',
						label = NULL,
						choices = c('Aboveground' = 'AGB', 'Belowground' = 'BGB', 'Soil' = 'SOC'),
						selected = c('AGB', 'BGB', 'SOC'),
						shape = 'curve',
						icon = icon('check'),
						status = 'success',
						outline = F
					),

					p('Climate', class = 'inputHeading'),
					prettyRadioButtons(
						inputId = 'radioClim',
						label = NULL,
						choices = c('Baseline', 'RCP 8.5'),
						shape = 'round',
						status = 'success',
						outline = F
					),

					p('Overlays', class = 'inputHeading'),
					prettyCheckboxGroup(
						inputId = 'checkOverlays',
						label = NULL,
						choices = c('Countries', 'Ecoregions'),
						shape = 'curve',
						icon = icon('check'),
						status = 'success',
						outline = F
					),

					p('Options', class = 'inputHeading'),
					prettySwitch(
						inputId = 'switchConstraints',
						label = 'Constrain',
						value = F,
						status = 'success',
						fill = F,

					),

					setSliderColor('#69bd54', 1),
					sliderTextInput(
						inputId = 'opacitySlider',
						label = 'Opacity',
						choices = 0:100,
						selected = 100,
						hide_min_max = T,
						width = '100%',
						post = '%'
					)
				)
			)
		),

		conditionalPanel(
			condition = "output.showPanel == 'loc'",
			absolutePanel(
				id = 'optionsPanel',
				top = 0,
				left = '70px',
				fixed = T,
				draggable = F,

				div(id = 'optionsPanel', class = 'panelClose',
					actionButton(
						inputId = 'loc_close',
						label = NULL,
						icon = ph_i('x', weight = 'thin', size = 'sm'),
						class = 'closePanel'
					)
				),

				div(id = 'optionsPanel', class = 'panelHeader',	'INSPECT PIXEL'),

				div(id = 'optionsPanel', class = 'panelBody',
					'TBD...',
					div(id = 'search')
				)
			)
		),

		conditionalPanel(
			condition = "output.showPanel == 'ply'",
			absolutePanel(
				id = 'optionsPanel',
				top = 0,
				left = '70px',
				fixed = T,
				draggable = F,

				div(id = 'optionsPanel', class = 'panelClose',
					actionButton(
						inputId = 'ply_close',
						label = NULL,
						icon = ph_i('x', weight = 'thin', size = 'sm'),
						class = 'closePanel'
					)
				),

				div(id = 'optionsPanel', class = 'panelHeader',	'AREA OF INTEREST'),

				div(id = 'optionsPanel', class = 'panelBody',

					p('Upload Shapefile', class = 'inputHeading'),
					fileInput(
						inputId = 'shpFile',
						label = NULL,
						multiple = T,
						width = '100%',
						accept = c('.shp', '.shx', '.dbf', '.prj')
					)
				)
			)
		),

		conditionalPanel(
			condition = "output.showPanel == 'inf'",
			absolutePanel(
				id = 'optionsPanel',
				top = 0,
				left = '70px',
				fixed = T,
				draggable = F,

				div(id = 'optionsPanel', class = 'panelClose',
					actionButton(
						inputId = 'inf_close',
						label = NULL,
						icon = ph_i('x', weight = 'thin', size = 'sm'),
						class = 'closePanel'
					)
				),

				div(id = 'optionsPanel', class = 'panelHeader',	'INFO'),

				div(id = 'optionsPanel', class = 'panelBody', 'TBD...')
			)
		)

	)

)

# img.mgc <- img.mgcha$divide(21.46587)

# df.avg <- ee_extract(x = img.unr.mgcha, y = sp, scale = 500, fun = ee$Reducer$mean(), via = 'getInfo', sf = F)
# df.sum <- ee_extract(x = img.unr.mgc, y = sp, scale = 500, fun = ee$Reducer$sum(), via = 'getInfo', sf = F)
# df <- data.frame(mean_mgcha = df.avg$AGB_UNR, sum_mgc = df.sum$AGB_UNR)

server <- function(input, output, session) {

	rv <- reactiveValues(cur_sel = 'none', old_sel = 'none', show = 'none', cnt = -1)

	observeEvent(input$lyr_btn, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'lyr'
	})

	observeEvent(input$loc_btn, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'loc'
	})

	observeEvent(input$ply_btn, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'ply'
	})

	observeEvent(input$inf_btn, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'inf'
	})

	observeEvent(input$lyr_close | input$loc_close | input$ply_close | input$inf_close, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'none'
	})

	sidebarListen <- reactive({
		list(
			input$lyr_btn, input$loc_btn, input$ply_btn, input$inf_btn,
			input$lyr_close, input$loc_close, input$ply_close, input$inf_close
		)
	})

	observeEvent(sidebarListen(), {

		if (rv$cur_sel == rv$old_sel) {
			rv$cnt <- rv$cnt + 1
		} else {
			rv$cnt <- 0
		}

		if (rv$cur_sel == 'lyr' & (rv$old_sel != 'lyr' | rv$cnt %% 2 == 0)) {
			rv$show <- 'lyr'
		} else if (rv$cur_sel == 'loc' & (rv$old_sel != 'loc' | rv$cnt %% 2 == 0)) {
			rv$show <- 'loc'
		} else if (rv$cur_sel == 'ply' & (rv$old_sel != 'ply' | rv$cnt %% 2 == 0)) {
			rv$show <- 'ply'
		} else if (rv$cur_sel == 'inf' & (rv$old_sel != 'inf' | rv$cnt %% 2 == 0)) {
			rv$show <- 'inf'
		} else {
			rv$show <- 'none'
		}

		if (verbose) {
			message('======= SIDEBAR BUTTON PRESS =======')
			message(paste('lyr:', input$lyr_btn))
			message(paste('loc:', input$loc_btn))
			message(paste('ply:', input$ply_btn))
			message(paste('inf:', input$inf_btn))
			message(paste('old:', rv$old_sel))
			message(paste('cur:', rv$cur_sel))
			message(paste('cnt:', rv$cnt))
			message(paste('show:', rv$show))
			message('====================================')
		}
	})

	output$showPanel <- reactive(rv$show)
	outputOptions(output, 'showPanel', suspendWhenHidden = F)

	# search.out <- eventReactive(input$search, {
	# 	search.tbl <- geo(input$search, method = 'osm', full_results = F, quiet = T)
	# 	search.lat <- search.tbl %>% pull(lat)
	# 	search.lng <- search.tbl %>% pull(long)
	# 	paste0('lat:', lat, ', lng: ', lng)
	# })
	# output$latlng <- renderPrint({ search.out() })

	# define function to return shapefile from user file inputs
	shp <- reactive({

		# shpdf is a data.frame with the name, size, type and datapath of the uploaded files
		shpdf <- input$shpFile

		# name of the temporary directory where files are uploaded
		tempdirname <- dirname(shpdf$datapath[1])

		# rename files
		for (i in 1:nrow(shpdf)) {
			file.rename(shpdf$datapath[i], paste0(tempdirname, '/', shpdf$name[i]))
		}

		# return temporary shapefile filepath
		shp <- paste(tempdirname, shpdf$name[grep(pattern = '*.shp$', shpdf$name)], sep = '/')

		shp
	})

	map <- reactive({

		leaflet(options = leafletOptions(minZoom = 2, zoomControl = F, worldCopyJump = T)) %>%
			# addSearchOSM(options = searchOptions(
			# 	# container = 'search',
			# 	position = 'topright',
			# 	collapsed = T,
			# 	hideMarkerOnCollapse = T,
			# 	zoom = 7)
			# ) %>%
			# onRender("function(el, x) { $('input.search-input')[0].placeholder = 'Search...' }") %>%
			addProviderTiles(
				# provider = 'CartoDB.DarkMatter',
				provider = 'CartoDB.DarkMatterNoLabels',
				group = 'basemap',
				options = providerTileOptions(attribution = paste(c(
					"<a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
					"<a href='https://carto.com/attributions'>CARTO</a>",
					"<a href='https://www.naturalearthdata.com/'>Natural Earth</a>",
					"<a href='https://earthengine.google.com'>Google Earth Engine</a>",
					"<a href='https://shiny.rstudio.com'>R Shiny</a>"), collapse = ' | '))) %>%
			setView(lat = 20, lng = 10, zoom = 3) %>%
			setMaxBounds(lng1 = -360, lat1 = -80, lng2 = 360, lat2 = 90) %>%
			onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") %>%
			addEasyButton(easyButton(
				icon = 'fa-globe',
				title = 'Zoom to World',
				position = 'topright',
				onClick = JS('function(btn, map){ map.setView([20, 10], 3); }'))) %>%
			addEasyButton(easyButton(
				icon = 'fa-crosshairs',
				title = 'Locate Me',
				position = 'topright',
				onClick = JS('function(btn, map){ map.locate({setView: true}); }'))) %>%
			addFullscreenControl(position = 'topright') %>%
			addScaleBar(position = 'bottomright', options = scaleBarOptions(metric = T, imperial = F))

	})

	output$map <- renderLeaflet({
		map()
	})

	opacity <- reactive({
		as.numeric(input$opacitySlider) / 100
	})

	layerListen <- reactive({
		list(input$checkPool, input$radioTime, input$opacitySlider, input$switchConstraints)
	})

	observeEvent(layerListen(), {

		car.time <- input$radioTime
		car.pools <- input$checkPool
		n.pools <- length(car.pools)
		if (verbose) message('========== CHANGE LAYERS ===========')

		if (input$switchConstraints) {
			# original image values: 0=NoData/Mask, 1=cropland, 2=shifting agriculture, 3=grazing lands, 4=urban areas
			msk <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Zonal')$select('Societal_Constraints')$unmask(0)$eq(0)
		}

		if (n.pools == 1) {
			add2map <- T
			var.name <- paste0(car.pools, '_', car.time)
			if (verbose) message(var.name)
			img.mgcha <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Base_Clim')$select(var.name)$unmask(0)
			if (input$switchConstraints) img.mgcha <- img.mgcha$multiply(msk)
			img.mgcha <- img.mgcha$updateMask(img.mgcha$gt(0))
		} else if (n.pools == 2) {
			add2map <- T
			var.name.1 <- paste0(car.pools[1], '_', car.time)
			var.name.2 <- paste0(car.pools[2], '_', car.time)
			if (verbose) message(var.name.1)
			if (verbose) message(var.name.2)
			img.1.mgcha <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Base_Clim')$select(var.name.1)$unmask(0)
			img.2.mgcha <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Base_Clim')$select(var.name.2)$unmask(0)
			img.mgcha <- img.1.mgcha$add(img.2.mgcha)
			if (input$switchConstraints) img.mgcha <- img.mgcha$multiply(msk)
			img.mgcha <- img.mgcha$updateMask(img.mgcha$gt(0))
		} else if (n.pools == 3) {
			add2map <- T
			var.name.1 <- paste0(car.pools[1], '_', car.time)
			var.name.2 <- paste0(car.pools[2], '_', car.time)
			var.name.3 <- paste0(car.pools[3], '_', car.time)
			if (verbose) message(var.name.1)
			if (verbose) message(var.name.2)
			if (verbose) message(var.name.3)
			img.1.mgcha <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Base_Clim')$select(var.name.1)$unmask(0)
			img.2.mgcha <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Base_Clim')$select(var.name.2)$unmask(0)
			img.3.mgcha <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Base_Clim')$select(var.name.3)$unmask(0)
			img.mgcha <- img.1.mgcha$add(img.2.mgcha)$add(img.3.mgcha)
			if (input$switchConstraints) img.mgcha <- img.mgcha$multiply(msk)
			img.mgcha <- img.mgcha$updateMask(img.mgcha$gt(0))
		} else {
			add2map <- F
			if (verbose) message('no layer')
		}

		if (add2map) {

			max.val <- switch(
				car.time,
				'CUR' = 400,
				'POT' = 400,
				'UNR' = 100
			)

			col.pal <- switch(
				car.time,
				'CUR' = c('black', viridis(255)),
				'POT' = c('black', viridis(255)),
				'UNR' = c('black', magma(255))
			)

			m1 <- Map$addLayer(
				eeObject = img.mgcha,
				visParams = list(
					min = 0,
					max = max.val,
					palette = col.pal
				)
			)

			leafletProxy('map', session) %>%
				clearGroup(group = 'raster') %>%
				addTiles(
					urlTemplate = m1$rgee$tokens,
					layerId = 'carbonLayer',
					group = 'raster',
					options = tileOptions(opacity = opacity()))

		} else {

			leafletProxy('map', session) %>%
				clearGroup(group = 'raster')

		}
	})

	shpListen <- reactive({
		list(input$shpFile)
	})

	observeEvent(shpListen(), {
		req(input$shpFile)
		if (nrow(input$shpFile) > 1) {
			f <- shp()
			sp <- read_sf(f) %>%
				st_transform('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

			leafletProxy('map', session) %>%
				clearGroup(group = 'polygon') %>%
				addFeatures(sp, group = 'polygon', weight = 2, color = 'white', opacity = 0.8, fillOpacity = 0)

		}

	})

	overlayListen <- reactive({
		list(input$checkOverlays)
	})

	observeEvent(overlayListen(), {

		ovr.sel <- input$checkOverlays

		if (is.null(ovr.sel)) {

			leafletProxy('map', session) %>%
				addTiles(group = 'overlay', options = tileOptions(opacity = 0)) %>%
				clearGroup(group = 'overlay')

		} else if (ovr.sel == 'Countries') {

			polys <- read_sf('/Users/sgorelik/projects/pot-c-app/shp/ne_10m_admin_0_countries.shp') %>%
				st_transform('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

			leafletProxy('map', session) %>%
				clearGroup(group = 'overlay') %>%
				addFeatures(
					polys,
					group = 'overlay',
					weight = 1,
					color = 'white',
					opacity = 0.7,
					fillOpacity = 0,
					popup = ~htmlEscape(NAME),
					highlightOptions = highlightOptions(color = 'white', weight = 2, bringToFront = T))


			# polys <- ee$FeatureCollection('USDOS/LSIB/2017')
			#
			# polys.style <- polys$style(
			# 	width = 0.5,
			# 	color = 'ffffffcc', # outline color = white, alpha = 80% transparent
			# 	fillColor = 'ffffff00' # fill color = white, alpha = 100% transparent
			# )
			#
			# m2 <- Map$addLayer(eeObject = polys.style, visParams = {})
			#
			# leafletProxy('map', session) %>%
			# 	clearGroup(group = 'overlay') %>%
			# 	addTiles(
			# 		urlTemplate = m2$rgee$tokens,
			# 		layerId = 'overlay',
			# 		group = 'overlay',
			# 		options = tileOptions(opacity = 1))

		} else if (ovr.sel == 'Ecoregions') {

			polys <- ee$FeatureCollection('RESOLVE/ECOREGIONS/2017')

			polys.style <- polys$style(
				width = 0.5,
				color = 'ffffffcc', # outline color = white, alpha = 80% transparent
				fillColor = 'ffffff00' # fill color = white, alpha = 100% transparent
			)

			m2 <- Map$addLayer(eeObject = polys.style, visParams = {})

			leafletProxy('map', session) %>%
				clearGroup(group = 'overlay') %>%
				addTiles(
					urlTemplate = m2$rgee$tokens,
					layerId = 'overlay',
					group = 'overlay',
					options = tileOptions(opacity = 1))

		} else {

		}
	})

}

shinyApp(ui = ui, server = server)
