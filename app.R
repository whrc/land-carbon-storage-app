# -------------------------------------------------------------------------------
# 1. Development/Deployment Options
# -------------------------------------------------------------------------------

# development (T) or deployment (F)?
dev <- T

# print messages to console?
verbose <- F

# -------------------------------------------------------------------------------
# 2. Libraries
# -------------------------------------------------------------------------------

# UI
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinybusy)
library(shinyBS)
library(phosphoricons)
library(htmlwidgets)
source('utils.R')

# mapping/analysis
library(rgee)
library(viridisLite)
library(viridis)
library(sf)
library(dplyr)
library(geojson) # not sure if this is needed anymore...
library(geojsonio) # needed for ee_extract()
library(geojsonsf) # needed to convert mapbox javascript return to sf object

### see https://github.com/crazycapivara/mapboxer/pull/104 ###
# devtools::install_github(repo = 'walkerke/mapboxer', ref = 'gljs-v2')
library(mapboxer)
mapboxer_use_v2(T)

# -------------------------------------------------------------------------------
# 3. Set rgee
# -------------------------------------------------------------------------------

if (dev) {
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
# 5. Get API Keys and Access Tokens
# -------------------------------------------------------------------------------

# get token from .Renviron
readRenviron('.Renviron')
mb.token <- Sys.getenv('MAPBOX_ACCESS_TOKEN')

# -------------------------------------------------------------------------------
# 6. Install Woodwell Fonts on ShinyApps.io Ubuntu server
# -------------------------------------------------------------------------------

if (!dev) {
	dir.create('~/.fonts')
	font.files <- list.files(path = 'fonts', full.names = T)
	file.copy(font.files, '~/.fonts')
	system('ls ~/.fonts')
	system('fc-cache -f ~/.fonts')
}

# -------------------------------------------------------------------------------
# 7. App Startup Parameters
# -------------------------------------------------------------------------------

START.VIEW <- 'GLOBE'
START.PERIOD <- 'CUR'
START.POOL <- c('AGB', 'BGB', 'SOC')
START.CLIMATE <- 'Base_Clim'
START.CONSTRAIN <- F

# -------------------------------------------------------------------------------
# 8. Define App
# -------------------------------------------------------------------------------

pal.cols <- if (START.PERIOD == 'UNR') { magma(256) } else { viridis(256) }
pal.style <- paste0('linear-gradient(0deg, #000000, ', paste0(pal.cols, collapse = ', '), ')')

sp.lines <- read_sf('shp/ne_10m_geographic_lines_WGS84_waterOnly.shp')
sp.countries <- read_sf('shp/ne_10m_admin_0_countries_WGS84_small.shp')
sp.bioclim <- read_sf('shp/gez_2010_wgs84_dissolved_small.shp') %>%
	mutate(fill_color = case_when(
		group == 'Polar' ~ '#ffffcc',
		group == 'Boreal' ~ '#a1dab4',
		group == 'Temperate' ~ '#41b6c4',
		group == 'Subtropical' ~ '#2c7fb8',
		group == 'Tropical' ~ '#253494'
	))

logo.href.path <- ifelse(dev, '/', '/land-carbon-storage-app')

globe.icon <- ph_i('globe', weight = 'bold', style = 'vertical-align: -0.26em;')
flat.icon <- ph_i('map-trifold', weight = 'bold', style = 'vertical-align: -0.26em;')
if (START.VIEW == 'globe') start.icon <- flat.icon else start.icon <- globe.icon

ui <- bootstrapPage(

	useShinyjs(),

	busy_start_up(
		loader = spin_kit(spin = 'cube-grid', color = '#FF5700', style = 'width: 50px; height: 50px; position: relative;'),
		text = 'Loading Platform...',
		mode = 'auto',
		color = '#EFEEEE',
		background = 'black'
	),

	add_busy_bar(
		color = '#FF5700', centered = T
	),

	tags$head(
		tags$link(rel = 'shortcut icon', href = 'favicon.ico'),
		includeCSS('main.css'),
		includeCSS('woodwell-fonts.css'),
		tags$style(
			type = 'text/css',
			paste0(
				'#colorRamp {
					width: 100%;
					height: 100%;
					background: ', pal.style, '
				}')
		),
		tags$script("
			Shiny.addCustomMessageHandler('projection', function(proj) {
				const map = mapboxer._widget['map'].map;
				map.setProjection(proj);
			});
		"),
		tags$script("
			Shiny.addCustomMessageHandler('legend', function(message) {
				document.getElementById('colorRamp').style.background = message.style;
				document.getElementById('item-1').innerHTML = message.item1;
				document.getElementById('item-2').innerHTML = message.item2;
				document.getElementById('item-3').innerHTML = message.item3;
				document.getElementById('item-4').innerHTML = message.item4;
				document.getElementById('item-5').innerHTML = message.item5;
			});
		"),
		tags$script("
			Shiny.addCustomMessageHandler('moveLayerBeforeLayer', function(message) {
				const map = mapboxer._widget['map'].map;
				map.moveLayer(message.layerID, message.beforeID);
			});
		"),
		tags$script("
			Shiny.addCustomMessageHandler('moveLayerToTop', function(layerID) {
				const map = mapboxer._widget['map'].map;
				map.moveLayer(layerID);
			});
		"),
		# tags$script("
		# 	Shiny.addCustomMessageHandler('closeTooltips', function(name) {
		# 		function myFunction() {
		# 			document.getElementById(name).click();
		# 		}
		# 	});
		# "),

		# for mapbox draw control
		tags$script(src = 'https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-draw/v1.3.0/mapbox-gl-draw.js'),
		tags$link(rel = 'stylesheet', href = 'https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-draw/v1.3.0/mapbox-gl-draw.css'),

		# for mapbox geocoding api
		tags$script(src = 'https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-geocoder/v5.0.0/mapbox-gl-geocoder.min.js'),
		tags$link(rel = 'stylesheet', href = 'https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-geocoder/v5.0.0/mapbox-gl-geocoder.css', type = 'text/css')

	),
	tags$head(uiOutput('cursor_css')),

	absolutePanel(
		id = 'sidebarPanel',
		top = 0,
		bottom = 0,
		left = 0,
		right = 'auto',
		width = '70px',
		height = '100%',
		fixed = T,
		draggable = F,

		div(id = 'appLogo', a(img(src = 'app-logo.svg', width = '100%', height = '100%'), href = logo.href.path)), # using an image allows it to refresh app on click

		div(style = 'height: 15px; width: 100%;'),

		actionButton(
			inputId = 'lyr_btn',
			label = NULL,
			icon = ph_i('stack', weight = 'thin', size = '2x'),
			width = '100%',
			class = 'sidebar'
		),

		actionButton(
			inputId = 'anl_btn',
			label = NULL,
			icon = ph_i('chart-bar-horizontal', weight = 'thin', size = '2x'),
			width = '100%',
			class = 'sidebar'
		),

		actionButton(
			inputId = 'loc_btn',
			label = NULL,
			icon = ph_i('map-pin', weight = 'thin', size = '2x'),
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

		mapboxerOutput(outputId = 'map', width = '100%', height = '100%'),

		absolutePanel(
			top = 10,
			right = 10,
			style = 'float: right; padding: 4px; border-radius: 4px; background-color: #fff;',
			actionButton(
				inputId = 'view',
				label = NULL,
				icon = start.icon,
				class = 'customControl'
			),
			div(id = 'navControl', style = 'padding: 0; outline: none; border: 0; box-shadow: none;')
		),

		absolutePanel(
			bottom = 10,
			left = 80,
			width = 80,
			height = 'auto',
			fixed = T,
			draggable = F,
			a(href = 'https://www.woodwellclimate.org/',  target = '_blank', img(src = 'woodwell-logo.png', class = 'woodwell', width = '80px', height = '34px'))
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
						inputId = 'period',
						label = NULL,
						choices = c('Current' = 'CUR', 'Potential' = 'POT', 'Unrealized Potential' = 'UNR'),
						selected = START.PERIOD,
						shape = 'round',
						status = 'success',
						outline = F
					),

					p('Pool', class = 'inputHeading'),
					prettyCheckboxGroup(
						inputId = 'pool',
						label = NULL,
						choices = c('Aboveground' = 'AGB', 'Belowground' = 'BGB', 'Soil' = 'SOC'),
						selected = START.POOL,
						shape = 'curve',
						icon = icon('check'),
						status = 'success',
						outline = F
					),

					p('Climate', class = 'inputHeading'),
					prettyRadioButtons(
						inputId = 'climate',
						label = NULL,
						choices = c('Baseline' = 'Base_Clim', 'RCP 8.5' = 'RCP85'),
						selected = START.CLIMATE,
						shape = 'round',
						status = 'success',
						outline = F
					),

					p('Options', class = 'inputHeading'),
					div(style = 'display: flex; align-items: end; margin-bottom: 4px;',
						setSliderColor(color = '#69bd54', sliderId = 1),
						tagAppendAttributes(
							sliderTextInput(
								inputId = 'transparency',
								label = NULL,
								choices = 0:100,
								selected = 0,
								hide_min_max = T,
								width = '60px',
								post = '%',
								force_edges = T
							),
							style = 'margin-top: -18px; margin-bottom: 4px;'
						),
						p('Transparency', style = 'margin-bottom: 5px; margin-left: 8px;')
					),
					div(class = 'infoTipGrid',
						tagAppendAttributes(
							prettySwitch(
								inputId = 'constrain',
								label = 'Constrain',
								value = F,
								status = 'success',
								fill = F,
								width = '100%'
							),
							style = 'margin-right: 0px;'
						),
						ph_i('info', weight = 'fill', size = '1x', color = '#828282', id = 'infoConstraints')
					),

					p('Overlays', class = 'inputHeading'),
					div(class = 'infoTipGrid',
						tagAppendAttributes(
							prettySwitch(
								inputId = 'countries',
								label = 'Countries',
								value = F,
								status = 'success',
								fill = F,
								width = '100%'
							),
							style = 'margin-right: 0px; margin-bottom: 4px;'
						),
						ph_i('info', weight = 'fill', size = '1x', color = '#828282', id = 'infoCountries'),
						tagAppendAttributes(
							prettySwitch(
								inputId = 'places',
								label = 'Places',
								value = F,
								status = 'success',
								fill = F,
								width = '100%'
							),
							style = 'margin-right: 0px; margin-bottom: 4px;'
						),
						div(),
						tagAppendAttributes(
							prettySwitch(
								inputId = 'imagery',
								label = 'Imagery',
								value = F,
								status = 'success',
								fill = F,
								width = '100%'
							),
							style = 'margin-right: 0px; margin-bottom: 4px;'
						),
						div(),
						tagAppendAttributes(
							prettySwitch(
								inputId = 'bioclimzones',
								label = 'Bioclimate Zones',
								value = F,
								status = 'success',
								fill = F,
								width = '100%'
							),
							style = 'margin-right: 0px;'
						)
					)
				)
			)
		),

		conditionalPanel(
			condition = "output.showPanel == 'anl'",
			absolutePanel(
				id = 'optionsPanel',
				top = 0,
				left = '70px',
				fixed = T,
				draggable = F,

				div(id = 'optionsPanel', class = 'panelClose',
					actionButton(
						inputId = 'anl_close',
						label = NULL,
						icon = ph_i('x', weight = 'thin', size = 'sm'),
						class = 'closePanel'
					)
				),

				div(id = 'optionsPanel', class = 'panelHeader',	'ANALYSIS'),

				div(id = 'optionsPanel', class = 'panelBody',
					bsCollapse(
						id = 'analysisCollapsePanel',
						multiple = F,
						open = 'Inspect Pixel',
						bsCollapsePanel(
							title = 'Inspect Pixel',
							p('Click a pixel on the map to return it\'s value below.', style = 'padding-bottom: 6px;'),
							tableOutput(outputId = 'pixelInspector')
						),
						bsCollapsePanel(
							title = 'Draw Shape',
							p('1. Click the polygon icon below to enable the drawing tool.'),
							p('2. Click on the map to select your first vertex.'),
							p('3. Click to create additional vertices.'),
							p('4. Double-click to add your last vertex, and the polygon will be created.'),
							p('5. To analyze the carbon within your polygon, click the calculator icon.'),
							p('6. To delete the polygon, select it and then click the trash icon.', style = 'padding-bottom: 6px;'),
							div(style = 'display: flex;',
								div(id = 'draw', class = 'drawControls'),
								actionButton(
									inputId = 'runDrawnPoly',
									label = NULL,
									icon = ph_i('calculator', weight = 'thin', size = '3x'),
									class = 'calcButton'
								)
							),
							tableOutput(outputId = 'drawnPolyResults')
						),
						bsCollapsePanel(
							title = 'Upload Shapefile',
							p('Upload a polygon shapefile to analyze the carbon within it\'s boundaries. Results will show below.', style = 'padding-bottom: 6px;'),
							p('Note, when selecting a .shp file, be sure to select the related .dbf, .shx and .prj files (i.e., you must provide exactly 4 files).', style = 'font-style: italic; padding-bottom: 6px;'),
							fileInput(
								inputId = 'shapefile',
								label = NULL,
								multiple = T,
								width = '100%',
								accept = c('.shp', '.shx', '.dbf', '.prj')
							),
							textOutput(outputId = 'inputFilesMessage'),
							textOutput(outputId = 'inputFilesError'),
							br(),
							textOutput(outputId = 'shpTypeError'),
							tableOutput(outputId = 'shapefileResults')
						)
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

				div(id = 'optionsPanel', class = 'panelHeader',	'SEARCH'),

				div(id = 'optionsPanel', class = 'panelBody', style = 'text-align: left;',
					div(id = 'searchbox')
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

				div(id = 'optionsPanel', class = 'panelBody', style = 'text-align: left;',

					HTML("
						<p class='inputHeading'>About the Site</p>
						<p>
							The Land Carbon Storage Platform is an interactive data visualization, analysis, and sharing application built by:
						</p>
						<a href='https://www.woodwellclimate.org/' target='_blank'>
							<img src='woodwell-logo-color-text.png' class='woodwellBig' style='width: 140px; height: auto;'>
						</a>
						<br>
						<p class='inputHeading'>About the Data</p>
						<p>
							This site contains a first-of-its-kind comprehensive data set on the global potential for increased storage of carbon on land.
							This data set was produced by Walker et al. (2022) and is described in detail in the
							<a href='https://doi.org/10.1073/pnas.2111312119' target='_blank'>Proceedings of the National Academy of Sciences</a>.
						</p>
						<p>
							The units of the carbon density maps shown here are megagrams of carbon per hectare (Mg C ha<sup>-1</sup>).
							The data set has been prepared at a spatial resolution of ca. 500 meters in the MODIS sinusoidal map projection.
						</p>
						<p>
							Data can be downloaded in GeoTIFF format from the
							<a href='https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DSDDQK' target='_blank'>Harvard Dataverse</a>.
						</p>
						<p class='inputHeading'>Contact</p>
						<div class='contactGrid'>
							<i class='ph-user'></i>
							<div>Seth Gorelik</div>
							<i class='ph-envelope-simple'></i>
							<a href='mailto:sgorelik@woodwellclimate.org?subject=Land Carbon Storage Platform Inquiry&cc=wwalker@woodwellclimate.org' target='_blank'>sgorelik@woodwellclimate.org</a>
						</div>
					")
				)
			)
		)

	),
	bsTooltip(
		id = 'infoConstraints',
		title = 'Mask areas critical to food production and human habitation. For more information on how these societal constraints were mapped, see <a href="https://doi.org/10.1073/pnas.2111312119" target="_blank">Walker et al. (2022)</a>.',
		placement = 'top',
		trigger = 'hover',
		options = list(container = 'body', delay = list(show = 0, hide = 0))
	),
	bsTooltip(
		id = 'infoCountries',
		title = 'Display country boundaries (with simplified borders for performance). Source: <a href="https://www.naturalearthdata.com/" target="_blank">Natural Earth</a>.',
		placement = 'top',
		trigger = 'hover',
		options = list(container = 'body', delay = list(show = 0, hide = 0))
	),

	# title for web browser tab
	title = 'Land Carbon Storage App'

)

# define function to return either mapbox source or EE image object for user selected carbon layer
gee.src <- function(pool = START.POOL, period = START.PERIOD, climate = START.CLIMATE, constrain = START.CONSTRAIN, returnEEobj = F) {

	n.pools <- length(pool)

	if ((constrain) & (n.pools > 0)) {
		# original image values: 0=NoData/Mask, 1=cropland, 2=shifting agriculture, 3=grazing lands, 4=urban areas
		msk <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Zonal')$select('Societal_Constraints')$unmask(0)$eq(0)
	}

	if (n.pools == 1) {
		var.name <- paste0(pool, '_', period)
		img.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', climate))$select(var.name)$unmask(0)
		if (constrain) img.mgcha <- img.mgcha$multiply(msk)
		img.mgcha <- img.mgcha$updateMask(img.mgcha$gt(0))
	} else if (n.pools == 2) {
		var.name.1 <- paste0(pool[1], '_', period)
		var.name.2 <- paste0(pool[2], '_', period)
		img.1.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', climate))$select(var.name.1)$unmask(0)
		img.2.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', climate))$select(var.name.2)$unmask(0)
		img.mgcha <- img.1.mgcha$add(img.2.mgcha)
		if (constrain) img.mgcha <- img.mgcha$multiply(msk)
		img.mgcha <- img.mgcha$updateMask(img.mgcha$gt(0))
	} else if (n.pools == 3) {
		var.name.1 <- paste0(pool[1], '_', period)
		var.name.2 <- paste0(pool[2], '_', period)
		var.name.3 <- paste0(pool[3], '_', period)
		img.1.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', climate))$select(var.name.1)$unmask(0)
		img.2.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', climate))$select(var.name.2)$unmask(0)
		img.3.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', climate))$select(var.name.3)$unmask(0)
		img.mgcha <- img.1.mgcha$add(img.2.mgcha)$add(img.3.mgcha)
		if (constrain) img.mgcha <- img.mgcha$multiply(msk)
		img.mgcha <- img.mgcha$updateMask(img.mgcha$gt(0))
	}

	if (n.pools > 0) {
		max.val <- switch(
			period,
			'CUR' = 400,
			'POT' = 400,
			'UNR' = 100
		)

		col.pal <- switch(
			period,
			'CUR' = c('black', viridis(256)),
			'POT' = c('black', viridis(256)),
			'UNR' = c('black', magma(256))
		)

		gee.lyr <- Map$addLayer(
			eeObject = img.mgcha,
			visParams = list(
				min = 0,
				max = max.val,
				palette = col.pal
			)
		)

		img.src <- mapbox_source(
			type = 'raster',
			tiles = list(gee.lyr$rgee$tokens),
			tileSize = 256
		)

		if (verbose) message(paste(c(pool, period, climate, constrain), collapse = '_'))

	} else {
		img.src <- NULL
	}

	if (returnEEobj) {
		result <- img.mgcha
	} else {
		result <- img.src
	}

	return(result)
}

# define a function to verify that there are exactly 4 files
# provided by user and they are a .shp, .shx, .prj, and .dbf
check_input_files <- function(shpdf) {
	files <- as.character(shpdf$name)
	usr.exts <- tools::file_ext(files)
	req.exts <- c('shp', 'shx', 'dbf', 'prj')
	if (length(files) == 4 & all(req.exts %in% usr.exts)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

# define function to get feature type of shapefile without reading shapefile into memory
check_shp_type <- function(shp) {
	wkb.opts <- c('wkbPoint', 'wkbLineString', 'wkbPolygon', 'wkbMultiPoint', 'wkbMultiLineString', 'wkbMultiPolygon', 'wkbGeometryCollection')
	shp.info <- rgdal::ogrInfo(shp)
	shp.wkb <- wkb.opts[shp.info$eType]
	return(gsub('wkb', '', shp.wkb))
}

server <- function(input, output, session) {

	# ---------------------------------------
	# sidebar selection
	# ---------------------------------------

	rv <- reactiveValues(cur_sel = 'none', old_sel = 'none', show = 'none', cnt = -1, crbn_lyr_cnt = 0)

	observeEvent(input$lyr_btn, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'lyr'
	})

	observeEvent(input$anl_btn, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'anl'
	})

	observeEvent(input$loc_btn, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'loc'
	})

	observeEvent(input$inf_btn, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'inf'
	})

	observeEvent(input$lyr_close | input$anl_close | input$loc_close | input$inf_close, {
		rv$old_sel <- rv$cur_sel
		rv$cur_sel <- 'none'
	})

	sidebarListen <- reactive({
		list(
			input$lyr_btn, input$anl_btn, input$loc_btn, input$inf_btn,
			input$lyr_close, input$anl_close, input$loc_close, input$inf_close
		)
	})

	observeEvent(sidebarListen(), {

		# shinyjs::hide('infoConstraints', anim = T)
		# shinyjs::hide('infoCountries', anim = T)

		# session$sendCustomMessage('closeTooltips', 'infoConstraints')

		if (rv$cur_sel == rv$old_sel) {
			rv$cnt <- rv$cnt + 1
		} else {
			rv$cnt <- 0
		}

		if (rv$cur_sel == 'lyr' & (rv$old_sel != 'lyr' | rv$cnt %% 2 == 0)) {
			rv$show <- 'lyr'
		} else if (rv$cur_sel == 'anl' & (rv$old_sel != 'anl' | rv$cnt %% 2 == 0)) {
			rv$show <- 'anl'
		} else if (rv$cur_sel == 'loc' & (rv$old_sel != 'loc' | rv$cnt %% 2 == 0)) {
			rv$show <- 'loc'
		} else if (rv$cur_sel == 'inf' & (rv$old_sel != 'inf' | rv$cnt %% 2 == 0)) {
			rv$show <- 'inf'
		} else {
			rv$show <- 'none'
		}

	})

	output$showPanel <- reactive(rv$show)
	outputOptions(output, 'showPanel', suspendWhenHidden = F)

	# ---------------------------------------
	# define map
	# ---------------------------------------

	output$map <- renderMapboxer({

		# initialize map with globe view and dark basemap
		mapboxer(token = mb.token,
				 projection = 'globe',
				 style = 'mapbox://styles/sgorelik/clb4ehzzl000o14mlqz2cumla',
				 renderWorldCopies = F,
				 logoPosition = 'bottom-left',
				 customAttribution = "<a href='https://earthengine.google.com' target='_blank'>Google Earth Engine</a> | <a href='https://shiny.rstudio.com' target='_blank'>Shiny</a>",
				 zoom = 2.0,
				 center = c(-10, 10)) %>%
			# add equator and tropics lines
			add_source(as_mapbox_source(sp.lines), id = 'lat_lines') %>%
			add_line_layer(
				source = 'lat_lines',
				line_color = '#42433B',
				line_width = 0.5,
				line_opacity = 0.6,
				line_dasharray = list(4, 4),
				visibility = T
			) %>%
			# add imagery but don't make visible yet
			add_layer(style = list(
				id = 'imagery',
				type = 'raster',
				source = mapbox_source(
					type = 'raster',
					url = 'mapbox://mapbox.satellite',
					tileSize = 256
				),
				layout = list(visibility = 'none')
			)) %>%
			# add initial carbon layer
			add_layer(style = list(
				id = 'carbon',
				type = 'raster',
				source = gee.src(pool = START.POOL, period = START.PERIOD, climate = START.CLIMATE, constrain = START.CONSTRAIN, returnEEobj = F),
				layout = list(visibility = 'visible')
			)) %>%
			# add places
			add_layer(style = list(
				id = 'places',
				type = 'raster',
				source = mapbox_source(
					type = 'raster',
					tiles = list(paste0('https://api.mapbox.com/styles/v1/sgorelik/clb1na6a0003915o0doxdw88p/tiles/256/{z}/{x}/{y}@2x?access_token=', mb.token)),
					# tiles = list('https://abcd.basemaps.cartocdn.com/rastertiles/voyager_only_labels/{z}/{x}/{y}{r}.png'),
					# attribution = '&copy; <a href="https://carto.com/attributions">CARTO</a>',
					tileSize = 256
				),
				layout = list(visibility = 'none')
			)) %>%
			# add country polygons
			add_source(as_mapbox_source(sp.countries), id = 'countries') %>%
			add_fill_layer(
				source = 'countries',
				id = 'countries',
				fill_color = 'transparent',
				fill_opacity = 1,
				fill_outline_color = 'white',
				popup = '{{NAME}}',
				visibility = F
			) %>%
			# add bioclimate zones
			add_source(as_mapbox_source(sp.bioclim), id = 'bioclim') %>%
			add_fill_layer(
				source = 'bioclim',
				id = 'bioclim',
				fill_color = c('get', 'fill_color'),
				fill_opacity = 0.8,
				fill_outline_color = 'white',
				popup = '{{group}}',
				visibility = F
			) %>%
			#add controls
			add_text_control(
				pos = 'bottom-right',
				text = "
					<strong>Mg C ha<sup>-1</sup></strong>
					<div class='legend-grid'>
						<div id='item-0'><div id='colorRamp'></div></div>
						<div id='item-1'>≥100</div>
						<div id='item-2'>75</div>
						<div id='item-3'>50</div>
						<div id='item-4'>25</div>
						<div id='item-5'>0</div>
					</div>"
			) %>%
			onRender(
				"function(el, x) {

					// get map object
					const map = mapboxer._widget[el.id].map;

					// add controls
					const scale = new mapboxgl.ScaleControl({
						maxWidth: 100,
						unit: 'metric'
					});
					map.addControl(scale, 'bottom-left');
					// scale._container.parentNode.className = 'mapboxgl-ctrl-bottom-right-adj';

					const nav = new mapboxgl.NavigationControl({
						showZoom: true,
						showCompass: true
					});
					document.getElementById('navControl').appendChild(nav.onAdd(map));

					// geocoder options: https://github.com/mapbox/mapbox-gl-geocoder/blob/main/API.md
					const searchbox = new MapboxGeocoder({
						accessToken: mapboxgl.accessToken,
						mapboxgl: mapboxgl,
						autocomplete: true,
						limit: 3,
						minLength: 4,
						collapsed: false,
						clearAndBlurOnEsc: true,
						placeholder: 'Search for a location'
					})
					document.getElementById('searchbox').appendChild(searchbox.onAdd(map));

					// return map coordinates on mouse click
					map.on('click', sendCoord2R);
					function sendCoord2R(e) {
						Shiny.setInputValue('map_coords_onclick', e.lngLat);
					}

					// add drawing controls and send polygons to R input$drawn_poly object as JSON string
					const draw = new MapboxDraw({
						displayControlsDefault: false,
						controls: {
							polygon: true,
							trash: true
						}
					});
					document.getElementById('draw').appendChild(draw.onAdd(map));
					map.on('draw.create', sendPoly2R);
					map.on('draw.delete', sendPoly2R);
					map.on('draw.update', sendPoly2R);
					function sendPoly2R(e) {
						const data = draw.getAll();
						var data_json_string = JSON.stringify(data);
						Shiny.setInputValue('drawn_poly', data_json_string);
					}
				}"
			)

	})

	# ---------------------------------------
	# toggle basemap (dark vs. imagery)
	# ---------------------------------------

	observe({

		mapboxer_proxy('map') %>%
			set_layout_property(layer_id = 'imagery', property = 'visibility', value = input$imagery) %>%
			update_mapboxer()

	})

	# ---------------------------------------
	# toggle map view (globe vs. flat)
	# ---------------------------------------

	# sends projection choice to javascript function defined in tags$head() on input change
	observeEvent(input$view, {

		if (input$view %% 2 == 1) {
			proj <- 'mercator'
			updateActionButton(session, inputId = 'view', icon = globe.icon)
		} else {
			proj <- 'globe'
			updateActionButton(session, inputId = 'view', icon = flat.icon)
		}

		session$sendCustomMessage('projection', proj)

	}, ignoreInit = F, ignoreNULL = F)

	# ---------------------------------------
	# toggle carbon layers
	# ---------------------------------------

	carb.lyr.listen <- reactive({
		list(input$pool, input$period, input$climate, input$constrain)
	})

	rv$cur_carb_lyr <- 'carbon'
	rv$cur_carb_is_blank <- F

	observeEvent(carb.lyr.listen(), {

		clim.sel <- input$climate
		pool.sel.cur <- input$pool
		per.sel.cur <- input$period

		if (clim.sel == 'Base_Clim') {
			per.opts <- c('Current' = 'CUR', 'Potential' = 'POT', 'Unrealized Potential' = 'UNR')
			per.sel.new <- per.sel.cur

			pool.opts <- c('Aboveground' = 'AGB', 'Belowground' = 'BGB', 'Soil' = 'SOC')
			pool.sel.new <- pool.sel.cur
		}

		if (clim.sel == 'RCP85') {
			per.opts <- c('Potential' = 'POT', 'Unrealized Potential' = 'UNR')
			per.sel.new <- ifelse(per.sel.cur == 'CUR', 'POT', per.sel.cur)

			pool.opts <- c('Aboveground' = 'AGB', 'Belowground' = 'BGB')
			pool.sel.new <- unlist(strsplit(gsub('SOC', '', paste0(pool.sel.cur, collapse = '-')), split = '-'))
		}

		updatePrettyRadioButtons(
			session = session,
			inputId = 'period',
			choices = per.opts,
			selected = per.sel.new,
			prettyOptions = list(
				shape = 'round',
				status = 'success',
				outline = F
			)
		)
		updatePrettyCheckboxGroup(
			session = session,
			inputId = 'pool',
			choices = pool.opts,
			selected = pool.sel.new,
			prettyOptions = list(
				shape = 'curve',
				icon = icon('check'),
				status = 'success',
				outline = F
			)
		)

		# count number of pools selected, keep previous count too
		n.pools <- length(pool.sel.new)
		rv$old_carb_is_blank <- rv$cur_carb_is_blank
		rv$cur_carb_is_blank <- ifelse(n.pools == 0, T, F)

		# save previous carbon layer id and create new id for current carbon layer
		rv$crbn_lyr_cnt <- rv$crbn_lyr_cnt + 1
		rv$old_carb_lyr <- rv$cur_carb_lyr
		rv$cur_carb_lyr <- paste0('carbon', rv$crbn_lyr_cnt)

		# message('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
		# df.carb <- data.frame(OLD = c(rv$old_carb_lyr), CUR = c(rv$cur_carb_lyr))
		# message(paste0(capture.output(df.carb), collapse = '\n'))

		if (rv$cur_carb_is_blank & !rv$old_carb_is_blank) {

			mapboxer_proxy('map') %>%
				set_layout_property(layer_id = rv$old_carb_lyr, property = 'visibility', value = F) %>%
				update_mapboxer()

		} else if (!rv$cur_carb_is_blank & rv$old_carb_is_blank) {

			mapboxer_proxy('map') %>%
				add_layer(style = list(
					id = rv$cur_carb_lyr,
					type = 'raster',
					source = gee.src(pool = pool.sel.new, period = per.sel.new, climate = clim.sel, constrain = input$constrain, returnEEobj = F),
					layout = list('visibility' = 'visible')
				)) %>%
				update_mapboxer()

			session$sendCustomMessage('moveLayerBeforeLayer', message = list(layerID = rv$cur_carb_lyr, beforeID = 'countries'))

		} else if (!rv$cur_carb_is_blank & !rv$old_carb_is_blank) {

			mapboxer_proxy('map') %>%
				set_layout_property(layer_id = rv$old_carb_lyr, property = 'visibility', value = F) %>%
				add_layer(style = list(
					id = rv$cur_carb_lyr,
					type = 'raster',
					source = gee.src(pool = pool.sel.new, period = per.sel.new, climate = clim.sel, constrain = input$constrain, returnEEobj = F),
					layout = list('visibility' = 'visible')
				)) %>%
				update_mapboxer()

			session$sendCustomMessage('moveLayerBeforeLayer', message = list(layerID = rv$cur_carb_lyr, beforeID = 'countries'))

		}

	}, ignoreInit = T)

	# ---------------------------------------
	# carbon layer legend
	# ---------------------------------------

	observeEvent(input$period, {

		if (input$period == 'UNR') {
			new.pal.cols <- magma(256)
			item.1 <- '≥100'
			item.2 <- 75
			item.3 <- 50
			item.4 <- 25
			item.5 <- 0
		} else {
			new.pal.cols <- viridis(256)
			item.1 <- '≥400'
			item.2 <- 300
			item.3 <- 200
			item.4 <- 100
			item.5 <- 0
		}

		new.pal.style <- paste0('linear-gradient(0deg, #000000, ', paste0(new.pal.cols, collapse = ', '), ')')

		session$sendCustomMessage('legend', message = list(
			style = new.pal.style,
			item1 = item.1,
			item2 = item.2,
			item3 = item.3,
			item4 = item.4,
			item5 = item.5
		))

	}, ignoreInit = F)

	# ---------------------------------------
	# carbon layer transparency
	# ---------------------------------------

	observeEvent(input$transparency, {
		opacity <- (1 - (as.numeric(input$transparency) / 100))
		mapboxer_proxy('map') %>%
			set_paint_property(layer_id = rv$cur_carb_lyr, property = 'raster-opacity', value = opacity) %>%
			update_mapboxer()
	})

	# ---------------------------------------
	# toggle bioclimate zones
	# ---------------------------------------

	observeEvent(input$bioclimzones, {

		mapboxer_proxy('map') %>%
			set_layout_property(layer_id = 'bioclim', property = 'visibility', value = input$bioclimzones) %>%
			update_mapboxer()

		if (input$bioclimzones) {
			session$sendCustomMessage('moveLayerBeforeLayer', message = list(layerID = 'bioclim', beforeID = 'gl-draw-polygon-fill-inactive.cold'))
		}

	})

	# ---------------------------------------
	# toggle countries
	# ---------------------------------------

	observeEvent(input$countries, {

		mapboxer_proxy('map') %>%
			set_layout_property(layer_id = 'countries', property = 'visibility', value = input$countries) %>%
			update_mapboxer()

		if (input$countries) {
			session$sendCustomMessage('moveLayerBeforeLayer', message = list(layerID = 'countries', beforeID = 'gl-draw-polygon-fill-inactive.cold'))
		}

	})

	# ---------------------------------------
	# toggle place names
	# ---------------------------------------

	observeEvent(input$places, {

		mapboxer_proxy('map') %>%
			set_layout_property(layer_id = 'places', property = 'visibility', value = input$places) %>%
			update_mapboxer()

		if (input$places) {
			session$sendCustomMessage('moveLayerToTop', 'places')

			# alos bring up country outlines
			updatePrettySwitch(
				session,
				inputId = 'countries',
				value = T
			)

		}

	})

	# ---------------------------------------
	# inspect pixel
	# ---------------------------------------

	# change cursor to crosshair when user is in pixel inspector panel
	set_cursor <- reactive({
		# message(rv$show)
		# message(input$analysisCollapsePanel)
		if ((rv$show == 'anl') & (req(input$analysisCollapsePanel) == 'Inspect Pixel')) {
			cursor <- 'crosshair'
		} else {
			cursor <- 'auto'
		}
		style <- paste0('#map canvas { cursor: ', cursor, '; }')
		return(style)
	})

	output$cursor_css <- renderUI({
		tags$style(HTML(set_cursor()))
	})

	# compute carbon density for selected pixel
	rv$df_pixel <- data.frame()
	observeEvent(input$map_coords_onclick, {

		# only compute if user is in pixel inspector panel
		if ((rv$show == 'anl') & (req(input$analysisCollapsePanel) == 'Inspect Pixel')) {

			# convert coords to sf object
			coords <- input$map_coords_onclick
			mouse.lng <- coords$lng
			mouse.lat <- coords$lat
			df.pnt <- data.frame(lat = mouse.lat, lng = mouse.lng)
			sf.pnt <- st_as_sf(df.pnt, coords = c('lng', 'lat'), crs = 4326)

			# get current carbon layer as EE object
			img.mgcha <- gee.src(pool = input$pool, period = input$period, climate = input$climate, constrain = input$constrain, returnEEobj = T)

			# extract carbon density at location
			df.pnt.mgcha <- ee_extract(x = img.mgcha, y = sf.pnt, scale = 500, fun = ee$Reducer$mean(), via = 'getInfo', sf = F)

			# format output table
			pnt.mgcha <- ifelse(nrow(df.pnt.mgcha) > 0, as.numeric(df.pnt.mgcha), 0)
			rv$df_pixel <- data.frame(Long = round(mouse.lng, digits = 5), Lat = round(mouse.lat, digits = 5), MgCha = pnt.mgcha)

		}

	})

	output$pixelInspector <- renderTable({
		rv$df_pixel
	}, rownames = F, spacing = 'xs', striped = F, hover = F, bordered = F)


	# ---------------------------------------
	# analyze drawn polygons
	# ---------------------------------------

	analyze.drawn.polygon <- eventReactive(input$runDrawnPoly, {

		if (!is.null(input$drawn_poly)) {

			json.str <- input$drawn_poly
			sp.poly <- geojson_sf(json.str)

			message(input$pool)
			message(input$period)
			message(input$climate)
			message(input$constrain)

			# get current carbon layer as EE object
			img.mgcha <- gee.src(pool = input$pool, period = input$period, climate = input$climate, constrain = input$constrain, returnEEobj = T)
			img.mgc <- img.mgcha$multiply(21.46587)

			# calculate carbon stock and avg density
			df.avg <- ee_extract(x = img.mgcha$rename('Mean_MgCha'), y = sp.poly, scale = 500, fun = ee$Reducer$mean(), via = 'getInfo', sf = F)
			df.sum <- ee_extract(x = img.mgc$rename('Total_MgC'), y = sp.poly, scale = 500, fun = ee$Reducer$sum(), via = 'getInfo', sf = F)

			df <- data.frame(Mean_MgCha = df.avg$Mean_MgCha, Total_MgC = df.sum$Total_MgC)
			return(df)

		}
	})

	output$drawnPolyResults <- renderTable({
		analyze.drawn.polygon()
	}, rownames = F, spacing = 'xs', striped = F, hover = F, bordered = F, digits = 1)


	# ---------------------------------------
	# analyze uploaded polygon shapefile
	# ---------------------------------------

	# reactive function to return shapefile from user file inputs
	shp <- reactive({

		# shpdf is a data.frame with the name, size, type and datapath of the uploaded files
		shpdf <- input$shapefile

		# name of the temporary directory where files are uploaded
		tempdirname <- dirname(shpdf$datapath[1])

		# rename files
		for (i in 1:nrow(shpdf)) {
			file.rename(shpdf$datapath[i], paste0(tempdirname, '/', shpdf$name[i]))
		}

		# temporary shapefile filepath
		shp <- paste(tempdirname, shpdf$name[grep(pattern = '*.shp$', shpdf$name)], sep = '/')

		shp
	})

	output$inputFilesMessage <- renderText({
		req(input$shapefile)
		df <- input$shapefile
		f.num <- nrow(df)
		if (f.num == 1) {
			f.str <- 'file'
		} else {
			f.str <- 'files'
		}
		if (check_input_files(df)) paste0(f.num, ' ', f.str, ' selected:\n', paste(as.character(df$name), collapse = '\n'))
	})

	output$inputFilesError <- renderText({
		req(input$shapefile)
		df <- input$shapefile
		f.num <- nrow(df)
		if (f.num == 1) {
			f.str <- 'file'
		} else {
			f.str <- 'files'
		}
		m.num <- 4 - f.num
		if (m.num == 1) {
			m.str <- 'file'
		} else {
			m.str <- 'files'
		}
		if (m.num > 0) {
			m.err <- paste0('\n\n', m.num, ' more ', m.str, ' needed...')
		} else {
			m.err <- ''
		}
		if (!check_input_files(df)) paste0(f.num, ' ', f.str, ' selected:\n', paste(as.character(df$name), collapse = '\n'), m.err)
	})

	output$shpTypeError <- renderText({
		req(input$shapefile)
		if (check_input_files(input$shapefile)) {
			f <- shp()
			shp.type <- check_shp_type(f)
			if (!grepl('Polygon', shp.type)) paste('Error: This is a', shp.type, 'shapefile. Only a polygon shapefile is accepted.')
		}
	})

	observeEvent(input$shapefile, {

		if (check_input_files(input$shapefile)) {

			f <- shp()
			sp.user <- read_sf(f) %>%
				st_transform('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

			bbox <- st_bbox(sp.user)

			mapboxer_proxy('map') %>%
				add_source(as_mapbox_source(sp.user), id = 'user_poly') %>%
				add_line_layer(
					source = 'user_poly',
					id = 'user_poly',
					line_color = 'white',
					line_opacity = 0.8,
					line_width = 3,
					visibility = T
				) %>%
				fit_bounds(bbox, padding = 40, offset = c(80, 0)) %>%
				update_mapboxer()

			session$sendCustomMessage('moveLayerToTop', 'user_poly')

		}

	})

	analyze.user.polygon <- reactive({

		req(input$shapefile)
		f <- shp()
		sp.user <- read_sf(f) %>%
			st_transform('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

		# get current carbon layer as EE object
		img.mgcha <- gee.src(pool = input$pool, period = input$period, climate = input$climate, constrain = input$constrain, returnEEobj = T)
		img.mgc <- img.mgcha$multiply(21.46587)

		# calculate carbon stock and avg density
		df.avg <- ee_extract(x = img.mgcha$rename('Mean_MgCha'), y = sp.user, scale = 500, fun = ee$Reducer$mean(), via = 'getInfo', sf = F)
		df.sum <- ee_extract(x = img.mgc$rename('Total_MgC'), y = sp.user, scale = 500, fun = ee$Reducer$sum(), via = 'getInfo', sf = F)

		df <- data.frame(Mean_MgCha = df.avg$Mean_MgCha, Total_MgC = df.sum$Total_MgC)
		return(df)

	})

	output$shapefileResults <- renderTable({
		req(input$shapefile)
		if (check_input_files(input$shapefile)) {
			f <- shp()
			shp.type <- check_shp_type(f)
			if (grepl('Polygon', shp.type)) analyze.user.polygon()
		}
	}, rownames = F, spacing = 'xs', striped = F, hover = F, bordered = F, digits = 1)

}

shinyApp(ui, server)
