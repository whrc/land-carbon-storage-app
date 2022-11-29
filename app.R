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
library(geojsonio)
library(shiny)
library(shinyWidgets)
library(shinyBS)
# library(shinyanimate)
# library(shinyglide) # for app walk through guide
library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras) # devtools::install_github('srgorelik/leaflet.extras')
library(leafem)
library(leafpm)
library(htmlwidgets)
library(htmltools)
library(jsonlite)
library(viridis)
library(phosphoricons)
library(curl)
source('utils.R')

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
esri.key <- Sys.getenv('ESRI_API_KEY')
# gm.key <- Sys.getenv('GOOGLE_MAPS_API_KEY')

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
# 7. Define App
# -------------------------------------------------------------------------------

# .tooltip-inner {
# 	text-align: left!important;
# }

ui <- bootstrapPage(

	# withAnim(),

	tags$head(
		# tags$link(rel = 'icon', type = 'image/png', href = 'www/woodwell-logo-color.png'),
		tags$link(rel = 'icon', type = 'image/png', href = 'https://assets-woodwell.s3.us-east-2.amazonaws.com/wp-content/uploads/2020/08/19125343/Woodwell_Favicon.png'),
		includeCSS('woodwell-fonts.css'),
		tags$style(
			type = 'text/css',
			'html, body {
				font-family: Ginto Normal;
				width: 100%;
				height: 100%;
			}
			.leaflet-container {
				cursor: crosshair!important;
				background: #1a1a1a;
			}
			.legend {
				background-color: #EFEEEE;
				font-family: Ginto Normal;
			}
			.leaflet-control-scale-line {
				font-family: Ginto Normal;
				color: #EFEEEE;
				border: 1px solid #EFEEEE;
				border-top: none;
				margin-top: 10px;
				margin-bottom: 6px;
				line-height: 1.1;
				padding: 2px 5px 1px;
				font-size: 11px;
				white-space: nowrap;
				overflow: hidden;
				-moz-box-sizing: border-box;
					box-sizing: border-box;
				background: #EFEEEE;
				background: rgba(255, 255, 255, 0);
			}
			.leaflet-control-scale-line:not(:first-child) {
				border: 1px solid #EFEEEE;
				border-top: none;
				margin-top: 6px;
			}
			.leaflet-control-scale-line:not(:first-child):not(:last-child) {
				border: 1px solid #EFEEEE;
				border-top: none;
			}
			img.woodwell {
				opacity: 0.35;
			}
			img.woodwell:hover {
				opacity: 0.7;
			}
			img.woodwellBig {
				display: block;
				margin-left: auto;
				margin-right: auto;
				margin-top: 20px;
				margin-bottom: 0px;
			}
			img.woodwellBig:hover {
				box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
			}
			.btn.sidebar {
				display: block;
				height: 70px;
				width: 100%;
				border-radius: 0%;
				border: 0px;
				background-color: #EFEEEE;
			}
			.btn.sidebar:focus {
				outline: none;
			}
			.btn.closePanel {
				height: 16px;
				border-radius: 0%;
				border: 0px;
				background-color: transparent;
				padding-top: 0px;
				padding-right: 10px;
			}
			.btn.closePanel:focus {
				outline: none;
				background-color: transparent;
				box-shadow: none;
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
				padding: 20px 20px 100px 20px;
				overflow-y: auto;
			}
			.inputHeading {
				font-weight: 700;
				padding-top: 10px;
			}
			.infoTipGrid {
				align-items: top;
				width: 100%;
				display: grid;
				grid-template-columns: 9fr 1fr;
			}
			#pixelInspector {
				overflow: hidden;
			}
			'
		)

		# use if app is loaded into iFrame on another website:
		# tags$script(type = 'text/javascript', src = 'https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js')
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
		style = 'background-color: #EFEEEE; z-index: 10;',

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
			# icon = ph_i('map-pin-line', weight = 'thin', size = '2x'),
			# icon = ph_i('grid-four', weight = 'thin', size = '2x'),
			icon = ph_i('crosshair', weight = 'thin', size = '2x'),
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
						choices = c('Baseline' = 'Base_Clim', 'RCP 8.5' = 'RCP85'),
						selected = 'Base_Clim',
						shape = 'round',
						status = 'success',
						outline = F
					),

					p('Settings', class = 'inputHeading'),
					div(class = 'infoTipGrid',
						tagAppendAttributes(
							prettySwitch(
								inputId = 'switchConstraints',
								label = 'Constrain',
								value = F,
								status = 'success',
								fill = F,
								width = '100%'
							),
							style = 'margin-right: 0px; margin-bottom: 4px;'
						),
						ph_i('info', weight = 'fill', size = '1x', color = '#828282', id = 'infoConstraints'),
						tagAppendAttributes(
							prettySwitch(
								inputId = 'switchCountries',
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
								inputId = 'switchEcoregions',
								label = 'Ecoregions',
								value = F,
								status = 'success',
								fill = F,
								width = '100%'
							),
							style = 'margin-right: 0px; margin-bottom: 4px;'
						),
						ph_i('info', weight = 'fill', size = '1x', color = '#828282', id = 'infoEcoregions'),
						tagAppendAttributes(
							prettySwitch(
								inputId = 'switchPlaces',
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
								inputId = 'switchImagery',
								label = 'Imagery',
								value = F,
								status = 'success',
								fill = F,
								width = '100%'
							),
							style = 'margin-right: 0px;'
						)
					),

					# p('Transparency', style = 'margin-left: 0px; margin-top: 4px; margin-bottom: 0px;'),
					p('Transparency', class = 'inputHeading', style = 'margin-bottom: 0px;'),
					setSliderColor('#69bd54', 1),
					tagAppendAttributes(
						sliderTextInput(
							inputId = 'transSlider',
							label = NULL,
							choices = 0:100,
							selected = 0,
							hide_min_max = T,
							width = '95%',
							post = '%'
						),
						style = 'padding-top: 0px; display: inline-block;'
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

				div(id = 'optionsPanel', class = 'panelHeader',	'PIXEL INSPECTOR'),

				div(id = 'optionsPanel', class = 'panelBody',
					verbatimTextOutput(outputId = 'pixelInspector')
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

				div(id = 'optionsPanel', class = 'panelBody', style = 'text-align: justify;',
					p('About the Site', class = 'inputHeading'),
					p('The Land Carbon Storage Platform is a data sharing, visualization, and analysis web-app built by:'),
					a(img(src = 'woodwell-logo-color-text.png', class = 'woodwellBig', width = '85%', height = 'auto'), href = 'https://www.woodwellclimate.org/', target = '_blank'),
					br(),
					p('About the Data', class = 'inputHeading'),
					p('This site contains a first-of-its-kind data set on the global potential for increased storage of carbon on land.',
					  'This comprehensive data set was produced by Walker et al. (2022) and is described in detail in the',
					  a('Proceedings of the National Academy of Sciences.', href = 'https://doi.org/10.1073/pnas.2111312119', target = '_blank')
					),
					p('The units of the carbon density maps shown here are megagrams of carbon per hectare (Mg C ha', tags$sup(-1), ').',
					  'The data set has been prepared at a spatial resolution of ca. 500 meters in the MODIS sinusoidal projection'),
					p('Data can be downloaded in GeoTIFF format from the',
					  a('Harvard Dataverse.', href = 'https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DSDDQK', target = '_blank')
					),
					p('Contact Us', class = 'inputHeading'),
					a('info@woodwellclimate.org', href = 'mailto:info@woodwellclimate.org?subject=Land Carbon Storage Platform Inquiry', target='_blank'),
					a('508-540-9900', href = 'tel:+15085409900')
				)
			)
		)

	),

	bsTooltip(
		id = 'infoConstraints',
		title = 'Mask areas critical to food production and human habitation.',
		placement = 'right',
		trigger = 'click',
		options = list(container = 'body', delay = list(show = 0, hide = 0))
	),
	bsTooltip(
		id = 'infoCountries',
		title = 'Display countries with simplified boundaries for performance. Source: <a href="https://www.naturalearthdata.com/" target="_blank">Natural Earth</a>.',
		placement = 'right',
		trigger = 'click',
		options = list(container = 'body', delay = list(show = 0, hide = 0))
	),
	bsTooltip(
		id = 'infoEcoregions',
		title = 'Display terrestrial ecoregions with simplified boundaries for performance. Source: <a href="https://ecoregions.appspot.com/" target="_blank">RESOLVE</a>.',
		placement = 'right',
		trigger = 'click',
		options = list(container = 'body', delay = list(show = 0, hide = 0))
	),

	# title for web browser tab
	title = 'Land Carbon Storage App'

)

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

	# observe({
	# 	if (rv$show != 'none') {
	# 		startAnim(session, id = 'optionsPanel', type = 'slideInLeft')
	# 	} else {
	# 		startAnim(session, id = 'optionsPanel', type = 'slideOutLeft')
	# 	}
	# })

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

		sp.lines <- read_sf('shp/ne_10m_geographic_lines_WGS84_waterOnly.shp')

		leaflet(options = leafletOptions(minZoom = 1.75, zoomControl = F, worldCopyJump = F, zoomSnap = 0.25)) %>%
			addMapPane(name = 'lines', zIndex = 410) %>%
			addMapPane(name = 'raster', zIndex = 420) %>%
			addMapPane(name = 'placeLabels', zIndex = 430) %>%
			addMapPane(name = 'countries', zIndex = 440) %>%
			addMapPane(name = 'ecoregions', zIndex = 450) %>%
			addMapPane(name = 'overlayPane', zIndex = 460) %>% # drawn polygons
			addMapPane(name = 'polygons', zIndex = 470) %>% # uploaded polygon shapefiles
			addFeatures(sp.lines, group = 'lines', weight = 0.6, color = '#42433B', opacity = 0.6, pane = 'lines', dashArray = '4') %>%
			setView(lat = 20, lng = 10, zoom = 2.5) %>%
			setMaxBounds(lng1 = -360, lat1 = -80, lng2 = 360, lat2 = 90) %>%
			onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") %>%
			addEasyButton(easyButton(
				icon = 'fa-globe',
				title = 'Zoom to World',
				position = 'topright',
				onClick = JS('function(btn, map){ map.setView([20, 10], 2.5); }'))) %>%
			addEasyButton(easyButton(
				icon = 'fa-crosshairs',
				title = 'Locate Me',
				position = 'topright',
				onClick = JS('function(btn, map){ map.locate({setView: true}); }'))) %>%
			addFullscreenControl(position = 'topright') %>%
			addScaleBar(position = 'bottomright', options = scaleBarOptions(metric = T, imperial = T)) %>%
			onRender(
				'function(el, x) {
                    this.on("click", function(event) {
                        var lat = event.latlng.lat;
                        var lng = event.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange("click_coordinates", coord)
                    });
                }'
			) %>%
			addSearchOSM(options = searchOptions(
				position = 'topright',
				collapsed = T,
				hideMarkerOnCollapse = T,
				zoom = 7)
			) %>%
			onRender("function(el, x) { $('input.search-input')[0].placeholder = 'Search for a location...' }") %>%
			addPmToolbar(
				targetGroup = 'drawn',
				toolbarOptions = pmToolbarOptions(position = 'topright', drawMarker = F, drawPolyline = F, cutPolygon = F),
				drawOptions = pmDrawOptions(finishOn = 'dblclick', allowSelfIntersection = F),
				editOptions = pmEditOptions(allowSelfIntersection = F),
				cutOptions = pmCutOptions(allowSelfIntersection = F)
			)

	})

	output$map <- renderLeaflet({
		map()
	})

	pixel.inspect <- reactive({
		if (!is.null(input$click_coordinates)) {
			mouse.lat <- input$click_coordinates[1]
			mouse.lng <- input$click_coordinates[2]
			df.pnt <- data.frame(lat = mouse.lat, lng = mouse.lng)
			sf.pnt <- st_as_sf(df.pnt, coords = c('lng', 'lat'), crs = 4326)
			tot.mgcha <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Base_Clim')$select('TOT_UNR')$unmask(0)
			pnt.mgcha <- ee_extract(x = tot.mgcha, y = sf.pnt, scale = 500, fun = ee$Reducer$mean(), via = 'getInfo', sf = F)
			paste('Lat:', mouse.lat, '\nLng:', mouse.lng, '\nAGB:', pnt.mgcha, 'MgC/ha')
		} else {
			'Click pixel...'
		}
	})

	output$pixelInspector <- renderText({
		pixel.inspect()
	})

	transparency <- reactive({
		(1 - (as.numeric(input$transSlider) / 100))
	})

	layerListen <- reactive({
		list(input$checkPool, input$radioTime, input$radioClim, input$transSlider, input$switchConstraints)
	})

	observeEvent(layerListen(), {

		if (verbose) message('check climate, update UI...')
		clim.sel <- input$radioClim
		per.sel.cur <- input$radioTime
		pool.sel.cur <- input$checkPool
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
			inputId = 'radioTime',
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
			inputId = 'checkPool',
			choices = pool.opts,
			selected = pool.sel.new,
			prettyOptions = list(
				shape = 'curve',
				icon = icon('check'),
				status = 'success',
				outline = F
			)
		)

		if (verbose) message('determine raster layer to render on map...')
		n.pools <- length(pool.sel.new)

		if (input$switchConstraints) {
			# original image values: 0=NoData/Mask, 1=cropland, 2=shifting agriculture, 3=grazing lands, 4=urban areas
			msk <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Zonal')$select('Societal_Constraints')$unmask(0)$eq(0)
		}

		if (n.pools == 1) {
			add2map <- T
			var.name <- paste0(pool.sel.new, '_', per.sel.new)
			if (verbose) message(var.name)
			img.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', clim.sel))$select(var.name)$unmask(0)
			if (input$switchConstraints) img.mgcha <- img.mgcha$multiply(msk)
			img.mgcha <- img.mgcha$updateMask(img.mgcha$gt(0))
		} else if (n.pools == 2) {
			add2map <- T
			var.name.1 <- paste0(pool.sel.new[1], '_', per.sel.new)
			var.name.2 <- paste0(pool.sel.new[2], '_', per.sel.new)
			if (verbose) message(var.name.1)
			if (verbose) message(var.name.2)
			img.1.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', clim.sel))$select(var.name.1)$unmask(0)
			img.2.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', clim.sel))$select(var.name.2)$unmask(0)
			img.mgcha <- img.1.mgcha$add(img.2.mgcha)
			if (input$switchConstraints) img.mgcha <- img.mgcha$multiply(msk)
			img.mgcha <- img.mgcha$updateMask(img.mgcha$gt(0))
		} else if (n.pools == 3) {
			add2map <- T
			var.name.1 <- paste0(pool.sel.new[1], '_', per.sel.new)
			var.name.2 <- paste0(pool.sel.new[2], '_', per.sel.new)
			var.name.3 <- paste0(pool.sel.new[3], '_', per.sel.new)
			if (verbose) message(var.name.1)
			if (verbose) message(var.name.2)
			if (verbose) message(var.name.3)
			img.1.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', clim.sel))$select(var.name.1)$unmask(0)
			img.2.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', clim.sel))$select(var.name.2)$unmask(0)
			img.3.mgcha <- ee$Image(paste0('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/', clim.sel))$select(var.name.3)$unmask(0)
			img.mgcha <- img.1.mgcha$add(img.2.mgcha)$add(img.3.mgcha)
			if (input$switchConstraints) img.mgcha <- img.mgcha$multiply(msk)
			img.mgcha <- img.mgcha$updateMask(img.mgcha$gt(0))
		} else {
			add2map <- F
			if (verbose) message('no layer')
		}

		if (add2map) {

			max.val <- switch(
				per.sel.new,
				'CUR' = 400,
				'POT' = 400,
				'UNR' = 100
			)

			col.pal <- switch(
				per.sel.new,
				'CUR' = c('black', viridis(255)),
				'POT' = c('black', viridis(255)),
				'UNR' = c('black', magma(255))
			)

			vis.params <- list(
				min = 0,
				max = max.val,
				palette = col.pal
			)

			m1 <- Map$addLayer(
				eeObject = img.mgcha,
				visParams = vis.params
			)

			leg.pal <- colorNumeric(
				palette = rev(col.pal),
				domain = rev(c(0, max.val))
			)

			leafletProxy('map', session) %>%
				clearGroup(group = 'raster') %>%
				removeControl(layerId = 'legend') %>%
				addTiles(
					urlTemplate = m1$rgee$tokens,
					layerId = 'carbonLayer',
					group = 'raster',
					options = tileOptions(
						opacity = transparency(),
						noWrap = T,
						bounds = list(list(-90, -180), list(90, 180)),
						pane = 'raster'
					)
				) %>%
				addLegend(
					layerId = 'legend',
					position = 'bottomright',
					pal = leg.pal,
					values = c(0, max.val),
					bins = 4,
					title = 'Mg C ha<sup>-1</sup>',
					opacity = 1,
					labFormat = labelFormat(transform = function(x) sort(x, decreasing = T))
				)

		} else {

			leafletProxy('map', session) %>%
				clearGroup(group = 'raster') %>%
				removeControl(layerId = 'legend')

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

			bbox <- st_bbox(sp) %>%
				as.vector()

			leafletProxy('map', session) %>%
				clearGroup(group = 'polygon') %>%
				addFeatures(sp, group = 'polygon', pane = 'polygons', weight = 3, color = '#3388ff', opacity = 0.8, fillOpacity = 0) %>%
				flyToBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4])
		}

	})

	observeEvent(input$switchCountries, {

		if (!input$switchCountries) {

			leafletProxy('map', session) %>%
				addTiles(group = 'countries', options = tileOptions(opacity = 0)) %>%
				clearGroup(group = 'countries')

		}

		if (input$switchCountries) {

			polys <- read_sf('shp/ne_10m_admin_0_countries_WGS84_small.shp')

			leafletProxy('map', session) %>%
				clearGroup(group = 'countries') %>%
				addFeatures(
					polys,
					group = 'countries',
					pane = 'countries',
					weight = 0.8,
					color = 'white',
					opacity = 0.6,
					fillOpacity = 0,
					popup = ~htmlEscape(NAME),
					highlightOptions = highlightOptions(color = 'white', weight = 2, opacity = 1, bringToFront = T))
		}

	})

	observeEvent(input$switchEcoregions, {

		if (!input$switchEcoregions) {

			leafletProxy('map', session) %>%
				addTiles(group = 'ecoregions', options = tileOptions(opacity = 0)) %>%
				clearGroup(group = 'ecoregions')

		}

		if (input$switchEcoregions) {

			polys <- read_sf('shp/Ecoregions2017_WGS84_smaller.shp')

			leafletProxy('map', session) %>%
				clearGroup(group = 'ecoregions') %>%
				addFeatures(
					polys,
					group = 'ecoregions',
					pane = 'ecoregions',
					weight = 0.8,
					color = 'white',
					opacity = 0.6,
					fillOpacity = 0,
					popup = ~paste('Ecoregion:', ECO_NAME, '<br>Biome:', BIOME_NAME, '<br>Realm:', REALM),
					highlightOptions = highlightOptions(color = 'white', weight = 2, opacity = 1, bringToFront = T))

		}

	})

	rv$esri.attr <- NULL

	observeEvent(input$switchImagery, {

		# dark
		if (!input$switchImagery) {
			leafletProxy('map', session) %>%
				clearGroup(group = 'basemap') %>%
				addTiles(
					urlTemplate = paste0('https://api.mapbox.com/styles/v1/sgorelik/clawraplt001g16pifnv8l4xa/tiles/256/{z}/{x}/{y}@2x?access_token=', mb.token),
					attribution = paste(c(
						"<a href='https://www.mapbox.com/about/maps/' target='_blank'>Mapbox</a>",
						"<a href='https://www.openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a>",
						"<a href='https://earthengine.google.com' target='_blank'>Google Earth Engine</a>",
						"<a href='https://shiny.rstudio.com' target='_blank'>R Shiny</a>"), collapse = ' | '),
					group = 'basemap',
					options = tileOptions(noWrap = T, bounds = list(list(-90, -180), list(90, 180))))
		}

		# imagery
		if (input$switchImagery) {

			if (is.null(rv$esri.attr)) {
				curl.req <- curl_fetch_memory(paste0('https://basemaps-api.arcgis.com/arcgis/rest/services/styles/ArcGIS:Imagery?type=style&token=', esri.key))
				esri.meta <- parse_json(rawToChar(curl.req$content))
				rv$esri.attr <- esri.meta$sources$esri$attribution
				if (verbose) message(rv$esri.attr)
			}

			leafletProxy('map', session) %>%
				clearGroup(group = 'basemap') %>%
				# addTiles(
				# 	urlTemplate = 'https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}',
				# 	attribution = paste(c(
				# 		"<a href='https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer' target='_blank'>U.S. Geological Survey</a>",
				# 		"<a href='https://earthengine.google.com' target='_blank'>Google Earth Engine</a>",
				# 		"<a href='https://shiny.rstudio.com' target='_blank'>R Shiny</a>"), collapse = ' | '),
				# 	group = 'basemap',
				# 	options = tileOptions(noWrap = T, bounds = list(list(-90, -180), list(90, 180))))
				addTiles(
					urlTemplate = paste0('https://ibasemaps-api.arcgis.com/arcgis/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}?token=', esri.key),
					attribution = paste(c(
						"Imagery Powered by <a href='https://www.esri.com/en-us/home' target='_blank'>Esri</a>",
						paste('Imagery', rv$esri.attr),
						"<a href='https://earthengine.google.com' target='_blank'>Google Earth Engine</a>",
						"<a href='https://shiny.rstudio.com' target='_blank'>R Shiny</a>"), collapse = ' | '),
					group = 'basemap',
					options = tileOptions(noWrap = T, bounds = list(list(-90, -180), list(90, 180))))

		}

	})

	observeEvent(input$switchPlaces, {

		# no place labels
		if (!input$switchPlaces) {
			leafletProxy('map', session) %>%
				addTiles(group = 'placeLabels', options = tileOptions(opacity = 0)) %>%
				clearGroup(group = 'placeLabels')
		}

		# place labels
		if (input$switchPlaces) {
			leafletProxy('map', session) %>%
				clearGroup(group = 'placeLabels') %>%
				addTiles(
					urlTemplate = paste0('https://api.mapbox.com/styles/v1/sgorelik/clb1na6a0003915o0doxdw88p/tiles/256/{z}/{x}/{y}@2x?access_token=', mb.token),
					attribution = paste(
						"Places:",
						"<a href='https://www.mapbox.com/about/maps/' target='_blank'>Mapbox</a> &",
						"<a href='https://www.openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a>"),
					group = 'placeLabels',
					options = tileOptions(pane = 'placeLabels', noWrap = T, bounds = list(list(-90, -180), list(90, 180))))
		}
	})

}

# df.avg <- ee_extract(x = img.unr.mgcha, y = sp, scale = 500, fun = ee$Reducer$mean(), via = 'getInfo', sf = F)
# df.sum <- ee_extract(x = img.mgcha$divide(21.46587), y = sp, scale = 500, fun = ee$Reducer$sum(), via = 'getInfo', sf = F)
# df <- data.frame(mean_mgcha = df.avg$AGB_UNR, sum_mgc = df.sum$AGB_UNR)

shinyApp(ui = ui, server = server)
