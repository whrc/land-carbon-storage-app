#!/usr/bin/env Rscript

# -------------------------------------------------------------------------------
# 1. Set Phase
# -------------------------------------------------------------------------------

# development (TRUE) or deployment (FALSE)?
dev <- F

# -------------------------------------------------------------------------------
# 2. Libraries
# -------------------------------------------------------------------------------

library(rgee)
library(shiny)
library(shinyWidgets)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leafem)
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

choices <- c('layers','location','polygon','info')
names(choices) <- c(
	as.character(ph('stack', weight = 'thin')),
	as.character(ph('map-pin-line', weight = 'thin')),
	as.character(ph('polygon', weight = 'thin')),
	as.character(ph('info', weight = 'thin'))
)

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
			img {
				opacity: 0.3;
			}
			img:hover {
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
			#title {
				display: table-cell;
				color: #36363C;
				font-size: 12px;
				height: 100px;
				text-align: center;
				vertical-align: middle;
				padding: 2px;
			}
			#optionsPanel {
				background-color: white;
				padding: 10px;
				min-width: 250px;
			}
			'
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

		div(id = 'title', 'POTENTIAL\nCARBON'),

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
			a(href='https://www.woodwellclimate.org/', img(src='woodwell-logo.png', width = '80px', height = '34px'))
		),

		conditionalPanel(
			condition = "output.showPanel == 'lyr'",
			absolutePanel(
				id = 'optionsPanel',
				top = 0,
				left = '70px',
				width = '20%',
				height = '100%',
				fixed = T,
				draggable = F,
				'Layers...'
			)
		),

		conditionalPanel(
			condition = "output.showPanel == 'loc'",
			absolutePanel(
				id = 'optionsPanel',
				top = 0,
				left = '70px',
				width = '20%',
				height = '100%',
				fixed = T,
				draggable = F,
				'Location...'
			)
		),

		conditionalPanel(
			condition = "output.showPanel == 'ply'",
			absolutePanel(
				id = 'optionsPanel',
				top = 0,
				left = '70px',
				width = '20%',
				height = '100%',
				fixed = T,
				draggable = F,
				'Polygons...'
			)
		),

		conditionalPanel(
			condition = "output.showPanel == 'inf'",
			absolutePanel(
				id = 'optionsPanel',
				top = 0,
				left = '70px',
				width = '20%',
				height = '100%',
				fixed = T,
				draggable = F,
				'Info...'
			)
		)

	)

)


img.unr.mgcha <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Base_Clim')$select('AGB_UNR')

img.unr.mgc <- img.unr.mgcha$divide(21.46587)

# sp <- read_sf('~/projects/telephone_gap/shp/TelGap_IRPBoundary/TelGap_IRPBoundary.shp') %>%
# 	st_transform('+proj=longlat +datum=WGS84')

# df.avg <- ee_extract(x = img.unr.mgcha, y = sp, scale = 500, fun = ee$Reducer$mean(), via = 'getInfo', sf = F)
# df.sum <- ee_extract(x = img.unr.mgc, y = sp, scale = 500, fun = ee$Reducer$sum(), via = 'getInfo', sf = F)
# df <- data.frame(mean_mgcha = df.avg$AGB_UNR, sum_mgc = df.sum$AGB_UNR)

server <- function(input, output) {

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

	toListen <- reactive({
		list(input$lyr_btn, input$loc_btn, input$ply_btn, input$inf_btn)
	})

	observeEvent(toListen(), {

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

		# message('======= EVENT =======')
		# message(paste('lyr:', input$lyr_btn))
		# message(paste('loc:', input$loc_btn))
		# message(paste('ply:', input$ply_btn))
		# message(paste('inf:', input$inf_btn))
		# message(paste('old:', rv$old_sel))
		# message(paste('cur:', rv$cur_sel))
		# message(paste('cnt:', rv$cnt))
		# message(paste('show:', rv$show))
	})

	output$showPanel <- reactive(rv$show)
	outputOptions(output, 'showPanel', suspendWhenHidden = F)

	map <- eventReactive(input$reposition, {

		m1 <- Map$addLayer(
			eeObject = img.unr.mgcha,
			visParams = list(
				min = 0,
				max = 80,
				palette = inferno(255)
			),
			name = 'Unrealized Potential'
		)

		leaflet(options = leafletOptions(minZoom = 2, zoomControl = F, worldCopyJump = T)) %>%
			# addSearchOSM(options = searchOptions(container = 'meow', hideMarkerOnCollapse = T, zoom = 7)) %>%
			# htmlwidgets::onRender("function(el, x) { $('input.search-input')[0].placeholder = 'Search...' }") %>%
			addProviderTiles('CartoDB.DarkMatterNoLabels') %>%
			setView(lat = 12, lng = -20, zoom = 3) %>%
			setMaxBounds(lng1 = -360, lat1 = -80, lng2 = 360, lat2 = 90) %>%
			addTiles(
				urlTemplate = m1$rgee$tokens,
				layerId = 'unrealized_potential',
				options = tileOptions(opacity = 1)) %>%
			# addFeatures(sp, weight = 2, radius = 3, fillColor = 'white', color = 'white', opacity = 0.8, fillOpacity = 0) %>%
			htmlwidgets::onRender("function(el, x) { L.control.zoom({ position: 'topright' }).addTo(this) }") %>%
			addEasyButton(easyButton(
				icon = 'fa-globe',
				title = 'Zoom to World',
				position = 'topright',
				onClick = JS('function(btn, map){ map.setView([12, -20], 3); }'))) %>%
			addEasyButton(easyButton(
				icon = 'fa-crosshairs',
				title = 'Locate Me',
				position = 'topright',
				onClick = JS('function(btn, map){ map.locate({setView: true}); }'))) %>%
			addFullscreenControl(position = 'topright') %>%
			addScaleBar(position = 'bottomright', options = scaleBarOptions(metric = T, imperial = F))

	},
		ignoreNULL = F
	)

	output$map <- renderLeaflet({
		map()
	})
}

shinyApp(ui = ui, server = server)
