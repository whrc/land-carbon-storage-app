#!/usr/bin/env Rscript

library(rgee)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinyWidgets)
library(viridis)
library(sf)
library(mapview)

source('utils.R')

# development = T, deployment = F
dev <- T

# -------------------------------------------------------------------------------
# 1. Set rgee
# -------------------------------------------------------------------------------

if (dev) {
	ee_Initialize(gcs = T, drive = T)
} else {
	if (!reticulate::virtualenv_exists('rgee_py')) {
		set_rgee_dependencies()
	}
}


# -------------------------------------------------------------------------------
# 2. Run again ee_Initialize, after a long period of inactivity.
# -------------------------------------------------------------------------------

tryCatch(
	expr = ee$Image(0),
	error = function(e) { ee_Initialize() }
)

# -------------------------------------------------------------------------------
# 3. DEFINE HERE YOUR APP
# -------------------------------------------------------------------------------

ui <- bootstrapPage(

	tags$head(
		# tags$link(href = "https://fonts.googleapis.com/css?family=Teko", rel = "stylesheet"),
		# tags$link(rel = "stylesheet", type = "text/css", href = "www/fonts.css"),
		# tags$style(type = "text/css", "html, body {width: 100%; height: 100%; font-family: Sohne;}"),

		tags$style(type = "text/css", "html, body {width: 100%; height: 100%;}"),

		# use if app is loaded into iFrame on another website:
		tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js", type="text/javascript")
	),

	leafletOutput(outputId = "map", width = "100%", height = "100%"),

	absolutePanel(
		top = 10, right = 10, style = "z-index:500; text-align: right;",
		tags$h2("Potential Carbon"),
		tags$a("About this data", href="https://www.pnas.org/doi/10.1073/pnas.2111312119")
	),

	absolutePanel(
		top = 150, left = 12, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;",

		dropdownButton(

			numericInput(inputId = 'in_latitude', label = 'Latitude', value = 9.08203, min = -90, max = 90, step = 0.1),
			numericInput(inputId = 'in_longitude', label = 'Longitude', value = 47.39835, min = -180, max = 180, step = 0.1),
			actionButton(inputId = 'reposition', 'Reposition Map', class = 'btn-primary'),

			label = 'Location',
			circle = F,
			icon = icon('bars'),
			width = '300px',
			tooltip = tooltipOptions(title = "Click to see inputs!")
		)

		# actionBttn(
		# 	inputId = "showPanel1",
		# 	label = NULL,
		# 	style = "simple",
		# 	color = "primary",
		# 	icon = icon("bars")
		# ),
		# conditionalPanel(
		# 	condition = 'input.showPanel1',
		# 	wellPanel(
		# 		numericInput(inputId = 'in_latitude', label = 'Latitude', value = 9.08203, min = -90, max = 90, step = 0.1),
		# 		numericInput(inputId = 'in_longitude', label = 'Longitude', value = 47.39835, min = -180, max = 180, step = 0.1),
		# 		actionButton(inputId = 'reposition', 'Reposition Map', class = 'btn-primary')
		# 	)
		# )
	)

)

unr_img <- ee$Image('projects/woodwell-biomass/assets/Walker_etal_2022_PNAS/Base_Clim')$select('AGB_UNR')

sp <- read_sf('~/projects/telephone_gap/shp/TelGap_IRPBoundary/TelGap_IRPBoundary.shp') %>%
	st_transform('+proj=longlat +datum=WGS84')

mean <- ee_extract(x = unr_img, y = sp, scale = 500, fun = ee$Reducer$mean(), via = 'gcs', container = 'bm-share')

server <- function(input, output) {

	map <- eventReactive(input$reposition, {

		m1 <- Map$addLayer(
			eeObject = unr_img,
			visParams = list(
				min = 0,
				max = 80,
				palette = inferno(255)
			),
			name = 'Unrealized Potential'
		)

		leaflet(options = leafletOptions(minZoom = 2)) %>%
			addSearchOSM() %>%
			addProviderTiles('CartoDB.DarkMatterNoLabels') %>%
			setView(lat = 12, lng = 0, zoom = 2) %>%
			addTiles(
				urlTemplate = m1$rgee$tokens,
				layerId = 'unrealized_potential',
				options = tileOptions(opacity = 1)) %>%
			addFeatures(sp, weight = 1, radius = 3, fillColor = 'white', color = 'white', opacity = 0.8, fillOpacity = 0.2) %>%
			addEasyButton(easyButton(
				icon = 'fa-globe',
				title = 'Zoom to World',
				onClick = JS('function(btn, map){ map.setView([12, 0], 2); }'))) %>%
			addEasyButton(easyButton(
				icon = 'fa-crosshairs',
				title = 'Locate Me',
				onClick = JS('function(btn, map){ map.locate({setView: true}); }'))) %>%
			addFullscreenControl(position = 'topright')

	},
		ignoreNULL = F
	)

	output$map <- renderLeaflet({
		map()
	})
}

shinyApp(ui = ui, server = server)
