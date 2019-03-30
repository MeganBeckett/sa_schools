# References
# Nav bar: https://shiny.rstudio.com/reference/shiny/1.0.1/navbarPage.html
# Absolute panels: https://shiny.rstudio.com/gallery/absolutely-positioned-panels.html

# LIBRARIES ----------------------------------------------------------------------------------------
library(shiny)
library(leaflet)
library(dplyr)
library(here)
#library(tmaptools)
library(htmltools)
library(htmlwidgets)
library(shinyjs)
library(ggplot2)
library(RColorBrewer)

# # JS FUNCTION FOR GEOLOCATE ------------------------------------------------------------------------
# jsCode <- '
# shinyjs.geoloc = function() {
#     navigator.geolocation.getCurrentPosition(onSuccess, onError);
#     function onError (err) {
#         Shiny.onInputChange("geolocation", false);
#     }
#     function onSuccess (position) {
#         setTimeout(function () {
#             var coords = position.coords;
#             console.log(coords.latitude + ", " + coords.longitude);
#             Shiny.onInputChange("geolocation", true);
#             Shiny.onInputChange("lat", coords.latitude);
#             Shiny.onInputChange("long", coords.longitude);
#         }, 5)
#     }
# };
# '
# READ IN DATA -------------------------------------------------------------------------------------
sa_schools <- readRDS(here::here("data/03_sa_schools.RDS"))

# VARIABLES FOR INPUT PANEL --------------------------------------------------------------------------
# Variables to set size by
size_vars <- c(
  "Learner number" = "Learners_Cat",
  "Educator number" = "Educators_Cat",
  "Standard size"
)

# Variables to colour by
color_vars <- c(
  "Sector" = "Sector",
  "Phase" = "Phase",
  "Quintile" = "Quintile",
  "Urban or rural" = "Urban_Rural"
)

# COLOUR SCALE -------------------------------------------------------------------------------------
# Set colour scale for ggplots
set_colours <- brewer.pal(5, "Set1")

# UI -----------------------------------------------------------------------------------------------
ui <- navbarPage(title = "South African Schools", id = "nav",
                 collapsible = TRUE,

                 tabPanel("Interactive map",

                          tags$head(
                            tags$style(HTML("
                              #controls {
                                background-color: white;
                                padding: 0 20px 20px 20px;
                                opacity: 0.5};")
                            )),

                          # # Tell shiny we will use some Javascript
                          # useShinyjs(),
                          # extendShinyjs(text = jsCode, functions = c("shinyjs.geoloc")),
                          #
                          # # Add button
                          # br(),
                          # actionButton("geoloc", "Localize me", class="btn btn-primary", onClick="shinyjs.geoloc()"),

                          # Create base map
                          leafletOutput("map", height = "800"),

                          # Input panel
                          absolutePanel(id = "controls", fixed = TRUE,
                                        draggable = TRUE, top = 100, left = "auto", right = 40, bottom = "auto",
                                        width = 230, height = "auto",
                                        style = "opacity: 0.8",

                                        h3("School explorer"),

                                        radioButtons("size", "Size by", size_vars, selected = "Learners_Cat"),
                                        plotOutput("hist_plot", height = 150),
                                        selectInput("color", "Color by", color_vars, selected = "Quintile"),
                                        plotOutput("bar_plot", height = 150)
                                        )
                          #absolutePanel(top=80, left=70, textInput("target_zone", "" , "Search"))
                          ),

                 tabPanel("School directory",

                          DT::dataTableOutput("table")
                          )


)

# SERVER -------------------------------------------------------------------------------------------
# Define server logic
server <- function(input, output) {

  data <- reactive({
    x <- sa_schools
  })

  # INTERACTIVE MAP --------------------------------------------------------------------------------
  output$map <- renderLeaflet({
    # # Get latitude and longitude
    # if(input$target_zone=="Search" | input$target_zone==""){
    #   ZOOM=6
    #   LAT=-30
    #   LONG=24
    # }else{
    #   target_pos=geocode_OSM(input$target_zone)
    #   LAT=target_pos$lat
    #   LONG=target_pos$lon
    #   ZOOM=12
    # }

    m <- leaflet() %>%
      # fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      setView(lng = 24, lat = -30, zoom = 6) %>%
      addTiles() %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom out",
        onClick=JS("function(btn, map){ map.setZoom(6); }")))
      # addEasyButton(easyButton(
      #   icon="fa-crosshairs", title="Locate Me",
      #   onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    m
  })

  # A reactive expression that returns the schools that are in view right now
  schools_in_view <- reactive({
    # if (is.null(input$map_bounds))
    #   return(sa_schools[FALSE,])

    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(sa_schools,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Histogram plot for variable chosen to size by, limited to schools in view
  output$hist_plot <- renderPlot({
    if (nrow(schools_in_view()) == 0)
      return(NULL)

    if (input$size == "Learners_Cat") {
      x_var = "Learners"
    } else {
      x_var = "Educators"
    }

    p <- ggplot(schools_in_view(), aes(x = schools_in_view()[[x_var]])) +
      geom_histogram() +
      labs(title= '',
           x = paste('No. of', x_var, sep = " "), y = 'School count')

    print(p)
  })

  # Bar plot for variable chosen to colour by, limited to schools in view
  output$bar_plot <- renderPlot({
    if (nrow(schools_in_view()) == 0)
      return(NULL)

    x_var <- input$color

    # Set colours according to factors in x_var so that remain constant even if schools_in_view don't
    # cover all levels
    names(set_colours) <- levels(sa_schools[[x_var]])
    col_scale <- scale_fill_manual(name = x_var,values = set_colours, na.value = "grey")
    p <- ggplot(schools_in_view(), aes(x = schools_in_view()[[x_var]], fill = schools_in_view()[[x_var]])) +
      geom_bar() +
      labs(title='',
           x= '', y = 'School count') +
      col_scale +
      theme(legend.position = "none")

    print(p)
  })


  # # Find geolocalisation coordinates when user clicks
  # observeEvent(input$geoloc, {
  #   js$geoloc()
  # })
  #
  # # zoom on the corresponding area
  # observe({
  #   if(!is.null(input$lat)){
  #     map <- leafletProxy("map")
  #     dist <- 0.2
  #     lat <- input$lat
  #     lng <- input$long
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   }
  # })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    sa_schools <- data()
    color_by <- input$color
    size_by <- input$size

    color_data <- sa_schools[[color_by]]
    pal <- colorFactor("Set1", color_data)

    if (size_by == "Standard size") {
      radius <- 200
    } else {
      radius <- sa_schools[[size_by]]
    }

   leafletProxy("map", data = sa_schools) %>%
     clearShapes() %>%
     # removeControl("legend") %>%
     addCircles(~longitude, ~latitude, radius=radius,
                 stroke=FALSE, fillOpacity=0.7, fillColor=pal(color_data),
                popup = paste(sa_schools$Name, "<br>",
                             "Phase:", sa_schools$Phase, "<br>",
                             "Sector:", sa_schools$Sector, "<br>",
                             "Quintile:", sa_schools$Quintile, "<br>",
                             "No. learners:", sa_schools$Learners, "<br>",
                             "No. educators:", sa_schools$Educators), weight = 1) %>%
     addLegend("bottomleft", pal=pal, values=color_data, title=color_by, layerId = "legend")
  })

  # DATA EXPLORER ----------------------------------------------------------------------------------
  output$table <- DT::renderDataTable({
    sa_schools_slim <- sa_schools %>%
      select(-latitude, -longitude)

    DT::datatable(sa_schools_slim)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

