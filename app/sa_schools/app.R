# References
# Nav bar: https://shiny.rstudio.com/reference/shiny/1.0.1/navbarPage.html
# Absolute panels: https://shiny.rstudio.com/gallery/absolutely-positioned-panels.html

# LIBRARIES ----------------------------------------------------------------------------------------
library(shiny)
library(leaflet)
library(dplyr)
library(here)

# READ IN DATA -------------------------------------------------------------------------------------
sa_schools <- readRDS(here::here("data_prep/03_sa_schools.RDS"))
# Subset for now
sa_schools <- sa_schools %>%
  group_by(Province) %>%
  top_n(100)

# VARIABLES FOR INPUT PANEL --------------------------------------------------------------------------
# Variables to set size by
size_vars <- c(
  "Learner number" = "Learners",
  "Educator number" = "Educators",
  "Standard size" = 100
)

# Variables to colour by
color_vars <- c(
  "Sector" = "Sector",
  "Phase" = "Phase",
  "Quintile" = "Quintile",
  "Urban or rural" = "Urban_Rural"
)


# UI -----------------------------------------------------------------------------------------------
# ui <- fluidPage(
#     # Create base map
#     leafletOutput("map", height = "1000")
# )

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

                          # Create base map
                          leafletOutput("map", height = "1000"),

                          # Input panel
                          absolutePanel(id = "controls", fixed = TRUE,
                                        draggable = TRUE, top = 100, left = "auto", right = 40, bottom = "auto",
                                        width = 230, height = "auto",
                                        style = "opacity: 0.8",

                                        h3("School explorer"),

                                        radioButtons("size", "Size", size_vars, selected = "Learners"),
                                        selectInput("color", "Color", color_vars, selected = "Sector")
                                        )

                          ),

                 tabPanel("Explore data",

                          DT::dataTableOutput("table")
                          )


)

# SERVER -------------------------------------------------------------------------------------------
# Define server logic
server <- function(input, output) {

  data <- reactive({
    x <- sa_schools
  })

  output$map <- renderLeaflet({
    sa_schools <- data()

    m <- leaflet(data = sa_schools) %>%
      addTiles() %>%
      # fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      setView(lng = 24, lat = -30, zoom = 6) %>%
      # addMarkers(lng = ~longitude,
      #            lat = ~latitude,
      #            popup = paste(sa_schools$Name, "<br>",
      #                          "Phase:", sa_schools$Phase, "<br>",
      #                          "Sector:", sa_schools$Sector, "<br>",
      #                          "Quintile:", sa_schools$Quintile, "<br>",
      #                          "# learners:", sa_schools$Learners, "<br>",
      #                          "# educators:", sa_schools$Educators))
      addCircles(lng = ~longitude,
                 lat = ~latitude,
                 radius = ~Learners/2,
                 popup = paste(sa_schools$Name, "<br>",
                               "Phase:", sa_schools$Phase, "<br>",
                               "Sector:", sa_schools$Sector, "<br>",
                               "Quintile:", sa_schools$Quintile, "<br>",
                               "# learners:", sa_schools$Learners, "<br>",
                               "# educators:", sa_schools$Educators), weight = 1)

    m
  })

  output$table <- DT::renderDataTable({
    sa_schools_slim <- sa_schools %>%
      select(-latitude, -longitude)

    DT::datatable(sa_schools_slim)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

