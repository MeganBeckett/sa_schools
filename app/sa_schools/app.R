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

# UI -----------------------------------------------------------------------------------------------
# ui <- fluidPage(
#     # Create base map
#     leafletOutput("map", height = "1000")
# )

ui <- navbarPage(title = "South African Schools", id = "nav",
                 collapsible = TRUE,

                 tabPanel("Interactive map",

                          # Create base map
                          leafletOutput("map", height = "1000")
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
               radius = ~Learners,
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

