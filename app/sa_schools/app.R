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

# READ IN DATA -------------------------------------------------------------------------------------
sa_schools <- readRDS(here::here("data/sa_schools.RDS"))

# VARIABLES FOR INPUT PANEL ------------------------------------------------------------------------
# Variables to set size by
size_vars <- c(
  "Learner number" = "Learners_Cat",
  "Educator number" = "Educators_Cat",
  "Standard size"
)

# Variables to colour by
colour_vars <- c(
  "Sector" = "Sector",
  "Phase" = "Phase",
  "Quintile" = "Quintile",
  "Rural or urban" = "Urban_Rural"
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
                                        selectInput("colour", "colour by", colour_vars, selected = "Quintile"),
                                        plotOutput("bar_plot", height = 180)
                                        )
                          # TODO: Add search bar
                          # absolutePanel(top=80, left=70, textInput("target_zone", "" , "Search"))
                          ),

                 tabPanel("School directory",

                          DT::dataTableOutput("table")
                          ),

                 tabPanel("About",
                          br(),
                          fluidRow(
                            column(10, offset = 1,
                                   h4("The South African schools map and directory"),
                                   br(),
                                   p(em("Gain a unique perspective into South Africa’s multifarious, multilayered, multicultural and multicoloured education system.")),
                                   p("South Africa has an incredibly diverse, expansive education system, with over 25 000 schools dotted across the country, ranging from dilapidated mud huts and blackboards underneath trees to state-of-the-art, high tech facilities. This Shiny App makes use of data files from the Department of Basic Education to create a visual map to explore, understand and appreciate the complexity of our education system."),
                                   br(),
                                   h4("Terminology"),
                                   br(),
                                   p("Relevant to this app, and used in the South Africa education system, here are some definitions:"),
                                   br(),
                                   tags$ul(
                                     tags$li(strong("Quintiles:"),
                                             (tags$ul(
                                               tags$li("In response to unequal access to quality public schooling, the South African Schools Act was amended in 2005 to establish a quintile system. Under this system, schools are categorised into 5 groups (quintiles) based on the relative wealth of their surrounding communities, using measures such as the rates of income, unemployment and illiteracy within the school’s catchment area."),
                                               tags$li("Schools in the poorest communities are classified as Quintile 1 and schools serving the wealthiest communities are classified as Quintile 5."),
                                               tags$li("The quintile of the school determines the level of government funding and subsidies that it receives. Additionally. quintile 1, 2 and 3 schools are not allowed to charge fees and are often referred to as no-fee schools."),
                                               tags$li("The Quintile system is controversial as it poses several challenges to manage and there are major concerns over how schools are classified and the effects this can have."),
                                               tags$li("For some current news relating to this topic, have a look at the following resources"),
                                               tags$ul(
                                                 tags$li(tags$a(href = "https://www.groundup.org.za/article/fees-are-issue-school-too-not-just-university/", "Fees are an issue at school too")),
                                                 tags$li(tags$a(href = "https://www.corruptionwatch.org.za/schools-quintile-system-to-change/", "Schools quintile system to change?")),
                                                 tags$li(tags$a(href = "https://www.iol.co.za/sunday-tribune/news/calls-intensify-for-education-dept-to-scrap-quintile-rank-system-18250382", "Calls intensify to scrap quintile system")))))),

                                     tags$li(strong("Sector:"),
                                             (tags$ul(
                                               tags$li("This refers to whether the school is public or independent."),
                                               tags$li("The majority of schools in South Africa are public schools, meaning they are governed by the state."),
                                               tags$li("Independent schools, often referred to as private schools, are schools that are not owned by the state. They are usually owned and operated by a trust, church or community, or by a for-profit company."),
                                               tags$ul(
                                                 tags$li("There is an impression that high-fee independent schools make up most of the sector. But, this is not the case."),
                                                 tags$li("Traditional, high-fee independent schools are now a minority in the sector, with only an estimated 15% of schools charging fees of more than R50 000 per annum."),
                                                 tags$li("The sector serves a wide range of different religions, philosophies and educational approaches across the full socio-economic spectrum."))))),

                                     tags$li(strong("Phase:"),
                                             (tags$ul(
                                               tags$li("The phase of the school refers to the grouping of grades. Most schools are either a Primary School (Gr 1 - 7) or a Secondary School, also known as High School (Gr 8 - 12)."),
                                               tags$li("Combined schools run from Gr 1 to Gr 12 and there is a small proportion of 'Intermediate' schools.")))),

                                     tags$li(strong("Rural or urban:"),
                                             (tags$ul(
                                               tags$li("Whilst the classification in this dataset is binary, schools fall along a continuum of rural to urban in South Africa."),
                                               tags$li("Furthermore, in South Africa there is no single definition of ‘rural’ as rurality is characterised by diverse contexts. While ‘rural’ usually refers to settings that are sparsely populated and where agriculture is the major means of economic activity, the concept also includes areas of dense settlement created by colonial and apartheid -driven land settlements. Several ‘mining’ areas where mining is no longer active also fall into this category."),
                                               tags$li("Recently, the DBE has also been looking at a number of changes in how rural schools are managed under the 'Rural Education Policy', which includes specifically reviewing and aligning the curriculum for rural schools to serve the diverse needs. This rural education policy views rurality as 'a driver of educational reform, not a follower of urban agendas and priorities'."))))

                                   ),
                                   br(),
                                   h4("Interesting observations?"),
                                   br(),
                                   p(strong(em("What insights do you see when exploring the map and data?"))),
                                   br(),
                                   p("One such insight which jumps out at us when viewing this school data from a bird's eye view is that the geographical location of the schools of today still mirror the former homelands, a legacy of Apartheid."),
                                   tags$blockquote("The Bantustans, or homelands, established by the Apartheid Government, were areas to which the majority of the Black population was moved to prevent them from living in the urban areas of South Africa. The Bantustans were a major administrative mechanism for the removal of Blacks from the South African political system under the many laws and policies created by Apartheid. The idea was to separate Blacks from the Whites, and give Blacks the responsibility of running their own independent governments, thus denying them protection and any remaining rights a Black could have in South Africa. In other words, Bantustans were established for the permanent removal of the Black population in White South Africa."),
                                   tags$img(src = "map.gif", width = 800, style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(),
                                   h4("The data"),
                                   p("The data used in this app was obtained from the Department of Basic Education website in the form of Excel files. There is one for each province and they define the 'master list' of schools in South Africa and the most current classifications, rankings and information about schools that the public has access to.",
                                     em("These data were last updated in 2017 and hence represent the state of schools as of 2017.")),
                                   p("A lot of the data are missing and therefore display as 'NA' in the map. Additionally, some schools are also missing their GPS locations. Part of this project and the data processing steps has been to geocode these schools' locations using Open Street Maps. There are currently still about 600 schools with missing GPS locations and hence they are not shown on the interactive map, but can be found in the School's directory table."),
                                   br(),
                                   h4("More information"),
                                   br(),
                                   p(em("This is a work in progress!")),
                                   p("To see the code for this app, have a look at the", tags$a(href = "https://github.com/MeganBeckett/sa_schools", "Github repo.")),
                                   p("For more information and questions, please don't hesitate to contact me on megan.beckett13@gmail.com"),
                                   br()
                            )
                          )
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
    # # Get latitude and longitude for search bar - not working yet
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
      setView(lng = 24, lat = -30, zoom = 6) %>%
      addTiles() %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Zoom out",
        onClick = JS("function(btn, map){ map.setZoom(6); }"))) %>%
      addEasyButton(easyButton(
        icon = "fa-street-view", title = "Locate me",
        onClick = JS("function(btn, map){ map.locate({setView: true });  }")))
    m
  })

  # A reactive expression that returns the schools that are in view right now for plotting
  schools_in_view <- reactive({

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

    x_var <- input$colour

    # Set colours according to factors in x_var so that remain constant even if schools_in_view don't
    # cover all levels
    names(set_colours) <- levels(sa_schools[[x_var]])
    col_scale <- scale_fill_manual(name = x_var, values = set_colours, na.value = "grey")
    p <- ggplot(schools_in_view(), aes(x = schools_in_view()[[x_var]], fill = schools_in_view()[[x_var]])) +
      geom_bar() +
      labs(title='',
           x= '', y = 'School count') +
      col_scale +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))

    print(p)
  })


  # This observer is responsible for updating the circle size and colouraccording to the variables
  # the user has chosen in side panel.
  # TODO: Create two observers? One for colour_by and one for size_by
  observe({
    sa_schools <- data()
    colour_by <- input$colour
    size_by <- input$size

    colour_data <- sa_schools[[colour_by]]

    # Note: For some reason, when only two factors, "Set1" uses red and green instead of red and blue.
    # So, for now, hard coded the colours for 2 level factor variables to be red and blue so match colours in barplot
    if (colour_by == "Quintile" | colour_by == "Phase") {
      pal <- colorFactor("Set1", domain = colour_data, ordered = TRUE, na.color = "grey")
    } else {
      pal <- colorFactor(c("#E41A1B", "#377EB8"), domain = colour_data, ordered = TRUE, na.color = "grey")
    }

    if (size_by == "Standard size") {
      radius <- 200
    } else {
      radius <- sa_schools[[size_by]]
    }

   leafletProxy("map", data = sa_schools) %>%
     clearShapes() %>%
     addCircles(~longitude, ~latitude, radius = radius,
                 stroke = FALSE, fillOpacity = 0.7, fillColor = pal(colour_data),
                popup = paste(sa_schools$Name, "<br>",
                             "Phase:", sa_schools$Phase, "<br>",
                             "Sector:", sa_schools$Sector, "<br>",
                             "Quintile:", sa_schools$Quintile, "<br>",
                             "No. learners:", sa_schools$Learners, "<br>",
                             "No. educators:", sa_schools$Educators), weight = 1) %>%
     addLegend("bottomleft", pal = pal, values = colour_data, title = colour_by, layerId = "legend")
  })

  # DATA EXPLORER ----------------------------------------------------------------------------------
  output$table <- DT::renderDataTable({
    sa_schools_slim <- sa_schools %>%
      select(-Type, -Region, -NoFeeSchool, -BoardingSchool, -Learners_Cat, -Educators_Cat, -Section21, -latitude, -longitude) %>%
      select(NatEmis, Name, Province, Quintile, Sector, Phase, Urban_Rural, Learners, Educators, District, Circuit, StreetAddress, Telephone)

    DT::datatable(sa_schools_slim)
  })
}

# Run the application
shinyAppDir(".")
shinyApp(ui = ui, server = server)

