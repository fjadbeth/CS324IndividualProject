#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libaries
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(stringr)

# Importing data set
data <- read.csv('data/college_data.csv')

# data$Region <- with(data, 
#                     ifelse(data$Region == "Southeast" || data$Region == "Southwest" || data$Region == "Plains",
#                     word(data$Region, 1),
#                     ifelse(data$Region == "US Service schools", word(data$Region, 1, 3), word(data$Region, 1, 2))))

deg_choice <- unique(data$Highest.degree.offered)
# Define UI for application
vars <- select_if(data, is.numeric)

# Data for table
tableData <- data %>% select(-1)

ui <- navbarPage("College Explorer", id="nav",
  tabPanel("Interactive map",
           leafletOutput("map", width="100%", height=600),
           ),
  tabPanel("Plot",
           sidebarPanel(
             selectizeInput('xcol', 'X Variable', colnames(vars)[2:length(vars)], selected = colnames(vars)[1]),
             selectizeInput('ycol', 'Y Variable', colnames(vars)[2:length(vars)], selected = colnames(vars[3]))
             ),
           mainPanel(
             plotlyOutput('plot'))
           ),
  # Third parameter needs to get changed. 
  tabPanel("Data Explore",
           fluidRow(
             column(6, 
             pickerInput(
               inputId = "myPicker", 
               label = "Select/deselect all + format selected", 
               choices = colnames(tableData), 
               options = list(
                 `actions-box` = TRUE,
                 size = 10,
                 `selected-text-format` = "count > 3"
               ),
               multiple = TRUE
             ))
             
             # column(4,
             #        selectInput("highDeg",
             #                    "Highest Degree:",
             #                    c("All", 
             #                      unique(as.character(data$Highest.degree.offered))))
             # ),
             # column(4,
             #        selectizeInput("state",
             #                    "State:",
             #                    c("All",
             #                      unique(as.character(data$State.abbreviation))))
             # ),
             # column(4,
             #        selectizeInput("reg",
             #                    "Region:",
             #                    c("All",
             #                      unique(as.character(data$Geographic.region))))
             # )
           ),
           # Create a new row for the table.
           DT::dataTableOutput("table")
           )
)


# Define server logic required to draw a histogram
map_data <- data %>% 
  select(Longitude, Latitude, Name)

server <- function(input, output, session) {
  
  mypopup <- paste("Name: ", map_data$Name,"</br>",
                   "Region: ", map_data$Region)
  
  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE, minZoom = 3)
      ) %>% 
      addMarkers(data = map_data, lng = ~Longitude,
                 lat = ~Latitude,
                 label = ~Name,
                 popup = mypopup,
                 clusterOptions = markerClusterOptions()) %>% 
      setView(lng = -98.35, lat = 39.5, zoom = 3) %>% 
      setMaxBounds(lng1 = -120, lng2 = -60, lat1 = 0, lat2 = 70)
    
  })
  
  # Scatterplot
  output$plot <- renderPlotly({
    # palette(c("#E41A1C", "#377EB8"))
    x <- input$xcol
    y <- input$ycol
    
    # Scatterplot not working properly
    plot_ly(vars, x = ~get(x), y = ~get(y), type = "scatter", mode = "markers") %>% 
      layout(title = paste(x, " vs. ", y),
             xaxis = list(title = x),
             yaxis = list(title = y))
    
  })
  
  # Table
  output$table <- DT::renderDataTable(DT::datatable({
    # dat <- tableData
    # if (input$highDeg != "All") {
    #   dat <- data[data$Highest.degree.offered == input$highDeg,]
    # }
    # if (input$state != "All") {
    #   dat <- data[data$State.abbreviation == input$state,]
    # }
    # if (input$reg != "All") {
    #   dat <- data[data$Geographic.region == input$reg,]
    # }
    # dat
    cols = colnames(tableData)
    if (!is.null(input$myPicker)) {
      cols = input$myPicker
    }
    tableData[,cols,drop=FALSE]
    options = list(scrollX = TRUE, sScrollY = '75vh', scrollCollapse = TRUE)
    extensions = list("Scroller")
  }))
}


# Run the application 
shinyApp(ui = ui, server = server)
