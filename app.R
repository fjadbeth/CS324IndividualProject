# Libraries
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
library(shinythemes)


# Data
data <- read.csv("data/IPEDS.csv") %>% 
  ## Selected the variables needed for this project
  select(2, 5, 7:8, 22:24, 27:30, 33, 34, 65, 70:73, 75, 78, 86, 111, 124, 126, 128, 131, 134) %>% 
  ## Renamed the variables 
  rename(High.Degree = Highest.degree.offered,
         Longitude = Longitude.location.of.institution,
         Latitude = Latitude.location.of.institution,
         Applicants = Applicants.total,
         Admissions = Admissions.total,
         Enrolled = Enrolled.total,
         SAT.Reading.25 = SAT.Critical.Reading.25th.percentile.score,
         SAT.Reading.75 = SAT.Critical.Reading.75th.percentile.score,
         SAT.Math.25 = SAT.Math.25th.percentile.score,
         SAT.Math.75 = SAT.Math.75th.percentile.score,
         ACT.25 = ACT.Composite.25th.percentile.score,
         ACT.75 = ACT.Composite.75th.percentile.score,
         Percent.Admitted = Percent.admitted...total,
         Tuition = Tuition.and.fees..2013.14,
         Tuition.Instate = Total.price.for.in.state.students.living.on.campus.2013.14,
         Tuition.Outstate = Total.price.for.out.of.state.students.living.on.campus.2013.14,
         State = State.abbreviation,
         Region = Geographic.region,
         Control = Control.of.institution,
         TEnrollment = Undergraduate.enrollment,
         Percent.Women = Percent.of.undergraduate.enrollment.that.are.women,
         Pecent.Instate = Percent.of.first.time.undergraduates...in.state,
         Percent.Outstate = Percent.of.first.time.undergraduates...out.of.state,
         Percent.Foreign = Percent.of.first.time.undergraduates...foreign.countries,
         GradRate.4yr = Graduation.rate...Bachelor.degree.within.4.years..total,
         Percent.Finan.Aid = Percent.of.freshmen.receiving.any.financial.aid
  ) %>% 
  ## Change the values in Region and High.Degree
  mutate(Region = case_when( 
    Region == "Southeast AL AR FL GA KY LA MS NC SC TN VA WV" ~"Southeast",
    Region == "Far West AK CA HI NV OR WA" ~"Far West",
    Region == "Southwest AZ NM OK TX" ~"Southwest",
    Region == "Rocky Mountains CO ID MT UT WY" ~"Rocky Mountains",
    Region == "New England CT ME MA NH RI VT" ~"New England",
    Region == "Mid East DE DC MD NJ NY PA" ~"Mid East",
    Region == "Great Lakes IL IN MI OH WI" ~"Great Lakes",
    Region == "Plains IA KS MN MO NE ND SD" ~"Plains",
    TRUE ~Region
  ), High.Degree = case_when(
    High.Degree == "Doctor's degree - research/scholarship" ~"Doctor's degree",
    High.Degree == "Doctor's degree - research/scholarship and professional practice" ~"Doctor's degree",
    High.Degree == "Doctor's degree -  professional practice" ~"Doctor's degree",
    High.Degree == "Doctor's degree - other" ~"Doctor's degree",
    TRUE ~High.Degree
  )
  )

vars <- select_if(data, is.numeric)

tableData <- data %>% 
  select(-3, -4) 


# Define UI
ui <- navbarPage(
  ## Theme
  theme = shinytheme("spacelab"), 
  ## App Title
  "College Explorer",
  ## Map Panel
  tabPanel("Map", ## Title
           ## CSS script
           tags$head(
             tags$style(HTML("
             #controls {
               padding: 12px; 
               border: 2px solid #666666; 
               border-radius: 3%; 
               background-color:#FFFFFA; 
               width: 22%; 
               font-size: 18px; 
               opacity: 0.45; 
               transition: opacity 500ms 1s;
             }
             #controls:hover {
                opacity: 0.95;
                transition-delay: 0;
            }"))
           ),
           fluidRow(
             br(),
             br(),
             ## Map output
             leafletOutput("map", width="100%", height=610)
           ),
           ## Options panel
           absolutePanel(
             id = "controls",
             bottom = 65, 
             right = 25, 
             fixed = TRUE,
             draggable = TRUE, 
             height = 620,
             ## Selecting region
             pickerInput(
               inputId = "region",
               label = "Select/deselect Region(s):",
               choices = unique(data$Region),
               options = list(
                 `actions-box` = TRUE,
                 size = 9,
                 `selected-text-format` = "count > 3"
               ),
               multiple = TRUE
             ),
             ## Selecting public/private or both
             pickerInput(
               inputId = "control",
               label = "Select/deselect Public/Private:",
               choices = unique(data$Control),
               options = list(
                 `actions-box` = TRUE,
                 size = 2,
                 `selected-text-format` = "count > 3"
               ),
               multiple = TRUE
             ),
             ## Selecting highest degree offered
             prettyCheckboxGroup(
               inputId = "degree",
               label = "Select Degree(s): ",
               choices = c("Doctor's degree", "Master's degree", "Bachelor's degree"),
               icon = icon("thumbs-up"),
               status = "default", 
               shape = "curve", 
               animation = "pulse",
               selected = unique(data$High.Degree)
             ),
             ## Selecting Tuition
             sliderInput("tuition", "Tuition:",
                         1032, 49138, c(1100, 20000), step = 200),
             ## Selecting Acceptance rate
             sliderInput("admit", "Acceptance Rate::",
                         6, 100, c(50, 80), step = 5),
             ## Selecting Total enrollment
             sliderInput("enrol", "Total Enrollment:",
                         0, 51333, c(0, 10000), step = 100)
            ),
  ),
  ## Scatterplot Panel
  tabPanel("Plot", ## Title
           sidebarPanel(
             ## Select box
             selectizeInput('xcol', 'X Variable', colnames(vars)[3:length(vars)], selected = colnames(vars)[3]),
             selectizeInput('ycol', 'Y Variable', colnames(vars)[3:length(vars)], selected = colnames(vars)[4])
             ),
           mainPanel(
             tabsetPanel(
               tabPanel("Scatterplot",
                        ## Scatterplot output
                        plotlyOutput('plot')
                        ),
               tabPanel("Regression",
                        ## Regression output
                        plotlyOutput('scatPlot'),
                        br(),
                        br(),
                        fluidRow(
                          style = "margin: auto; width: 600px",
                          ## Text output
                          verbatimTextOutput("summa"))),
               )
             )
           ),
  ## Table Panel
  tabPanel("Table", ## Title
           fluidRow(
             column(6, 
             ## Selecting variables 
             pickerInput(
               inputId = "myPicker", 
               label = "Select/deselect variables:", 
               choices = colnames(tableData %>% select(-1)), 
               options = pickerOptions(
                 `actions-box` = TRUE,
                 size = 10,
                 maxOptions = 10,
                 maxOptionsText = "Upto 10 variables allowed for selection",
                 `selected-text-format` = "count > 3"
               ),
               multiple = TRUE
             ))
           ),
           ## Table output
           DT::dataTableOutput("table")
           ),
  ## About Panel
  tabPanel("About", ## Title
           h1(strong("About College Explorer"),
              style = "text-align:center; font-family: 'Times New Roman', serif"),
           column(12,
                  p("This application is meant to be a tool for users to understand trends and correlations better using
                    the college data set in 2013-14 from IPEDS. Using a map, a scatter plot, and a table, users have a 
                    chance to explore and play around with the visualizations to investigate any interesting relationships,
                    as well as new findings. The data set used for this project consists of various variables such as tuition, 
                    percent of women enrolled, and region, which would be helpful for people from different backgrounds. 
                    This ranges from professors who might be interested in finding any social inequalities to high schools 
                    students who might want to look into college data sets so that they can start narrowing down their 
                    interests. The limitation of this project is that the data set is outdated. Thus, any discovery made 
                    using this visualization may not be applicable today.",
                    style= "margin: 40px; margin-top:10px"),
                  img(src = "college.jpeg", height = "60%", width = "60%", style = 'display:block;margin-right:30%; margin-left:30%; width:40%; margin-top: 25px; margin-bottom:2px')
           )
  )
)


# Define server 
server <- function(input, output, session) {
  ## Map
  output$map <- renderLeaflet({
    
    ## Making new dataset for plotting
    mark_data <- data

    ## Based on the input, filter out the necessary information
    if (!is.null(input$region)) {
      mark_data <- filter(mark_data, Region %in% input$region)
    } 
    if (!is.null(input$control)) {
      mark_data <- filter(mark_data, Control %in% input$control)
    } 
    if (!is.null(input$degree)) {
      mark_data <- filter(mark_data, High.Degree %in% input$degree)
    }
    if (!is.null(input$admit)) {
      mark_data <- filter(mark_data, Percent.Admitted >= input$admit[1], Percent.Admitted <= input$admit[2])
    }
    if (!is.null(input$enrol)) {
      mark_data <- filter(mark_data, TEnrollment >= input$enrol[1], TEnrollment <= input$enrol[2])
    }
    if (!is.null(input$tuition)) {
      mark_data <- filter(mark_data, Tuition >= input$tuition[1], Tuition <= input$tuition[2])
    }
    
    
    ## Texts to show when users clicks the pin
    mypopup <- paste("<strong>Name: </strong>", mark_data$Name,"</br>",
                     "<strong>Highest Degree Offered: </strong>", mark_data$High.Degree, "</br>",
                     "<strong>Public/Private: </strong>", mark_data$Control, "</br>",
                     "<strong>Total Enrollment: </strong>", mark_data$TEnrollment, "</br>",
                     "<strong>State: </strong>", mark_data$State, "</br>",
                     "<strong>Region: </strong>", mark_data$Region)
    
    ## Draw markers and labels
    icons <- awesomeIcons(
      icon = 'ios-close',
      library = 'ion'
    )
    
    tryCatch(
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = FALSE, minZoom = 3)) %>%
        addAwesomeMarkers(data = mark_data,
                          lng = ~Longitude,
                          lat = ~Latitude,
                          label = ~Name,
                          popup = mypopup,
                          icon = icons,
                          clusterOptions = markerClusterOptions()
        ) %>% 
        setView(lng = -75.35, lat = 45.5, zoom = 3.49),
        ## Handles error when data not found
        error = function(c) {
          leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(noWrap = FALSE, minZoom = 3)) %>%
            setView(lng = -75.35, lat = 45.5, zoom = 3.49)
        }
    )
  })
  
  # Scatterplot
  output$plot <- renderPlotly({
    ## Creating temporary values to store
    x <- input$xcol
    y <- input$ycol
    
    ## Creating dataset that contains numeric values and the college name
    vars_name <- left_join(vars, data)
    
    ## Plot and draw lines + labels
    plot_ly(vars_name, x = ~get(x), y = ~get(y), type = "scatter", mode = "markers", text = ~paste(Name)) %>%
      layout(title = paste(x, " vs. ", y),
             xaxis = list(title = x),
             yaxis = list(title = y))
  })
  
  # Regression
  output$scatPlot <- renderPlotly({
    ## Creating temporary values to store
    x <- input$xcol
    y <- input$ycol
    
    ## Creating new dataset for drawing the line of best fit
    scat <- vars %>% 
      # Get rid of all the rows that contains NA
      na.omit()
    ## Storing the line of best fit
    fit <- lm(get(y) ~ get(x), data = scat)
    
    ## Display the summary statistics of regression
    output$summa <- renderPrint({
      summary(fit)
    })
    
    ## Plot and draw lines + labels
    plot_ly(scat, x = ~get(x), y = ~get(y), type = "scatter", mode = "markers") %>%
      add_lines(x = ~get(x), y = fitted(fit)) %>% 
      layout(title = paste(x, " vs. ", y),
             xaxis = list(title = x),
             yaxis = list(title = y))
  })
  
  # Table
  output$table <- DT::renderDataTable(DT::datatable({
    cols = colnames(tableData)
    if (!is.null(input$myPicker)) {
      cols = append(c("Name"), input$myPicker)
    }
    
    dat <- tableData[,cols,drop = FALSE]
    extensions = list("Scroller")
    
    dat
  }))
}


# Run the application 
shinyApp(ui = ui, server = server)
