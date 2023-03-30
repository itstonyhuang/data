library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(sf)
library(sp)
library(shinycssloaders)
library(tidyverse)
library(leaflet)
library(gt)
library(plotly)
library(DT)
library(terra)
library(shinyalert)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(rintrojs)
library(stringr)
library(leafpop)
library(rsconnect)

boston_flights <- read.table("https://raw.githubusercontent.com/itstonyhuang/data/main/bosflights18.txt", header = TRUE, sep=",")


#wrangling the data set 
boston_flights <- boston_flights %>%
  mutate(flightdate = make_datetime(year, month, dayofmonth), 
         weekday_name = case_when(
           weekday == 1 ~ "Monday",
           weekday == 2 ~ "Tuesday",
           weekday == 3 ~ "Wednesday",
           weekday == 4 ~ "Thursday",
           weekday == 5 ~ "Friday",
           weekday == 6 ~ "Saturday",
           weekday == 7 ~ "Sunday",
           TRUE ~ NA_character_
         ), 
         weather_delay = as.factor(weather_delay),
         nas_delay = as.factor(nas_delay),
         security_delay = as.factor(security_delay),
         late_aircraft_delay = as.factor(late_aircraft_delay),
         carrier_delay = as.factor(carrier_delay),
         bos_depart = as.factor(bos_depart))

#more wrangling 
# bos <- boston_flights %>%
#   group_by(year, flightnum) %>%
#   summarize(n = sum(flight_time)) %>%
#   group_by(year) %>%
#   mutate(prop = n/sum(n)) 


#new wrangling
# bos <- boston_flights %>%
#   group_by(year, flightnum) %>%
#   summarize(n = sum(flight_time)) %>%
#   group_by(year) %>%
#   mutate(prop = n/sum(n)) 


# 
# User interface
# ui <- fluidPage(
#   titlePanel("Which Flights in Boston have a greater flight time?"),
#   sidebarLayout(
#     sidebarPanel(
#       # Create a text input widget
#       selectizeInput(inputId = "Flight Number",
#                      label = "Enter Flight Number here",
#                      choices = NULL,
#                      multiple = TRUE),
#       p("Put a single space between the numbers."),
#       radioButtons(inputId = "variable",
#                    label = "Variable of Interest",
#                    choices = c("n", "prop"),
#                    selected = "prop"),
#        sliderInput("year_range", "Range of Years:",
#                    min = min(boston_flights$year),
#                    max = max(boston_flights$year),
#                    value = c(2018,2018),
#                    sep=""),
#        actionButton("update", "Update Results!")),
#      mainPanel(
#        plotOutput(outputId = "graph"),
#       DTOutput(outputId = "table")
#      )
#    )
# )
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Boston Flights"),
#   dashboardSidebar(
#       sidebarMenu(
#       menuItem("Home Page", tabName = "home"),
#       menuItem("Charts", tabName = "charts")
#       )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "home",
#               h2("Home tab")
#       ),
#       
#       tabItem(tabName = "chart",
#               h2("chart tab")
#     )
#   )
#     
#   )
# )

my_height = "25em"

#ui of the dashboard
ui <- navbarPage("Boston Flights", theme = shinytheme("flatly"),
                 tabPanel("Home", icon = icon("home"),
                          sidebarLayout(
                            sidebarPanel(
                              h4("Learn how to use \nthe various features of this app:"),     width = 2, 
                              tags$style(".btn-block { width: 100%; margin-bottom: 20px; }"),
                              actionButton("homeBtn", "Home",class = "btn-block", icon = icon("home")),
                              actionButton("visioBtn", "Visualization",class = "btn-block",icon = icon("map")),
                              actionButton("tableBtn", "Table", class = "btn-block",icon = icon("table")),
                              actionButton("codeBtn", "Code", class = "btn-block",icon = icon("code")),
                              actionButton("aboutBtn", "About", class = "btn-block", icon = icon("person")),
                              tags$div(
                                style = "padding: 10px; color: #333333; font-size: 15px; text-align: center;",
                                "Discover the story behind Boston's flights in 2018 with our interactive dashboard! 
                           With clear visualizations and easy-to-use filters, you can explore flight trends and patterns to gain valuable insights. 
                           Let us help you unlock the secrets of Boston's skyways!")
                            ),
                            mainPanel(
                              h1("Analysing 2018 Flights at Boston Logan Airport", align="center"),
                              h6("For Use by Stat 108 students", align="center"),
                              fluidRow(tags$img(src='https://content.r9cdn.net/rimg/dimg/e9/1b/3241ae57-city-25588-178c675d416.jpg?width=1200&height=630&crop=true'), 
                                       align = "center", style = paste0("width: 100%; height: ", my_height, ";"))
                            )
                          )
                 ),
                 # tabPanel("Visualization", icon = icon("map"),
                 #          sidebarLayout(
                 #            sidebarPanel(
                 #              # Create a text input widget
                 #              selectizeInput(inputId = "Flight Number",
                 #                             label = "Enter Flight Number here",
                 #                             choices = NULL,
                 #                             multiple = TRUE),
                 #              p("Put a single space between the numbers."),
                 #              radioButtons(inputId = "variable",
                 #                           label = "Variable of Interest",
                 #                           choices = c("n", "prop"),
                 #                           selected = "prop"),
                 #              sliderInput("year_range", "Range of Years:",
                 #                          min = min(boston_flights$year),
                 #                          max = max(boston_flights$year),
                 #                          value = c(2018,10),
                 #                          sep=""),
                 #              actionButton("update", "Update Results!"),
                 #              width = 4),
                 #            mainPanel(
                 #              h1("Analysing 2018 Flights at Boston Logan Airport", align="center"),
                 #              plotOutput(outputId = "graph"),
                 # 
                 #            )
                 #          )
                 # ),
                 tabPanel("Visualization", icon = icon("map"),
                          sidebarPanel(
                          # Create a text input widget
                            textInput(inputId = "dest",
                                      label = "destination",
                                      value = "LGA MCO JFK ATL LAX"),
                            p("Put a single space between the destinations."),
                          ),
                          mainPanel(
                            h1("Flights leaving Boston", align="center"),
                            plotOutput(outputId = "graph"),
                            DTOutput(outputId = "table")
                          )
                 ),      
                 # tabPanel("Table", icon = icon("table"),
                 #          h2("Table tab")
                 # ),
                 tabPanel("Code", icon = icon("code"),
                          h2("Code tab"),
                          verbatimTextOutput("code"),
                          sidebarLayout(
                            sidebarPanel (
                              downloadButton("download", label = "Download Source Code"),
                              textOutput("message")
                              
                            ),
                            mainPanel ())
                 ),
                 tabPanel("About", icon = icon("person"),
                          fluidRow(
                            column(12, h2("Dashboard Creators", style = "center"),
                                   tabsetPanel(
                                     tabPanel("Creator 1", "Asteria Chilambo"),
                                     tabPanel("Creator 2", "Tony Huang"),
                                     tabPanel("Creator 3", "Corbin")
                                   ))
                          )
                 )
)

#Server function 
server <- function(input, output, session){

  
  # output$message <- renderText({
  #   if(input$download){
  #     file.copy("bostonflight.Rmd", "bostonflight_copy.Rmd")
  #     return("Source code downloaded successfully!")
  #   }
  # })
  
  # output$plot1 <- renderPlot({
  #   plot(rnorm(10), main = "Plot 1")
  # })
  # 
  # output$plot2 <- renderPlot({
  #   plot(rnorm(10), main = "Plot 2")
  # })
  
  #Welcome the users to our DashBoard Page
  shinyalert(title = "Welcome to the Boston Flights Dashboard", 
             confirmButtonText = "I Understand",
             type = "info",
             text = "Your one-stop shop for exploring and analyzing flight data from Boston Logan International Airport in 2018. With this dashboard, you can dive deep into the data to uncover insights and trends related to flight delays, cancellations, and more. Use the sidebar navigation to access different sections of the app, including visualizations, tables, and code snippets. Our goal is to help you gain a deeper understanding of flight patterns and improve your travel experience. Enjoy!Users are invited to provide 
             feedback by filling out a Google Form. The link to this form can be found on the 'About' page of this dashboard.")
  
  observeEvent(input$homeBtn, {
    showModal(
      modalDialog(
        title = "Home",
        "Welcome to the Home tab! This is the landing page for the Boston flights dashboard. 
        From here, you can access all the other tabs, including the visualization, table, and code 
        tabs. Explore flight data and gain insights into flight patterns, delays, and other important details. 
        Use the navigation bar to easily move between tabs and discover everything the Boston flights dashboard has to offer."
      )
    )
  })
  
  observeEvent(input$visioBtn, {
    showModal(
      modalDialog(
        title = "Visualization",
        "Welcome to the visualization tab! Here, you can explore visualizations of Boston flights in 2018. 
        Gain insights into flight patterns, delays, and more through interactive charts and graphs."
      )
    )
  })
  
  observeEvent(input$tableBtn, {
    showModal(
      modalDialog(
        title = "Table",
        "Welcome to the table tab! Here, you can explore the Boston flights dataset in a tabular format. 
        View detailed information on flight schedules, delays, and other important details. Use the search and filter functions to easily find the data you need."
      )
    )
  })
  
  observeEvent(input$codeBtn, {
    showModal(
      modalDialog(
        title = "Code",
        "Welcome to the code tab! Here, you can explore the R code used to create the Boston flights dashboard. Get an in-depth look at how the data was cleaned and manipulated, as well as how the charts and tables were created. 
        Use this code as a starting point for your own data analysis projects!"
      )
    )
  })
  
  observeEvent(input$aboutBtn, {
    showModal(
      modalDialog(
        title = "About",
        "Welcome to the About tab! Here, you can learn more about the team behind the Boston flights dashboard. Meet the developers and mentors who worked on this project, as well as explore the sources of the data used to create the dashboard. Gain insights into the inspiration behind this project and the technologies used to bring it to life.",
        easyClose = TRUE
      )
    )
  })
  
  
  #coding for visuals
  
  dat <- reactive({
    boston_flights %>% filter(dest %in% c(unlist(str_split(input$dest, " "))))
  })
  
  output$graph <- renderPlot({
    ggplot(data = dat(), 
           mapping = aes(x = dest, fill = carrier)) +
      geom_bar()
  })
  
  #coding for table
  output$table <- renderDT({
    dat() %>% 
      group_by(dest, carrier) %>% 
      summarize(count = n(), avg_flight_time = mean(flight_time), 
                avg_dist = mean(distance))
  })
  
  my_script <- readLines("/Users/tonyhuang/Desktop/stat_108/project1-group10/ShinyApps/bostonflights/app.r")

  #render the R script in the code tab
  output$code <- renderPrint({
    cat(my_script, sep = "\n")
  })
  

  
  # updateSelectizeInput(session, 'flightnum',
  #                      choices = unique(boston_flights$flightnum),
  #                      server = TRUE)
  # 
  # dat_names1 <- eventReactive(input$update, {
  #   bos %>%
  #     filter(flightnum %in% c(unlist(str_split(input$flightnum, " "))),
  #            year >= input$year_range[1],
  #            year <= input$year_range[2])  %>%
  #     mutate(y_var = .data[[input$variable]])
  # })
  # 
  # y_label <- eventReactive(input$update, input$variable)
  # 
  # output$graph <- renderPlot({
  #   
  #   ggplot(data = dat_names1(),
  #          mapping = aes(x = year, y = y_var,color = flightnum)) +
  #     geom_line(linewidth = 2)+
  #     labs(y = y_label())
  # })
  # 
  # 
  # 
  # dat_names_agg <- reactive({
  #   dat_names1() %>%
  #     group_by(flightnum) %>%
  #     summarize(count = sum(n)) %>%
  #     arrange(desc(count))
  # })
  # output$table <-  renderDT({
  #   datatable(dat_names_agg(),
  #             options = list(paging = FALSE,
  #                            searching = FALSE,
  #                            orderClasses = TRUE))
  # })
  
  
  
}

#Creates app
shinyApp(ui = ui, server = server)

