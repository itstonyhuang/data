# Shiny packages
library(shiny); library(shinyalert); library(shinythemes) 
library(shinydashboard); library(shinydashboardPlus); library(shinyWidgets)
library(rintrojs); library(shinycssloaders)

# Graphing packages
library(leaflet);  library(plotly); library(gt); library(DT)
library(ggthemes); library(lattice); library(leafpop); library(scales)

# Data management packages
library(tidyverse); library(lubridate); library(stringr); library(terra)
library(sf); library(sp)

# deploy app to web
library(rsconnect)


# Read in 2018 Boston Flight Data
boston_flights <- read.table("https://raw.githubusercontent.com/itstonyhuang/data/main/bosflights18.txt", header = TRUE, sep=",")

# Read in airport meta data that largely contains IATA codes and geo-coordinates
airports <- read.table("https://raw.githubusercontent.com/itstonyhuang/data/main/airports.dat.txt", header = FALSE, sep=",")
oldnames = c("V1", "V2", "V3", "V4", "V5", "V6", "V7",
             "V8", "V9", "V10", "V11", "V12", "V13", "V14")
newnames = c("id", "name", "city", "country", "IATA", "ICAO",
             "latitude", "longitude", "altitude", "timezone", "DST",
             "database_timezone", "type", "source")
airports = airports %>% rename_at(vars(oldnames), ~ newnames) %>%
  select(name, country, IATA, latitude, longitude)

# Dataset that the leaflet will use mostly
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
           TRUE ~ NA_character_), 
         weather_delay = as.factor(weather_delay),
         nas_delay = as.factor(nas_delay),
         security_delay = as.factor(security_delay),
         late_aircraft_delay = as.factor(late_aircraft_delay),
         carrier_delay = as.factor(carrier_delay),
         bos_depart = as.factor(bos_depart)) %>%
  filter(origin != "other", dest != "other")

# Datasets later for additional geom for the leaflet
airport_counts <- left_join(boston_flights %>% count(dest), airports, by = c("dest" = "IATA"))

airport_trajectories <- boston_flights %>%
  filter(dest != "BOS") %>%
  group_by(origin, dest) %>%
  summarize(avg_flight = mean(flight_time), n = n()) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c("dest", "origin")) %>%
  left_join(., airports[,c("IATA", "latitude", "longitude")], by = c("value" = "IATA"))


# Dataset wrangling for the table  
boston_flight <- boston_flights %>%
  filter(origin != "other", dest != "other") %>%
  mutate(flightdate = make_datetime(year, month, dayofmonth), 
         weekday_name = case_when(
           weekday == 1 ~ "Monday",
           weekday == 2 ~ "Tuesday",
           weekday == 3 ~ "Wednesday",
           weekday == 4 ~ "Thursday",
           weekday == 5 ~ "Friday",
           weekday == 6 ~ "Saturday",
           weekday == 7 ~ "Sunday",
           TRUE ~ NA_character_), 
         carrier = recode(carrier,"B6" = "JetBlue" , "9E" = "Endeavor Air",
                          "AA" = "American Airlines" , "DL" = "Delta Airlines", 
                          "UA" = "United Airlines", "YX" = "Republic Airlines" ,
                          "WN" = "Southwest Airlines", "AS" = "Alaska Airlines",
                          "NK" = "Spirit Airlines", "MQ" = "Envoy Air", 
                          "OO" = "Skywest Airlines" , "EV" = "ExpressJet",
                          "VX" = "Virgin America", "YV" = "Mesa Airlines"),
         origin = recode(origin, "SFO" = "SAN FRANSCISCO", "ATL" = "ATLANTA",
                         "BOS" = "BOSTON", "LAX" = "LOS ANGELES", "BWI"="BALTIMORE",
                         "CLT" = "CHARLOTTE", "DCA" = "WASHINGTON, DC", "DEN" = "DENVER",
                         "DFW" = "DALLAS", "DTW" = "DETROIT", "EWR" = "NEWARK",
                         "FLL" = "FORT LAUDERDALE", "JFK" = "NEWYORK,JFK",
                         "LGA" = "NEWYORK, LGA", "MCO" = "ORLANDO",
                         "MSP" = "MINNEAPOLIS", "ORD" = "CHICAGO, ORD",
                         "PHL" = "PHILADELPHIA", "PIT" = "PITTSBURGH", 
                         "RDU" = "DURHAM"),
         dest = recode(dest, "SFO" = "SAN FRANSCISCO", "ATL" = "ATLANTA",
                       "BOS" = "BOSTON", "LAX" = "LOS ANGELES", "BWI"="BALTIMORE",
                       "CLT" = "CHARLOTTE", "DCA" = "WASHINGTON, DC", "DEN" = "DENVER",
                       "DFW" = "DALLAS", "DTW" = "DETROIT", "EWR" = "NEWARK",
                       "FLL" = "FORT LAUDERDALE", "JFK" = "NEWYORK,JFK",
                       "LGA" = "NEWYORK, LGA", "MCO" = "ORLANDO",
                       "MSP" = "MINNEAPOLIS", "ORD" = "CHICAGO, ORD",
                       "PHL" = "PHILADELPHIA", "PIT" = "PITTSBURGH", 
                       "RDU" = "DURHAM"),
         weather_delay = recode(weather_delay, "0" = "NO", "1" = "YES"),
         nas_delay = recode(nas_delay, "0" = "NO", "1" = "YES"),
         security_delay = recode(security_delay, "0" = "NO", "1" = "YES"),
         weather_delay = recode(weather_delay, "0" = "NO", "1" = "YES"),
         late_aircraft_delay = recode(late_aircraft_delay,"0" = "NO", "1" = "YES"),
         carrier_delay = recode(carrier_delay,"0" = "NO", "1" = "YES"),
         weather_delay = as.factor(weather_delay),
         nas_delay = as.factor(nas_delay),
         security_delay = as.factor(security_delay),
         late_aircraft_delay = as.factor(late_aircraft_delay),
         carrier_delay = as.factor(carrier_delay),
         weekday_name = as.factor(weekday_name),
         bos_depart = as.factor(bos_depart),
         carrier = as.factor(carrier),
         origin = as.factor(origin),
         dest = as.factor(dest)) %>%
  rename ("Aircraft Delay" = "late_aircraft_delay",
          "Weather Delay" = "weather_delay",
          "NAS Delay" = "nas_delay",
          "Security Delay" = "security_delay",
          "Carrier Delay" = "carrier_delay",
          "Destination" = "dest",
          "Flight Number" = "flightnum",
          "Distance" = "distance",
          "Flightdate" = "flightdate",
          "Weekday" = "weekday_name",
          "Flight Duration (min)" = "flight_time",
          "Origin" = "origin",
          "Carrier" = "carrier") %>%
  select(!c(weekday,year,depart,wheels_off,schedule_depart,tailnum,
            month,dayofmonth,bos_depart))

my_height = "25em"

# User-interface of the dashboard
ui <- navbarPage("Boston Flights", theme = shinytheme("sandstone"),
                 tabPanel("Home", icon = icon("home"),
                          sidebarLayout(
                            sidebarPanel(
                              h3("Guide: Learning to Navigate the Apps", tags$br(), tags$br()), width = 2,
                              tags$style(".btn-block { width: 100%; margin-bottom: 14px; }"),
                              actionButton("homeBtn", "Home",class = "btn-block", icon = icon("home")),
                              actionButton("mapBtn", "Flight Map", class = "btn-block", icon = icon("map")),
                              actionButton("visioBtn", "Summary Statistics",class = "btn-block",icon = icon("map")),
                              actionButton("tableBtn", "Table", class = "btn-block",icon = icon("table")),
                              actionButton("codeBtn", "Code", class = "btn-block",icon = icon("code")),
                              actionButton("aboutBtn", "About", class = "btn-block", icon = icon("person")),
                              tags$div(style = "padding: 10px; color: #333333; font-size: 15px; text-align: center;",
                                       "Discover the story behind Boston's flights in 2018 with our interactive dashboard!
                           With clear visualizations and easy-to-use filters, you can explore flight trends and patterns to gain valuable insights.
                           Let us help you unlock the secrets of Boston's skyways!")),
                            mainPanel(
                              h1("Analyzing 2018 Flights at Boston Logan Airport", align="center"),
                              h5("A Stat 108 Project", align="center"),
                              fluidRow(tags$img(src='https://content.r9cdn.net/rimg/dimg/e9/1b/3241ae57-city-25588-178c675d416.jpg?width=1200&height=630&crop=true'),
                                       align = "center", style = paste0("width: 100%; height: ", my_height, ";"))
                            )
                          )
                 ),
                 tabPanel("Flight Map", icon = icon("map"),
                          sidebarPanel(width = 15,
                                       setSliderColor("#1E53C3", 1),
                                       sliderInput("flightrange", "Flight Date Range - Begin by sliding across time!",
                                                   min = as.Date("01-01-2018","%m-%d-%Y"),
                                                   max = as.Date("12-31-2018","%m-%d-%Y"),
                                                   value = c(as.Date("01-01-2018","%m-%d-%Y"),
                                                             as.Date("12-31-2018","%m-%d-%Y")),
                                                   timeFormat = "%m-%d-%Y")),
                          mainPanel(h1("Flights Leaving Boston", align = "center")),
                          leafletOutput(outputId = "map", width = "75%", height = "500px")),                 
                 tabPanel("Summary Statistics", icon = icon("map"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "dest",
                                          label = "What's your destination(s)?",
                                          choices = c("ATL", "BWI", "CLT", "DCA", "DEN", "DFW", "DTW", "EWR", "FLL", "JFK", "LAX", "LGA", "MCO", "MSP", "ORD", "PHL", "PIT", "RDU", "SFO"), multiple = 20),
                              p(style = "font-size: 16px;",
                                HTML("Boston Flights at Select Airports and their 3-letter codes:
                                      <li>Hartsfield-Jackson Atlanta International Airport = ATL</li>
                                      <li>Baltimore-Washington International Airport = BWI</li>
                                      <li>Charlotte Douglas International Airport = CLT</li>
                                      <li>Ronald Reagan Washington National Airport = DCA</li>
                                      <li>Denver International Airport = DEN</li>
                                      <li>Dallas/Fort Worth International Airport = DFW</li>
                                      <li>Detroit Metropolitan Wayne County Airport = DTW</li>
                                      <li>Newark Liberty International Airport = EWR</li>
                                      <li>Fort Lauderdale-Hollywood International Airport = FLL</li>
                                      <li>John F. Kennedy International Airport = JFK</li>
                                      <li>Los Angeles International Airport = LAX</li>
                                      <li>LaGuardia Airport = LGA</li>
                                      <li>Orlando International Airport = MCO</li>
                                      <li>Minneapolis-Saint Paul International Airport = MSP</li>
                                      <li>O'Hare International Airport = ORD</li>
                                      <li>Philadelphia International Airport = PHL</li>
                                      <li>Pittsburgh International Airport = PIT</li>
                                      <li>Raleigh-Durham International Airport = RDU</li>
                                      <li>San Francisco International Airport = SFO</li>"
                                )),
                            ),
                            mainPanel(
                              h1("Boston Flights - Outgoing", align="center"),
                              plotOutput(outputId = "graph"),
                              DTOutput(outputId = "table"))
                          )),
                 tabPanel("Table", icon = icon("table"),
                          mainPanel(
                            h1("Explore Flights to and from Boston Logan Airport", align="center"),
                            DT::dataTableOutput("myTable"))),
                 tabPanel("Code", icon = icon("code"), verbatimTextOutput("code")),
                 tabPanel("About", icon = icon("person"),
                          tabsetPanel(
                            tabPanel("Our Team",
                                     fluidRow(column(width = 4, tags$img(src = "https://media.licdn.com/dms/image/C4E03AQE3BiSyL8Vb5A/profile-displayphoto-shrink_800_800/0/1662057271369?e=2147483647&v=beta&t=388qd7RygM5J_yQY-Yo7ivMnyvAJHXDW2YSVvzpkiLk", height = 300, width = 300),
                                                     tags$div(style = "text-align: left; font-size: 20px; font-weight: bold; margin: 5px 0;", "Tony Huang"),
                                                     div(style = "width: 400px; overflow: hidden; text-overflow: ellipsis;",
                                                         p(style = "font-size: 16px;",
                                                           HTML("Tony is a junior from Quincy House studying Economics and Statistics.
                               He is originally from Orlando, FL. At Harvard, he is Co-President of the Harvard Undergraduate
                               Economics Association and Vice President of the Statistics Club. Outside of class,
                               he is always dragging his friends to sample the latest ice cream flavors with him!
                               This summer, he will be interning at West Monroe as a Mergers & Acquisitions Consultant in NYC.
                               He aspires to become the CEO of a Fortune 500 company in the near future!")),
                                                         )),
                                              column(width = 4,  img(src = "https://images.squarespace-cdn.com/content/v1/628beaedda3c807dda0c134d/cf72e9ff-3435-43b1-ab33-9f3ae20a8f0c/unnamed.png?format=1000w", height = 300, width = 300),
                                                     tags$div(style = "text-align: left; font-size: 20px; font-weight: bold; margin: 5px 0;", "Corbin Lubianski"),
                                                     div(style = "width: 400px; overflow: hidden; text-overflow: ellipsis;",
                                                         p(style = "font-size: 16px;",
                                                           HTML("Corbin is an avid enthusiast of statistics and how we can apply it to improve our communities.
                             As a junior in Quincy House, Corbin is the Co-Treasurer of the Harvard Undergraduate Association and currently works as a course assistant for Econ 10b and Econ 50.
                             Outside of class, he loves to play board games with friends like Catan or on the Switch like Smash.
      
                             After graduation, Corbin aspires to work at the Federal Reserve as a research assistant with hopes to manage the levers of monetary policy at a wider level in the future!")),
                                                         )
                                              ),
                                              column(width = 4,  img(src = "https://images.squarespace-cdn.com/content/v1/54f50fb4e4b0014ec1a1959f/1660754145116-5VY87EMDQMOLIP81YD3A/Asteria%2Bhcfa.jpg", height = 300, width = 300),
                                                     tags$div(style = "text-align: left; font-size: 20px; font-weight: bold; margin: 5px 0;", "Asteria Chilambo"),
                                                     div(style = "width: 400px; overflow: hidden; text-overflow: ellipsis;",
                                                         p(style = "font-size: 16px;",
                                                           HTML("Asteria is a senior in Leverett house at Harvard studying pure mathematics.
                             Hailing from Tanzania, Asteria has always been fascinated by the beauty and power of mathematics, as well as its intersection with data science and statistics.
                             In addition to her academic pursuits, Asteria is deeply committed to the empowerment of girls in STEM, using her knowledge and experience to help create opportunities for young women in these fields.
                             In her free time, Asteria cherishes spending time with her family and loved ones.
                             ")
                                                           )),
                                                     ))),
                            tabPanel("Data",
                                     fluidRow(
                                       column(width = 6, h2("Organizations", align = "center"),
                                              tags$div(
                                                style = "display: flex; justify-content: center;",
                                                tags$img(
                                                  src = "https://www.bts.gov/sites/bts.dot.gov/files/2022-11/30thAnniversaryLoge_0.png",
                                                  style = "height: 300px; width: 300px; object-fit: cover; margin-right: 10px;"
                                                ),
                                                tags$img(
                                                  src = "https://www.bts.gov/sites/bts.dot.gov/files/2023-03/Air%20Traffic.png",
                                                  style = "height: 300px; width: 300px; object-fit: cover;"
                                                )
                                              ),
                                              div(style="text-align:center;",
                                                  tags$img(src = "https://statistics.fas.harvard.edu/sites/hwpi.harvard.edu/files/styles/os_files_xxlarge/public/statistics-2/files/revised_plain_logo.jpg?m=1541772887&itok=pkVj1fpV", height = 400, width = 400))
                                       ),
                                       column(width = 6,
                                              h2("About", align = "center"),
                                              h3("Organizations", align = "center"),
                                              tags$p("This analysis was completed as part of a class project for Introduction to Statistical Computing with R,
                                         also known as Stat 108, which is taught by Dr. Kelly McConville. Dr. McConville is a Senior Lecturer
                                         on Statistics and a Survey Statistician at Harvard University. The goal of the project was to create
                                         an interactive dashboard, and this analysis represents the culmination of those efforts.", style = "font-size: 16px;"),
                                              h3("Data Sources", align = "center"),
                                              tags$p("The data used in this analysis was obtained from the US Department Bureau of Transportation.
                                         The Bureau provides comprehensive statistics on air transportation in the United States,
                                         including flight schedules, fares, and passenger traffic. We accessed and utilized this publicly
                                         available data to conduct our analysis and create this dashboard. Here is the link to the source",
                                                     tags$a("US Department Bureau of Transportation", href = "https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FGJ&QO_fu146_anzr=b0-gvzr"), style = "font-size: 16px;"),
                                              h3("Disclaimer", align = "center"),
                                              tags$p("The data used in this analysis was obtained from the US Department of Transportation Bureau of Transportation Statistics.
                                         The authors of this analysis are not responsible for the accuracy, completeness, or reliability of the data.
                                         Any conclusions drawn from this analysis are solely those of the authors and do not necessarily reflect the views
                                         of the US Department of Transportation or any of its agencies.", style = "font-size: 16px;"),
                                              h3("App Goals", align = "center"),
                                              tags$p("Our app is designed to provide users with an interactive experience exploring flight data from the US Department Bureau of Transportation. Our goals for this app are to:
                                   provide users with an engaging and informative way to explore flight data;
                                   enable users to filter and visualize flight data in an intuitive manner,
                                  and foster a deeper understanding of flight patterns and trends to and from Boston
                                   We hope that this app will provide a valuable resource for anyone interested in flight data and contribute to a better understanding of air travel in the United States.", style = "font-size: 16px;"))
                                     ))
                          )
                 )
)


# Server function 
server <- function(input, output, session){
  
  # Read the rscript
  my_script <- readLines("https://raw.githubusercontent.com/itstonyhuang/data/main/app.r")
  
  # Render the R script in the code tab
  output$code <- renderPrint({
    cat(my_script, sep = "\n")
  })
  
  
  # Welcome the users to our DashBoard Page
  shinyalert(title = "Welcome to the Boston Flights Dashboard", 
             confirmButtonText = "I Understand",
             type = "info",
             text = "Your one-stop shop for exploring and analyzing flight data from Boston Logan International Airport in 2018. With this dashboard, you can dive deep into the data to uncover insights and trends related to flight delays, and more. Use the sidebar navigation to learn how to use the different tabs of the app. Our goal is to help you gain a deeper understanding of flight patterns and improve your travel experience. Enjoy!") 
  #Users are invited to provide feedback by emailing us. Our Contact information can be found on the 'About' page of this dashboard.")
  

  
  # Message for the home button
  observeEvent(input$homeBtn, {
    showModal(
      modalDialog(
        title = "Home",
        "Welcome to the Home button! This is the landing page for the Boston flights dashboard. 
        From here, you can access all the other tabs, including the Flight Map, Summary Statistics, Code, and About.  
        tabs. Explore flight data and gain insights into flight patterns, delays, and other important details. 
        Use the navigation bar to understand each tab!"
      )
    )
  })
  
  # Message for map data
  observeEvent(input$mapBtn, {
    showModal(
      modalDialog(
        title = "Flight Map",
        "Welcome to the Flight Map button! 
        Click on the 'Flight Maps' tab above to explore the origin and destination airports on an interactive map.
        You can hover over each point to see more information about the flight route and zoom in to explore specific areas of interest."
      )
    )
  })
  
  # Message for the summary statistics data
  observeEvent(input$visioBtn, {
    showModal(
      modalDialog(
        title = "Summary Statistics",
        "Welcome to the Summary Statistics button! Click on the 'Summary Statistics' tab above to explore visualizations of Boston flights in 2018. 
        Gain insights into flight patterns, delays, and more through interactive graphs and tables."
      )
    )
  })
  
  # Message for the table data
  observeEvent(input$tableBtn, {
    showModal(
      modalDialog(
        title = "Table",
        "Welcome to the Table button! Click on the 'Table' tab above to explore the Boston flights dataset in a tabular format. 
        View detailed information on flight schedules, delays, and other important details. Use the search and filter functions to easily find the data you need."
      )
    )
  })
  
  
  #message for the code button 
  observeEvent(input$codeBtn, {
    showModal(
      modalDialog(
        title = "Code",
        "Welcome to the code button! Click on the 'Code' tab above to  explore the R code used to create the Boston flights dashboard. 
        Get an in-depth look at how the data was cleaned and manipulated, as well as how the charts and tables were created. 
        Use this code as a starting point for your own data analysis projects!"
      )
    )
  })
  
  # Message for the about button  
  observeEvent(input$aboutBtn, {
    showModal(
      modalDialog(
        title = "About",
        "Welcome to the About button! Click on the 'About' tab above to learn more about the team behind the Boston flights dashboard. 
        Meet the developers and mentors who worked on this project, as well as explore the sources of the data used to create the dashboard. 
        Gain insights into the inspiration behind this project and the technologies used to bring it to life.",
        easyClose = TRUE
      )
    )
  })
  
  
  # Render table
  output$myTable <- DT::renderDataTable({
    datatable(boston_flight,
              options = list(pageLength = 10,dom = '<"top-right"f>rt<"bottom-right"ilp><"clear">'), # Set number of rows per page
              filter = "top", # Add a filter row at the top of the table
              selection = "none", # Disable row selection
              rownames = TRUE, # Hide row names
              class = 'cell-border stripe'
    )
  })
  
  # Given the user's subset, filter boston flights data into data
  dat <- reactive({
    boston_flights %>% filter(dest %in% c(unlist(str_split(input$dest, " "))), dest != "BOS") %>%
      mutate(carrier = recode(carrier,"B6" = "JetBlue" , "9E" = "Endeavor Air",
                              "AA" = "American Airlines" , "DL" = "Delta Airlines", 
                              "UA" = "United Airlines", "YX" = "Republic Airlines" ,
                              "WN" = "Southwest Airlines", "AS" = "Alaska Airlines",
                              "NK" = "Spirit Airlines", "MQ" = "Envoy Air", 
                              "OO" = "Skywest Airlines" , "EV" = "ExpressJet",
                              "VX" = "Virgin America", "YV" = "Mesa Airlines"))
  })
  
  # Produce ggplot bar graph
  output$graph <- renderPlot({
    ggplot(data = dat(), mapping = aes(x = dest, fill = carrier)) +
      geom_bar() +
      scale_fill_brewer(palette = "Set3") + 
      labs(x = "Destination", y = "Count") + 
      theme_bw() + 
      theme(legend.position = "bottom",
            legend.title = element_blank())
  })
  
  # Data wrangling for table
  output$table <- renderDT({
    dat() %>%
      group_by(dest, carrier) %>% 
      summarize(Count = n(), Average_Flight_Time = round(mean(flight_time), 3), 
                Average_Distance = round(mean(distance), 3)) %>%
      rename(Destination = dest, Carrier = carrier) %>%
      mutate(Average_Flight_Time = paste0(Average_Flight_Time, " mins"),
             Average_Distance = paste0(Average_Distance, " miles"))
  })
  
  # Given user's date filtering, adapt previous wrangled datasets into leaflet
  filtered_counts <- reactive({
    left_join(boston_flights %>%
                filter(flightdate >= as.Date(input$flightrange[1]),
                       flightdate <= as.Date(input$flightrange[2])) %>%
                count(dest), airports, by = c("dest" = "IATA"))
  })
  
  # Given user's date filtering, adapt previous wrangled datasets into leaflet
  filtered_airport_trajectories <- reactive({
    boston_flights %>%
      filter(dest != "BOS", flightdate >= as.Date(input$flightrange[1]),
             flightdate <= as.Date(input$flightrange[2])) %>%
      group_by(origin, dest) %>%
      summarize(avg_flight = mean(flight_time), n = n()) %>%
      mutate(id = row_number()) %>%
      pivot_longer(cols = c("dest", "origin")) %>%
      left_join(., airports[,c("IATA", "latitude", "longitude")], by = c("value" = "IATA"))
  })
  
  # Given user's date filtering, adapt previous wrangled datasets into leaflet
  filtered_markers <- reactive({
    left_join(filtered_counts(), filtered_airport_trajectories()[,c(1,2,5)],
              by = c("dest" = "value")) %>%
      mutate(popup_text = case_when(
        dest == "BOS" ~ paste0(name, " (", dest, ")", '<br/>',
                               "Total Outgoing Flights: ", sum(n.x[-c(2:20)])) %>% 
          lapply(htmltools::HTML),
        TRUE ~ paste0(name, " (", dest, ")", '<br/>',
                      "Average Flight Time: ", round(avg_flight, 0),
                      " minutes", '<br/>', "Number of Incoming Flights: ", n.x) %>% 
          lapply(htmltools::HTML)))
  })
  
  # Generate static leaflet that will serve as the basis for the interactive aesthetics
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 5.5)) %>%
      setView(lng = -98.261388, lat = 38.389050, zoom = 3) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addProviderTiles(providers$Stamen.TonerLabels) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       data = airport_counts,
                       label = ~as.character(name),
                       color = "black",
                       stroke = FALSE,
                       radius = 2,
                       fillOpacity = 1)
  })  
  
  # Given user's date filtering, generate leaflet onto static
  observe({
    leafletProxy("map") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       data = filtered_counts(),
                       label = ~as.character(name),
                       color = "black",
                       stroke = FALSE,
                       radius = 2,
                       fillOpacity = 1) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       data = filtered_counts(),
                       radius = ~(n)^(1/3),
                       color = "cadetblue",
                       fillOpacity = 0.3) %>%
      addCircleMarkers(lng = -71.0052, lat = 42.3643,
                       radius = 3255^(1/3),
                       color = "cadetblue",
                       fillOpacity = 0.3) %>%
      addPolylines(data = filtered_airport_trajectories(), lng = ~longitude,
                   lat = ~latitude, group = ~id, color = "darkgray",
                   opacity = 0.1, weight = 1, smoothFactor = 1) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       data = filtered_markers(),
                       popup = ~popup_text,
                       color = "black",
                       stroke = FALSE,
                       radius = 8,
                       fillOpacity = 0)
  })
}

#Creates app
shinyApp(ui = ui, server = server)

