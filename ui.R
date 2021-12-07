ui <- fluidPage(
  titlePanel("Covid-19 Data Tracker"),
    sidebarLayout(
      sidebarPanel(
        helpText("Please select the data you would like to display:"),
        radioButtons("regionChoice", label = "Would you like to view data by country or continent?",
                     choices = c("country","continent"), 
                     selected = "country"),
        uiOutput("region"),
        selectInput("var", label = "Which data would you like to view?",
                    choices = variable_choices,
                    selected = "cumulative_cases"),
        dateRangeInput("dateRange", label = "Enter date range of interest:",
                       start =  min(covid_loc$date), end = max(covid_loc$date),
                       min = min(covid_loc$date), max =  max(covid_loc$date))
      ),
    mainPanel(
      fluidPage(
        fluidRow(
          plotOutput("allChart", width = "100%",height = "300px")
          ),
        fluidRow(
          column(6,plotOutput("casesChart", width = "100%", height = "250px")),
          column(6,plotOutput("deathsChart", width = "100%", height = "250px"))
                 ),
        fluidRow(
          column(6,plotOutput("casesPerChart", width = "100%", height = "250px")),
          column(6,plotOutput("deathsPerChart", width = "100%", height = "250px"))
        )
          
        ) #fluid page in main panel
  
        ) #main panel
      ) #sidebar layout
    ) #main fluid page
                   
