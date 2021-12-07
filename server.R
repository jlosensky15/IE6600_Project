server <- function(input, output){
  observeEvent(input$regionChoice, {
    output$region <- renderUI({
      if (input$regionChoice == "continent"){
      pickerInput(
        inputId = "continentChoice",
        label = "Choose continent of interest",
        choices =  as.list(unique(covid_loc$continent)))
      }else{
        pickerInput(
          inputId = "countryChoice",
          label = "Choose country of interest",
          choices =  as.list(unique(covid_loc$country)))
      }
        })
  })
  
  # creates chart for all continents
  output$allChart <- renderPlot({
    covid_loc %>% group_by(continent, date) %>% summarize(cases = sum(new_cases_past_week)) %>% 
      ggplot()+
      geom_line(aes(x = date, y = cases/100000, color = continent))+
      scale_y_continuous("Weekly New Cases (100,000 of cases)")
  })
  
  # new weekly cases chart in position (1,1)
  output$casesChart <- renderPlot({
    cases_plot(new_cases_past_week,as.character(input$countryChoice),input$dateRange)+
      scale_y_continuous("Weekly New Cases")
  })
  
  # new weekly deaths chart in position (1,2)
  output$deathsChart <- renderPlot({
    cases_plot(new_deaths_past_week,as.character(input$countryChoice), input$dateRange)+
      scale_y_continuous("Weekly New Deaths")
  })
  
  # new weekly cases per million chart in position (1,2)
  output$casesPerChart <- renderPlot({
    cases_plot(new_cases_per_million_past_week,as.character(input$countryChoice), input$dateRange)+
      scale_y_continuous("Weekly New Cases per Million")
  })
  
  # new weekly deaths per million chart in position (2,2)
  output$deathsPerChart <- renderPlot({
    cases_plot(new_deaths_per_million_past_week,as.character(input$countryChoice),input$dateRange)+
      scale_y_continuous("Weekly New Deaths per Million")
  })
  
}