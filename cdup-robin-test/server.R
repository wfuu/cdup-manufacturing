library(shiny)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  cfs_sample <- read.csv('sample.csv')
  state_dict <- read.csv('state_code.csv', stringsAsFactors = FALSE)
  
  # Expression that generates a data table. The expression is
  # wrapped in a call to renderTable to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a table
  
  cfs_aggregate <- function(level, quarter = TRUE){
    # level argument can take in 6 variables: origin state, origin MA, origin cfs,
    # destination state, destination MA, destination cfs area.
    data = cfs_sample %>% group_by_(level, ifelse(quarter,'QUARTER',FALSE)) %>% 
      summarise(total_value = sum(SHIPMT_VALUE),
                total_weight = sum(SHIPMT_WGHT)
      )
    data = data %>% rename_(FIPS = level)  %>% left_join(state_dict)
    return(data)
  }
  
  output$map <- renderPlot({
    dataset <- cfs_aggregate(input$level, input$quarter)
  
    dataset_cho <- dataset[,c(5, 3)]
    dataset_cho <- dataset_cho %>% rename(region = State, value = total_value) 
    dataset_cho$region <- tolower(dataset_cho$region)
    map <- state_choropleth(dataset_cho, title = "Total Value Per State")
    map
  })
  
  
  output$bylevel <- renderTable({
    dataset <- cfs_aggregate(input$level, input$quarter)
    dataset
  })
  
})