#UI

#Install these packages and also install the 'DT' package
library(shiny)
library(ggplot2)
library(readxl)
library(data.table)
library(gdata)
library(dplyr)
library(rmarkdown)


shinyUI(fluidPage(
  titlePanel("2012 Commodity Flow Survey Data Usage Tutorial"),
  
  sidebarLayout(
    sidebarPanel("Panel Placeholder"),
    mainPanel(
      tabsetPanel(
        tabPanel("Getting Started", includeHTML("getting_started.html")),
        tabPanel("Preparing the Data", includeHTML("preparing_data.html")),
        tabPanel("Creating the Narrative", includeHTML("creating_narrative.html")),
        tabPanel("Putting it All Together", includeHTML("putting_together.html"))
      )
    )
  )
)
)
