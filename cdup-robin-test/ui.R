library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("CDUP !"),
  
  # Sidebar with a select input for the level of anlaysis
  sidebarLayout(
    sidebarPanel(
      selectInput("level", "Choose a variable to group by",
                  choices = c("ORIG_STATE", "ORIG_MA", "ORIG_CFS_AREA", 
                              "DEST_STATE", "DEST_MA", "DEST_CFS_AREA")),
      selectInput("quarter", "Show data over 4 quarters or not?",
                  choices = c(TRUE, FALSE), FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("map"),
      tableOutput("bylevel")
    )
  )
))