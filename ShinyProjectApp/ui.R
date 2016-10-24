#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# This is UI file
#

library(shiny)

# Define UI for application that predict using regresion model
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Regression model - How observations affect MPG "),
  
  # Sidebar with checkbox group
  sidebarLayout(
    sidebarPanel(
      ## Check box group
      checkboxGroupInput("checkboxChoice", "Select variables:",
                         choices = c( 
                           "Displacement (cu.in.)" = "disp",
                           "Gross horsepower" = "hp",
                           "Number of carburetors" = "carb",
                           "Number of cylinders" = "cyl",
                           "Number of forward gears" = "gear",
                           "Transmission" = "am",
                           "Weight (1000 lbs)" = "wt"),
                         selected = c("cyl")
                         )
      
    ),
    
    ## Main panel with three tabs
    mainPanel(
      h4("Predicted MPG on selected observations:"),

      ## Three tabs
      tabsetPanel(type = "tabs", 
                  tabPanel("Predicted MPG from Model", br(), 
                           "Model coefficient:", verbatimTextOutput("modelSummary"), br(),
                           "Model prediction:", verbatimTextOutput("predMPG")), br(), 
                  tabPanel("Plots", br(), uiOutput("multiPlots")), 
                  tabPanel("Data Table", br(), tableOutput("data"))
      )
    )
  )
))
