#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(caret)
library(rpart)

# Define server logic
shinyServer(function(input, output) {

  ## For the data table on third tab
  output$data <- renderTable({
    mtcars[, c("mpg", input$checkboxChoice), drop = FALSE]
  }, rownames = TRUE)
  
  ## Tab two multiple plots on selected variable individually
  output$multiPlots <- renderUI({
    plotOutputList <- 
      lapply(input$checkboxChoice, function(i) {
      plotName <- paste("plot", i, sep="")
      plotOutput(plotName)
    })
    do.call(tagList, plotOutputList)
  })
  
  # Call renderPlot for each one using slected check box value
  observe({
    for (i in input$checkboxChoice) {
      local({
        currentVariable <- i
        plotName <- paste("plot", currentVariable, sep="")
        
        output[[plotName]] <- renderPlot({
          
          if(currentVariable == "cyl"){
            qplot(cyl, mpg, fill=factor(cyl), data=mtcars, geom="boxplot") +
              theme_bw() +
              xlab("Number of Cylinders") +
              ylab("Miles per gallon") +
              scale_fill_discrete(name="Cylinders") +
              ggtitle(paste("This plot was plotted with", currentVariable, "option"))
          }
          else if(currentVariable == "disp"){
              qplot(mpg, disp, data=mtcars) +
                geom_smooth(method = "lm", se = FALSE)+
                xlab("Displacement") +
                ylab("Miles per gallon") +
                ggtitle(paste("This plot was plotted with", currentVariable, "option"))
          }
          else if(currentVariable == "gear"){
            qplot(gear, mpg, fill=factor(gear), data=mtcars, geom="boxplot") +
              theme_bw() +
              xlab("Number of Gears") +
              ylab("Miles per gallon") +
              scale_fill_discrete(name="Gears") +
              ggtitle(paste("This plot was plotted with", currentVariable, "option"))
          }
          else if(currentVariable == "wt"){
            qplot(mpg, wt, data=mtcars) + 
              geom_smooth(method = "lm", se = FALSE) + 
              xlab("Weight") + 
              ylab("Miles per gallon") + 
              ggtitle(paste("This plot was plotted with", currentVariable, "option"))
          }
          else if(currentVariable == "carb"){
            qplot(mpg, carb, data=mtcars) + 
              geom_smooth(method = "lm", se = FALSE) + 
              xlab("Number of arburetors") + 
              ylab("Miles per gallon") + 
              ggtitle(paste("This plot was plotted with", currentVariable, "option"))
          }
          else if(currentVariable == "hp"){
            qplot(mpg, hp, data=mtcars) + 
              geom_smooth(method = "lm", se = FALSE) + 
              xlab("Horse power") + 
              ylab("Miles per gallon") + 
              ggtitle(paste("This plot was plotted with", currentVariable, "option"))
          }
          else if(currentVariable == "am"){
            qplot(am, mpg, fill=factor(am), data=mtcars, geom="boxplot") + 
              theme_bw() + xlab("Transmission") + 
              ylab("Miles per gallon") + 
              scale_fill_discrete(name="Type",breaks=c("0", "1"),labels=c("Automatic", "Manual")) + 
              ggtitle(paste("This plot was plotted with", currentVariable, "option"))
          }
         
          })
        
       })
    }
  })

  ## Tab one predict 
  set.seed(89999)
  
  ## Clean data a for predict
  myMtcars <- mtcars
  myMtcars$cyl <- factor(myMtcars$cyl) 
  myMtcars$am <- factor(myMtcars$am)
  myMtcars$carb <- factor(myMtcars$carb) 
  myMtcars$vs <- factor(myMtcars$vs)
  myMtcars$gear <- factor(myMtcars$gear)
  levels(myMtcars$am) <- c('Automatic', 'Manual')
  
  ## Get model summary
  mpgModelSummary <- reactive({
    model <- lm(mpg ~ ., data = myMtcars[, c("mpg", input$checkboxChoice), drop = FALSE])
    summary(model)
  })
  
  ## Get predict 
  mpgModelPred <- reactive({
    mpgModel <- lm(mpg ~ ., data = myMtcars[, c("mpg", input$checkboxChoice), drop = FALSE])
    predict(mpgModel, newdata = myMtcars)
  })
  
  ## Output
  output$modelSummary <- renderPrint({
    mpgModelSummary()$coeff
  })
  
  output$predMPG <- renderPrint({
    mpgModelPred()
  })
})
