
################################# CONTROLLER   LAYER  USING SHINY (V2.0) #####################################

library(shiny)

source("./LmRegSource.R")
shinyServer(function(input, output) {
  
  
  output$RegressionPlot <- renderPlot({
    fitPredictLm(splittedData = splittedData, p = input$p,afficherTest = input$showTest)
  })
  output$ridgo <- renderPlot({
    if (input$p != 0){
      xPoly = model.matrix(y~poly(x,input$p), data = splittedData$train)
      fitridge = glmnet(xPoly, splittedData$train$y, lambda = c(0,10^(seq(7,-2,length.out = 50))), alpha = 0)
      plot(fitridge, xvar = "lambda")
      abline(v = as.numeric(input$lambda), lty = 2, lwd = 2, col = 'blue3')
      }
  })
  output$lasso <- renderPlot({
    if (input$p != 0){
      xPoly = model.matrix(y~poly(x,input$p), data = splittedData$train)
      fitridge = glmnet(xPoly, splittedData$train$y, lambda = c(0,10^(seq(7,-2,length.out = 50))), alpha = 1)
      plot(fitridge, xvar = "lambda")
      abline(v = as.numeric(input$lambda), lty = 2, lwd = 2, col = 'blue3')
      }
  })
  
  reactiveRegPlot <- eventReactive(input$showReg, { 

      regPredict(splittedData = splittedData, p = input$p, ridge = as.logical(input$useRidge), 
               lambdaVal = as.numeric(input$lambda), afficherTest = input$showTest)
    
    })
  
  output$Regul <- renderPlot({
    reactiveRegPlot()
    #fitPredictLm(splittedData = splittedData, p = input$p,afficherTest = input$showTest)
    
    }
    )
  
}
)
