
################################# CONTROLLER LAYER  USING SHINY (V1.0) #####################################

library(shiny)

source("./LmRegSource.R")
shinyServer(function(input, output) {
  
  
  output$RegressionPlot <- renderPlot({
    fitPredictLm(splittedData = splittedData, p = input$p,afficherTest = input$showTest)
  })
  output$ridgo <- renderPlot({
    if (input$p != 0){
      xPoly = model.matrix(y~poly(x,input$p), data = splittedData$train)
      fitridge = glmnet(xPoly, splittedData$train$y, lambda = c(0,10^(seq(7,-2,length.out = 10))), alpha = 0)
      plot(fitridge, xvar = "lambda")}
  })
  output$lasso <- renderPlot({
    if (input$p != 0){
      xPoly = model.matrix(y~poly(x,input$p), data = splittedData$train)
      fitridge = glmnet(xPoly, splittedData$train$y, lambda = c(0,10^(seq(7,-2,length.out = 10))), alpha = 1)
      plot(fitridge, xvar = "lambda")}
  })
  
  output$Regul <- renderPlot({
    fitPredictLm(splittedData = splittedData, p = input$p,afficherTest = input$showTest)
    if (input$p != 0){
      regPredict(splittedData = splittedData, p = input$p, ridge = as.logical(input$useRidge), 
                 lambdaVal = ifelse(is.na(as.numeric(input$lambda)),0, as.numeric(input$lambda)), afficherTest = input$showTest)
      
    }
  })
  
}
)
