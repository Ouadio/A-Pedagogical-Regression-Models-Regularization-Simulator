


################################# BUSINESS LOGIC   (V1.1) #####################################


#Loading Libraries
library(glmnet)
library(caret)

#Toy Data Generator
genPolyData = function(size = 300, xRange = c(0,15), wNoiseSD = 10,  ...){
  myCoef = list(coef)
  x = runif(size, min =xRange[1], max = xRange[2])
  wNoise = rnorm(size, mean = 0, sd=wNoiseSD)
  coefs = c(...)
  
  y = numeric(length = size)
  for (i in 1:length(coefs))
    y = y + coefs[i]*(x**(i-1))
  y = y + wNoise
  binRandRange = round((max(y)-min(y))*0.1)
  binRand = sample(0,binRandRange, size = size, replace = T)
  y = y + binRand
  result = data.frame(x = x , y = y)
  return(result)
}


trainTestSplit = function(data, randomly = FALSE){
  if(randomly){
    inTrain = sample(c(1:nrow(data)), size = round(0.6*nrow(data)),replace = F)
    sepPoints = NULL
  }
  else{
    quantiles = as.numeric(quantile(data$x, c(0.25, 0.5, 0.75)))
    inTrain = which(((data$x<quantiles[2]) & (data$x>quantiles[1])) | data$x>quantiles[3])
    sepPoints = quantiles
  }
  inTest = c(1:nrow(data))[!(c(1:nrow(data)) %in% inTrain)]
  trainData = data[inTrain,]
  testData = data[inTest,]
  result = list(train = trainData, test = testData, sepPoints = sepPoints)
  return(result)
}

#Linear Model Fitting
fitPredictLm = function(splittedData, p = 1, afficherTest = FALSE){
  
  trainData = splittedData$train
  testData = splittedData$test
  sepPoints = splittedData$sepPoints
  data = rbind(trainData, testData)
  data = data[order(as.numeric(row.names(data))), ]
  plot( data$x,data$y, type = "n", xlab = "x", ylab = "y")
  points(trainData$x, trainData$y, col="brown2", pch=20)
  points(testData$x, testData$y, col="green", pch=20)
  
  if (!is.null(splittedData$sepPoints)){
    sepPoints = splittedData$sepPoints
    abline(v  = c(sepPoints[1], sepPoints[2], sepPoints[3]), lty=2, col="grey2")
  }

  
  if (p!=0){
    lm1 = lm(y ~ poly(x, p), data = trainData)
    trRMSE = round(RMSE(pred = predict(lm1, trainData), obs = trainData$y), digits = 2)
    tsRMSE = round(RMSE(pred = predict(lm1, testData), obs = testData$y), digits = 2)
    
    if(afficherTest){
      ord = order(data$x)
      points(data$x[ord], predict(lm1, newdata = data[ord,], type = "response"), type = "l", lwd = 2)
      title(paste("RMSE : Train = ",trRMSE, "| Test = ", tsRMSE))
    }
    else{
      ord1 = order(trainData$x[trainData$x>sepPoints[3]])
      ord2 = order(trainData$x[trainData$x<sepPoints[2]])
      #points(trainData$x, predict(lm1, newdata = trainData, type = "response"), type = "l", lwd  = 2)
      points(trainData$x[trainData$x>sepPoints[3]][ord1], predict(lm1, newdata = subset(trainData, x>sepPoints[3])[ord1,], type = "response"), type = "l", lwd = 2)
      points(trainData$x[trainData$x<sepPoints[2]][ord2], predict(lm1, newdata = subset(trainData, x<sepPoints[2])[ord2,], type = "response"), type = "l", lwd = 2)
      title(paste("RMSE : Train = ",trRMSE))
    }
    
  }
  
}

#Linear Model Regularization & Selection (Ridge/Lasso)
regPredict = function(splittedData, p,ridge = TRUE, lambdaVal=0, afficherTest = FALSE){
  
  trainData = splittedData$train[order(splittedData$train$x),]
  testData =  splittedData$test[order(splittedData$test$x),]
  sepPoints = splittedData$sepPoints
  data = rbind(trainData, testData)
  data = data[order(data$x),]
  
  plot( data$x,data$y, type = "n", xlab = "x", ylab = "y")
  points(trainData$x, trainData$y, col="brown2", pch=20)
  points(testData$x, testData$y, col="green", pch=20)
  if (!is.null(sepPoints)){
    abline(v  = c(sepPoints[1], sepPoints[2], sepPoints[3]), lty=2, col="grey2")
  }

  
  ord = order(data$x)
  inTrain = as.numeric(rownames(trainData))
  inTest = as.numeric(rownames(testData))
  polyWhole = model.matrix(y~poly(x,p), data = data)
  polyTrain = polyWhole[inTrain,]
  polyTest = polyWhole[inTest,]
  
  lambd = c(0,10^(seq(-2,7,length=10))) 
  if(ridge)
    alpha = 0
  else
    alpha = 1
  
  fitridge = glmnet(polyTrain, trainData$y, lambda = lambd, alpha = alpha, standardize = T)
  
  preds = predict.glmnet(object = fitridge, newx = polyWhole, s=exp(lambdaVal), type = "response")

  predsTr = predict.glmnet(object = fitridge, newx = polyTrain, s=exp(lambdaVal), type = "response")
  trRMSE =round( RMSE(pred = predsTr, obs = trainData$y), digits = 2)
  
  if (afficherTest){
    predsTs = predict.glmnet(object = fitridge, newx = polyTest, s=exp(lambdaVal), type = "response")
    tsRMSE = round(RMSE(pred = predsTs, obs = testData$y), digits = 2)  
    points(data$x, preds, type = "l",lwd = 2)
    title(paste("RMSE : Train = ",trRMSE, "| Test = ", tsRMSE))
    
    
  }
  else{
    
    trIndx1 = data$x>sepPoints[3]
    trPart1 = data$x[trIndx1]
    predTr1 = preds[trIndx1]
    #trPart1 = trPart1[order(trPart1)]
    
    trIndx2 = data$x<sepPoints[2] & data$x>sepPoints[1]
    trPart2 = data$x[trIndx2]
    predTr2 = preds[trIndx2]
    #trPart2 = trPart2[order(trPart2)]
    
    points(trPart1, predTr1, type = 'l', lwd = 2)
    points(trPart2, predTr2, type = 'l', lwd = 2)
    
    title(paste("RMSE : Train = ",trRMSE))
    
  }
    
}


#Pre-settings of Toy Data (will be manually set on GUI in the next version)

myData = genPolyData(size = 400, xRange = c(-5,5), wNoiseSD = 2,  0,0.15, -1, 0.2, -1.6, 5 )
splittedData = trainTestSplit(data = myData, randomly = F)

