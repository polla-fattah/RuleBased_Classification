library(e1071)
library(pROC) # for cvFolds

source('Visualize.R')

createData <- function (data) {
  dataOld_idtype <<- data.frame(idtyp=data$idtyp, idsubj=data$idsubj, period=data$period, 
                              belief=data$belief, contribution=data$contribution, 
                              otherscontrib=data$otherscontrib, b0=data$b0, b1=data$b1, 
                              b2=data$b2, b3=data$b3, b4=data$b4, b5=data$b5, b6=data$b6, 
                              b7=data$b7, b8=data$b8, b9=data$b9, b10=data$b10, b11=data$b11, 
                              b12=data$b12, b13=data$b13, b14=data$b14, b15=data$b15, 
                              b16=data$b16, b17=data$b17, b18=data$b18, b19=data$b19, b20=data$b20, 
                              u=data$u, predictedcontribution=data$predictedcontribution)
  
  
  dataNew_idtype <<- data.frame(idtyp=data$idtyp, idsubj=data$idsubj, period=data$period, 
                              belief=data$belief, contribution=data$contribution,
                              payoff=data$payoff, initialMean=data$initialMean, contribMean=data$contribMean, 
                              initialDeviation=data$initialDeviation, meanInitialDeviation=data$meanInitialDeviation, 
                              predictionAccuracy=data$predictionAccuracy, SDpredictionAccuracy=data$SDpredictionAccuracy)
  
  dataCb_idtype <<- data.frame(idtyp=data$idtyp, idsubj=data$idsubj, period=data$period, 
                             belief=data$belief, contribution=data$contribution)
  
  dataSwap <<- data.frame(class=data$class, idsubj=data$idsubj, period=data$period, 
                          belief=data$belief, contribution=data$contribution, 
                          otherscontrib=data$otherscontrib, b0=data$b0, b1=data$b1, 
                          b2=data$b2, b3=data$b3, b4=data$b4, b5=data$b5, b6=data$b6, 
                          b7=data$b7, b8=data$b8, b9=data$b9, b10=data$b10, b11=data$b11, 
                          b12=data$b12, b13=data$b13, b14=data$b14, b15=data$b15, 
                          b16=data$b16, b17=data$b17, b18=data$b18, b19=data$b19, b20=data$b20, 
                          u=data$u, predictedcontribution=data$predictedcontribution,payoff=data$payoff, initialMean=data$initialMean, contribMean=data$contribMean, 
                          initialDeviation=data$initialDeviation, meanInitialDeviation=data$meanInitialDeviation, 
                          predictionAccuracy=data$predictionAccuracy, SDpredictionAccuracy=data$SDpredictionAccuracy)
  
  dataOld_class <<- data.frame(class=data$class, idsubj=data$idsubj, period=data$period, 
                               belief=data$belief, contribution=data$contribution, 
                               otherscontrib=data$otherscontrib, b0=data$b0, b1=data$b1, 
                               b2=data$b2, b3=data$b3, b4=data$b4, b5=data$b5, b6=data$b6, 
                               b7=data$b7, b8=data$b8, b9=data$b9, b10=data$b10, b11=data$b11, 
                               b12=data$b12, b13=data$b13, b14=data$b14, b15=data$b15, 
                               b16=data$b16, b17=data$b17, b18=data$b18, b19=data$b19, b20=data$b20, 
                               u=data$u, predictedcontribution=data$predictedcontribution)
  
  dataNew_class <<- data.frame(class=data$class, idsubj=data$idsubj, period=data$period, 
                               belief=data$belief, contribution=data$contribution,payoff=data$payoff, initialMean=data$initialMean, contribMean=data$contribMean, 
                               initialDeviation=data$initialDeviation, meanInitialDeviation=data$meanInitialDeviation, 
                               predictionAccuracy=data$predictionAccuracy, SDpredictionAccuracy=data$SDpredictionAccuracy)
  
  
  dataCb_class <<- data.frame(class=data$class, idsubj=data$idsubj, period=data$period, 
                              belief=data$belief, contribution=data$contribution)
  
}
mroc <- function (class, pre){
  return (multiclass.roc(class, pre)$auc[1]);
}

classificationTest <- function (customData) {
  set.seed(999)
  all_roc <- 0
  for(i in 1:10){
    periodData <- customData[customData$period==1, -c(2,3)]
    index <- sample(1:140, 35)
    
    testData <- periodData[index,]
    trainData <- periodData[-index,]
    
    x <- trainData[,-1]
    
    y <- as.factor(trainData[,1])
    #  if(i == 1)
    model <- svm(x=x, y=y)
    #plot(model, trainData, belief ~ contribution)
    
    prediction <- predict(model, testData[,-1])
    
    #tab <- table(pred = prediction, true = testData[,1])
    #cat("\n\nperiod = ", i, "\n")
    # print(tab)
    rocValu <- mroc(as.integer(testData[,1]), as.integer(prediction))
    #print(rocValu)
    all_roc <- all_roc + rocValu
  }
  return (round(x=all_roc/10, digits=3))
}

analyze <- function(){
  createData(DATA_FRAME)
  
  cat("Old\t&", classificationTest(dataOld_idtype), 
      "\t&", classificationTest(dataOld_class), "\t\\\\ \n")
  
  cat("New\t&",classificationTest(dataNew_idtype), 
      "\t&", classificationTest(dataNew_class), "\t\\\\ \n")
  
  ind <- grep("lableAccuracy", names(DATA_FRAME))
  cat("Both\t&", classificationTest(DATA_FRAME[,-ind]), 
      "\t&", classificationTest(dataSwap), "\t\\\\ \n")
  
  cat("B \\& C\t&",classificationTest(dataCb_idtype), 
      "\t&", classificationTest(dataCb_class), "\t\\\\ \n")
}

main <- function(){
  print('IQR')
  attachClass(classify(c(1,3,5,2,4,2,6,5))) # IQR
  analyze()
  
  print('Complete')
  attachClass(classify(c(1,3,6,2,4,2,7,5))) # Complete
  analyze()
  
  print('SD')
  attachClass(classify(c(1,4,6,2,4,2,8,6))) # SD
  analyze()
  
}
