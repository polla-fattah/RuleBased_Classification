library(party)
library(dtw)
library(wavelets)
library(caret)
library(e1071)
library(C50) # for C5.0 


source('Validate.R')

getTransformedData <- function(){
  wtData <- NULL
  ids <- unique(DATA_FRAME$idsubj) # Change this for diffrent data types
  classIdIndx = c()
  i <- 1
  for (id in ids) {
    indx <- which(DATA_FRAME$idsubj == id)[1]
    classIdIndx[i] <- indx

    i <- i + 1
    
    # 10 round PGG actual contribution
    a <- t(t(DATA_FRAME[DATA_FRAME$idsubj == id,]$contribution[-1])) 

    wt <- dwt(a, filter='haar', boundary='periodic')
  
    wtData <- rbind(wtData, unlist(c(wt@W, wt@V[[wt@level]])))
  }
  rownames(wtData) <- NULL
  
  wtData <- as.data.frame(wtData)
  
  #classId <<- as.factor(DATA_FRAME[classIdIndx, ]$idtyp)
  classId <<- as.factor(classify(round((LOWER + UPPER) / 2, 0)))
  
  wtData <- data.frame(cbind(classId, wtData))
  
  
  return(wtData)
}


getData2 <- function(){
  wtData <- NULL
  ids <- unique(DATA_FRAME$idsubj) # Change this for diffrent data types
  classIdIndx = c()
  i <- 1
  for (id in ids) {
    indx <- which(DATA_FRAME$idsubj == id)[1]
    classIdIndx[i] <- indx
    i <- i + 1

    a <- DATA_FRAME[DATA_FRAME$idsubj == id,]$contribution
          
   # print(a)
    

    wtData <- rbind(wtData, a)
  }
  rownames(wtData) <- NULL
  
  wtData <- as.data.frame(wtData)

  #classId <<- as.factor(DATA_FRAME[classIdIndx, ]$idtyp)
  classId <<- as.factor(classify(round((LOWER + UPPER) / 2, 0)))
  
  wtData <- data.frame(cbind(classId, wtData))
  

  return(wtData)
}

#wtSc <- getData2()
wtSc <- getTransformedData()

#print(length(wtSc$classId))


     
# build a decision tree with ctree() in package party
set.seed(9999)
aucs = c()
flds <- createFolds(wtSc$classId, k = 10, list = TRUE, returnTrain = FALSE)

for (i in 1:10){
  trainData <- wtSc[-flds[[i]], ]
  testData <- wtSc[flds[[i]], -1]
  testLables <- wtSc[flds[[i]], 1]
  
  #print(testLables)
  model <- svm(classId ~ ., data=trainData)
  #model <- ctree(classId ~ ., data=trainData)
  #model <- C5.0(classId ~ ., data=trainData)
  
  pClassId <- predict(model, testData)
  
  #print(c(testLables))
  aucs[i] <- AUC(c(testLables), c(pClassId))

}
print(round(mean(aucs), 3))







