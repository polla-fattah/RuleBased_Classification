source('Aggrigate.R')
source('Classify.R')
source('Evaluate.R')
source('Bruteforce.R')
source('DEvolution.R')
source('Visualize.R')
source('Validate_Stocks.R')

##### Environment Variables #####
setVariables <- function(){
  TIME_COL <<- 'Date'
  ID_COL <<- 'Symbol'
  TEMPORAL_ATTRIBUTES <<- c("ClosedPercent") # , "HighPercent"
  LOWER <<- c( 0,  4) 
  UPPER <<- c( 2,  6) 
  paramNames <- c('sdClose-VS', 'sdClose-US')
  names(LOWER) <<- paramNames
  names(UPPER) <<- paramNames
}

##### Manipulate ######
##### If needed, please create your own functions here #####

addValPercent <- function(colName, usedCol){
  for(id in ITEM_ID){
    ids <- DATA_FRAME[ID_COL] == id
    DATA_FRAME[ids, colName] <<- round((DATA_FRAME[ids, usedCol] / max(DATA_FRAME[ids, usedCol])) * 100, 0)
    
  }

}
# Difference of consecutive values
valDiffConsec <- function(subdata, usedCol, r=-1, abs=F){
  temp <- subdata[[usedCol]]
  x <- sd(temp[-1] - temp[-length(temp)]) * 10
  
  if(r != -1)	x <- round(x, r)
  if(abs)	x <- abs(x)
  
  return(x)
}

addColumns <- function(){

  addValPercent('ClosedPercent', 'Close')
  addAggrColumn(itemSD, 'ClosedPercentSD', 'ClosedPercent', r = 0)
  addAggrColumn(valDiffConsec, 'ClosedDiffSD', 'ClosedPercent', r=0)
  

}

####### Conditional functions for finding players ########
veryStable <- function(p, curValue){
  return(p$ClosedPercentSD < curValue[1])
}

unstable <- function(p, curValue){
  return(p$ClosedPercentSD > curValue[2] )
}

stable <- function(p, curValue){
	return (T)
}
registerClasses <- function(){
  # registering classes
  registerNewClass('VS', veryStable)	# VS = Very Stable
  registerNewClass('US', unstable)	  # US = UnStable
  registerNewClass('S', stable)# RS = Rough Stable

}
###### Main Function #######
main <- function(loadData=F){
  set.seed(999)
  criteria <- c(centroidDist, completeDist, varSD, varSSE, varQuantile)
  
  setVariables()
  registerClasses()



	if(loadData)
		allData <- read.csv('C:\Users\pqf\Google Drive\PhD\Codes\Data\SP500 StockMarket 1-1-15 to 1-3-15.csv')#SP500 StockMarket 1-1-15 to 1-3-15.csv
  
  testIndex <- which(allData$Date > 20)

  ttr.centroidDist <<- trainTest(testIndex, allData, costFun=centroidDist)
  ttr.completeDist <<- trainTest(testIndex, allData, costFun=completeDist)
  ttr.varSD <<- trainTest(testIndex, allData, costFun=varSD)
  ttr.varSSE <<- trainTest(testIndex, allData, costFun=varSSE)
  ttr.varQuantile <<- trainTest(testIndex, allData, costFun=varQuantile)

  
   mcr.centroidDist <<- multiClassify(2, allData,  costFun=centroidDist)
   mcr.completeDist <<- multiClassify(2, allData, costFun=completeDist)
   mcr.varSD <<- multiClassify(2, allData, costFun=varSD)
    mcr.varSSE <<- multiClassify(allData, costFun=varSSE)
   mcr.varQuantile <<- multiClassify(2, allData, costFun=varQuantile)
  
  #  dcr <<- doubleClassify(allData[which(allData$Date <= 20),] , allData[which(allData$Date > 20),], costFun=varQuantile)
  
  # testIndex <- which(allData$Date > 100)
  
#  cat('start trainTest function...\n')
#   i = 1
#   ttr <<- c()
#   for(cr in criteria){
#     ttr[i] <<- trainTest(testIndex, allData, costFun=cr)
#     i = i + 1
#   }
#   mcr <<- c()
#   i = 1
#   cat('start multiClassify function...\n')
#   for(cr in criteria){
#     mcr[i] <<- multiClassify(5, allData, costFun=cr)
#     i = i + 1
#   }
#  mcr <<- multiClassify(2, allData, costFun=varQuantile)
  
#	bestClassifier <- dd$optim$bestmem
#	print('hi')
#	addClass(classify(bestClassifier))
#	print(table(classify(bestClassifier)))
	
# 	plotTAAC(c('Very Stable', 'Unstable', 'Rough Stable', 'Smooth Stable')
# 				,row = 2, col=2)
# 	
  
	#	bf <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=varSD)
}
main(T)
