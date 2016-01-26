source('Manipulate.R')
source('Classify.R')
source('Evaluate.R')
source('Bruteforce.R')
source('DEvolution.R')
source('Visualize.R')
source('Validate_Stocks.R')

##### Environment Variables #####
setVariables <- function(tp=3){
  TIME_POINT <<-tp
  TIME_COL <<- 'Date'
  ID_COL <<- 'Symbol'
  TEMPORAL_ATTRIBUTES <<- c("ClosedPercent") # , "HighPercent"
# For full data of 125 time points
  if(TIME_POINT==4){
    LOWER <<- c( 1, 10, 5, 19, 12)
    UPPER <<- c( 4, 14, 9, 24, 16)
  }
  else if(TIME_POINT==3){
    LOWER <<- c( 0, 10, 4, 17, 12)
    UPPER <<- c( 2, 14, 5, 24, 16)    
  }
  

  
  paramNames <- c('sdClose-VS', 'sdDiff-VS', 'sdClose-US', 'sdDiff-US', 'sdDiff-RS')
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
  return(p$ClosedPercentSD < curValue[1] && p$ClosedDiffSD < curValue[2])
}

unstable <- function(p, curValue){
  return(p$ClosedPercentSD > curValue[3] || p$ClosedDiffSD > curValue[4])
}

ruggedStable <- function(p, curValue){
	if(p$ClosedDiffSD > curValue[5] )
		return (T)
	
	return (F)
}
smoothStable <- function(p, curValue){
	return (T)
}
registerClasses <- function(){
  # registering classes 
  registerNewClass('VS', veryStable)	# VS = Very Stable
  registerNewClass('US', unstable)	  # US = UnStable
  registerNewClass('RS', ruggedStable)# RS = Rough Stable
  registerNewClass('SS', smoothStable)# SS = Smooth Stable
  
}
###### Main Function #######
main <- function(loadData=F){
  allData <- NULL
  dataSplit <- 0
  set.seed(999)
  criteria <- c(centroidDist, completeDist, varSD, varSSE, varQuantile)
  
  setVariables()
  registerClasses()
  
  
	if(loadData){
	  if(TIME_POINT==4){
	    allData <- read.csv('SP500 StockMarket 1-1-15 to 1-7-15.csv')
	    dataSplit <- 63
	  }
	  else if(TIME_POINT==3){
	    allData <- read.csv('SP500 StockMarket 1-1-15 to 1-3-15.csv')
	    dataSplit <- 20
	  }
	}
  
  testIndex <- which(allData$Date > dataSplit)

  ttr.centroidDist <<- trainTest(testIndex, allData, costFun=centroidDist)
  ttr.completeDist <<- trainTest(testIndex, allData, costFun=completeDist)
  ttr.varSD <<- trainTest(testIndex, allData, costFun=varSD)
  ttr.varSSE <<- trainTest(testIndex, allData, costFun=varSSE)
  ttr.varQuantile <<- trainTest(testIndex, allData, costFun=varQuantile)

  
#   mcr.centroidDist <<- multiClassify(2, allData,  costFun=centroidDist)
#   mcr.completeDist <<- multiClassify(2, allData, costFun=completeDist)
#   mcr.varSD <<- multiClassify(2, allData, costFun=varSD)
#   mcr.varSSE <<- multiClassify(2, allData, costFun=varSSE)
#   mcr.varQuantile <<- multiClassify(2, allData, costFun=varQuantile)
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
#save(ttr.centroidDist, ttr.completeDist, ttr.varSD, ttr.varSSE, 
#     ttr.varQuantile, file = "objects/emptyFirstClass.RData")
#load("objects/emptyFirstClass.RData")


