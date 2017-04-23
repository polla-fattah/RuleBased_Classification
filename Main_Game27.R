source('Aggrigate.R')
source('Classify.R')
source('Evaluate.R')
source('Bruteforce.R')
source('DEvolution.R')
source('Visualize.R')

##### Manipulate ######
##### If needed, please create your own functions here #####
addPayoff <- function(contribution='contribution', othersContrib='otherscontrib', colName='payoff', r=0){
	DATA_FRAME[colName] <<- round(20 - DATA_FRAME[,contribution] + 0.4 * (DATA_FRAME[,contribution]+ 3 * DATA_FRAME[,othersContrib]), r)
	
}
addInitialDeviation <- function(contribution='contribution', belief='belief', colName='initialDev'){
	column <- rep(0, nrow(DATA_FRAME))
	for( i in 1:length(column))
		column[i] <-DATA_FRAME[i, paste0('b', DATA_FRAME[i, belief])]
	
	column <- abs(column - DATA_FRAME[contribution])
	DATA_FRAME[colName] <<- column
	
}

addColumns <- function(){
	
	addAggrColumn(itemMean, 'meanContrib', 'contribution', r=0)
	addAggrColumn(itemMean, 'meanBelief', 'belief', r=0)
	
	addColsMean(c('b0', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 'b10', 
					  'b11', 'b12', 'b13', 'b14', 'b15', 'b16', 'b17', 'b18', 'b19', 'b20'), 
					'initialMean', r=0)
	addPayoff()
	addInitialDeviation()
	addAggrColumn(itemMean, 'initialDevMean', 'initialDev', r=0)
	addColDiff('belief', 'otherscontrib', 'predictionAccuracy')
	addAggrColumn(itemSD, 'predictionAccuracySD', 'predictionAccuracy', r=1)
	
	addAggrColumn(itemValCount, 'zeroContrib', 'contribution', r=0)
	
}
####### Conditional functions for finding players ########
isFreeRider <- function(p, curValue){
	if(p$zeroContrib >= curValue[7]) 
		return (T)
	
	if(p$meanBelief >= curValue[4] && p$meanContrib <= curValue[1])
		return (T)
	
	#  if(p$meanContrib <= curValue[1])
	#    return (T)
	
	return (F)
}
isWeakContributor <- function(p, curValue){
	if(p$zeroContrib >= curValue[8]) 
		return (T)
	
	if(p$meanBelief >= curValue[5] && p$meanContrib <= curValue[2])
		return (T)
	
	return (F)
}
isNormalContributor <- function(p, curValue){
	if(p$meanContrib <= curValue[3] ) #&& p$meanBelief >= curValue[6]
		return (T)
	
	return (F)
}
isStrongContributor <- function(p, curValue){
	return (T)
}



###### Main Function #######
setEnv <- function(subset=c()){
  TIME_COL <<- 'period'
  ID_COL <<- 'idsubj'
  TEMPORAL_ATTRIBUTES <<- c("contribution") # , "belief"
  
  # registering classes 
  registerNewClass('FR', isFreeRider)
  registerNewClass('WC', isWeakContributor)
  registerNewClass('NC', isNormalContributor)
  registerNewClass('SC', isStrongContributor)
  
  
  LOWER <<- c( 1, 1, 2, 2, 4, 2, 20, 15) 
  UPPER <<- c( 1, 4, 6, 9, 9, 9, 25, 20) 
  paramNames <- c('meanContrib-Fr', 'meanContrib-Wc', 'meanContrib-Nc', 
                  'meanBelief-Fr', 'meanBelief-Wc', 'meanBelief-Nc', 
                  'zeroContrib-Fr', 'zeroContrib-Wc')
  names(LOWER) <<- paramNames
  names(UPPER) <<- paramNames
  
  set.seed(999) 
  rawData <- read.csv('C://Users/pqf/Google Drive/PhD/Codes/Data/game27.csv')
  
  if(length(subset) == 0){
    aggrigate(rawData)
  }
  else{
    aggrigate(rawData[!(rawData$idsubj %in% subset),])
  }
  

}
main <- function(){
  setEnv()

	dd <<- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
						DEoptim.control(itermax=20, trace = T), 
						fnMap=fnm, costFun=varSSE) 
	
	addClass(classify(dd$optim$bestmem))
	print(table(classify(dd$optim$bestmem)))
	plotTAAC(c('Free Riders', 'Weak Contributors', 'Normal Contributors', 'Strong Contributors')
				,row = 2, col=2)
	
	
	dd1 <<- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
						 DEoptim.control(itermax=20, trace = T), 
						 fnMap=fnm, costFun=centroidDist) 
	
	addClass(classify(dd1$optim$bestmem))
	print(table(classify(dd1$optim$bestmem)))
	plotTAAC(c('Free Riders', 'Weak Contributors', 'Normal Contributors', 'Strong Contributors')
				,row = 2, col=2)	
#	evaluate(UPPER)
#	bf <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=varSD)
}
mainBruteForce <- function(){
  setEnv(); set.seed(999)
  best_varQuantile <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=varQuantile)
  
#  setEnv(); set.seed(999) 
#  best_varSD       <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=varSD)
  
  setEnv(); set.seed(999) 
  best_completeDist <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=completeDist)
  
#  setEnv(); set.seed(999) 
#  best_centroidDist <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=centroidDist)
  
#  setEnv(); set.seed(999) 
#  best_Dunn <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=Dunn)
  
#  setEnv(); set.seed(999) 
#  best_DB   <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=Davies.Bouldin)
  
  setEnv(); set.seed(999) 
  best_SD   <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=SD)
  
#  setEnv(); set.seed(999) 
#  best_SDbw <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=SDbw)
}
#mainBruteForce()
#main()
#table(classify(c(1, 1, 3, 3, 4, 2, 24, 17))) # IQR
#table(classify(c(1, 4, 6, 4, 9, 2, 20, 20))) # SD
#table(classify(c(1, 3, 3, 2, 4, 2, 20, 15))) # Complete

calculateAUC <- function(){
  setEnv()
  subjects <<- AGGRI_DATA$idsubj
  
  funcCosts <- c(varQuantile, completeDist, SD)
  
  set.seed(999)
  flds <- createFolds(AGGRI_DATA$idsubj, k = 10, 
                      list = TRUE, returnTrain = FALSE)
  
  kk <- 1
  for(ffn in funcCosts){
    aucs = c()
    if(kk == 1) refrenceClass <- classify(c(1, 1, 3, 3, 4, 2, 24, 17)) # IQR
    if(kk == 2) refrenceClass <- classify(c(1, 3, 3, 2, 4, 2, 20, 15)) # Complete
    if(kk == 3) refrenceClass <- classify(c(1, 4, 6, 4, 9, 2, 20, 20)) # SD
    
    
    for(i in 1:10){
      indx <- flds[[i]]
      setEnv(subjects[indx])
      dd <<- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
                     DEoptim.control(itermax=5, trace = F), 
                     fnMap=function(x) {round(x,0)}, costFun=ffn)
      setEnv()
      foundClass <- classify(dd$optim$bestmem)
      
      aucs[i] <- AUC(foundClass[indx], refrenceClass[indx])
    }
    print(round(mean(aucs), 3))
    kk <- kk + 1
  }
}

calculateAUC()
