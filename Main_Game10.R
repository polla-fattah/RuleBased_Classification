source('Manipulate.R')
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
setEnv <- function(){
  #Environment Variables
  TIME_COL <<- 'period'
  ID_COL <<- 'idsubj'
  TEMPORAL_ATTRIBUTES <<- c("contribution") # , "belief", "initialDev"
  
  # registering classes 
  registerNewClass('FR', isFreeRider)
  registerNewClass('WC', isWeakContributor)
  registerNewClass('NC', isNormalContributor)
  registerNewClass('SC', isStrongContributor)
  
  
  LOWER <<- c( 1, 1, 2, 2, 4, 2, 6, 5) #FR, WC, NC, SC
  UPPER <<- c( 1, 4, 6, 9, 9, 9, 9, 6) #FR, WC, NC, SC
  paramNames <- c('meanContrib-Fr', 'meanContrib-Wc', 'meanContrib-Nc', 
                  'meanBelief-Fr', 'meanBelief-Wc', 'meanBelief-Nc', 
                  'zeroContrib-Fr', 'zeroContrib-Wc')
  names(LOWER) <<- paramNames
  names(UPPER) <<- paramNames
  
  
  set.seed(999)
  
  aggrigate(read.csv('game10.csv'))
  
}
main <- function(){
  setEnv()
  funcCosts <- c(varSD, completeDist, centroidDist,  varSSE, varQuantile)
  i = 1
  dataStore <<- list()
	for(ffn in funcCosts){
		dd <<- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
							DEoptim.control(itermax=20, trace = F), 
							fnMap=function(x) {round(x,0)}, costFun=ffn) 
		dataStore[[i]] <<- dd$optim$bestmem
		i=i+1
		addClass(classify(dd$optim$bestmem))
		print(table(classify(dd$optim$bestmem)))
		plotTAAC(c('Free Riders', 'Weak Contributors', 'Normal Contributors', 'Strong Contributors')
					,row = 2, col=2)
		break
	}
  
}
timeIt <- function(){
  setEnv()
  ptm <- proc.time()
  dd <<- DEoptim(fn=evaluate, lower=LOWER, upper=UPPER,                                          
                 DEoptim.control(itermax=20, trace = T), 
                 fnMap=function(x) {round(x,0)}, costFun=varSD) 
  cat('required Time for find optimum solution = ')
  print(proc.time() - ptm)
  cat('\n\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
  
  setEnv()
  ptm <- proc.time()
  
  bf <<- findBestRule(fn=evaluate, lower=LOWER, upper=UPPER, costFun=varSD)
  cat('required Time for find best solution = ')
  print(proc.time() - ptm)
  cat('\n\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
   
}
