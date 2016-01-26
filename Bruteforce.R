
######### bruteforsing #######
findBestRule <- function(fn, lower, upper, ...){
	lower <<- lower
	curValue <<- lower
	UPPER <<- upper
	PARAM_LENGTH <<- length(curValue)
	
	minCost <- Inf
	bestValue <- c()
	
	cat('The number of possibilities = ', omega(), '\n')
	
	loop <- 0
	startTime <- proc.time()
	while(increment()){
		
		co <- fn(curValue, ...)
		
		if(co < minCost){
			minCost = co
			bestValue <- curValue
			cat('\n\n Cerrent best value is ==============================\n')
			print(bestValue)
		}
		
		loop = loop + 1
		if (loop %% 1000 == 0){
			cc <- proc.time() - startTime
			cat('Till Now we are in loop', loop, '  and  ', cc[3] , ' seconds has been elapsed.\n')
			cat(curValue, '\n\n')
		}
	}
	cc <- proc.time() - startTime
	cat('Time required in Minutes : ', cc[3]/60, '\n')
	cat(curValue, '\n\n')
	return (list(min=minCost, best=bestValue))
}

increment <- function(idx=1){
	if (idx > PARAM_LENGTH)
		return(FALSE)

	if(curValue[idx] == UPPER[idx]){
		curValue[idx] <<- LOWER[idx]
		if(idx <= PARAM_LENGTH)
			return(increment(idx+1))
	}
	else{
		curValue[idx] <<- curValue[idx] + 1
	}
	return(TRUE)
}
##### count Number of possibilities ######
omega <- function(){
	r <- UPPER - LOWER
	r <- 1 + r[which(r != 0)]
	prod(r)
}