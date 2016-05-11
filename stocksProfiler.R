filter <- function(a){
  # Rules for very stable stocks
   	if(a$SD_Close < 1200 && a$SD_Diff < 600)
      return (T)

  # Rules for unstable stocks
   	if(a$SD_Close > 1800 && a$SD_Diff > 800)
      return (T)

  # Rougeed stable
  	if (a$SD_Diff > 650)
  		return (F)
  return (T)
}

getNoralizedVal <- function(a){
  result <- list()
  result$name = a$Symbol[1]
  result$Close = a$Close
  
  todayClose = result$Close[-1]
  yestrdayClose = result$Close[-length(result$Close)]
  result$percentageClose = ((todayClose - yestrdayClose) / yestrdayClose) * 100
  
  
  result$SD_Close <- round(sd(result$Close) * 10)
  result$SD_Diff <- round(sd(todayClose - yestrdayClose) * 10 )

  result$Date <- a$Date
  return(result)
}

plotPart <- function(a){
  a <- getNoralizedVal(a)
  
  if(!filter(a)){
		xlabVal = paste0('SD Close = ', a$SD_Close)
		
	 	plot(x=a$Date, y = a$Close, main=a$name, xlab = xlabVal, 
	 		  ylab = 'Market Value', col='blue', ylim=c(0, 1000))
	 	
	 	lines(x=a$Date, y = a$Close)
	 	abline(lm(a$Close ~ a$Date ))
		
		xlabVal = paste0('SD * 10 = ', a$SD_Diff)
		
	 	plot(x=a$Date[-1], y = a$percentageClose, main=paste(a$name, '%'), 
	 		  xlab = xlabVal, ylab = 'Market Value', col='brown')
	 	
	 	lines(x=a$Date[-1], y = a$percentageClose)
	 	return (c(round((lm(a$Close ~ a$Date )$coefficients[2]), 2), a$SD_Close, a$SD_Diff  ))
	 	
  }
  else{
    return (-131313)
  }
 	
}

plotAll <- function(a){
	result <- c()
	png(filename=paste0("stocks/", a$Symbol[1], ".png"),width = 800, height = 700)
	par(mfrow= c(3, 2))
	result <- c(plotPart(a),
					plotPart(a[which(a$Date <= dateSplit), ]),
					plotPart(a[which(a$Date > dateSplit), ]))
	dev.off()
	return(result)
}
probingPlot <- function(b, type='combined'){
  if(type == 'combined' || type == 'all'){
    a <- getNoralizedVal(b)
    if(!filter(a)){
      png(filename=paste0("stocks/", b$Symbol[1], "_ALL.png"),width = 800, height = 220)
      par(mfrow= c(1, 2))
      plotPart(b)
      dev.off()
    }
  }
  if(type == 'combined' || type == 'first' || type == 'parts'){
     a <- getNoralizedVal(b[which(b$Date <= dateSplit), ])
     if(!filter(a)){
       png(filename=paste0("stocks/", b$Symbol[1], "_FRST.png"),width = 800, height = 220)
       par(mfrow= c(1, 2))
       plotPart(b[which(b$Date <= dateSplit), ])

       dev.off()
     }
  }
  
  if(type == 'combined' || type == 'second' || type == 'parts'){
    a <- getNoralizedVal(b[which(b$Date > dateSplit), ])
    if(!filter(a)){
      png(filename=paste0("stocks/", b$Symbol[1], "_SECND.png"),width = 800, height = 220)
      par(mfrow= c(1, 2))
      plotPart(b[which(b$Date > dateSplit), ])
      dev.off()
    }
  }
}

stock <<- read.csv('../Data/SP500 1-2015 to 7-2015 Normilized.csv', as.is=T)
stockSplit <- split(stock, stock$Symbol)
dateSplit <<- 63
f <- data.frame()
for (a in stockSplit){
  #cat(a$Symbol[1], '\n')
	#print(plotAll(a))
  #f <- rbind(f, plotAll(a))
  probingPlot(a, 'first')
}
# colnames(f) <- c('Slope All', 'SD_Close All', 'SD_Diff All', 
# 					  'Slope P1', 'SD_Close P1', 'SD_Diff P1', 
# 					  'Slope P2', 'SD_Close P2', 'SD_Diff P2')