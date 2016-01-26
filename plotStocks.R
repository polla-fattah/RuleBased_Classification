filter <- function(a){
  #	if(!(a$SD_Close > 6 || a$SD_Diff > 22))
  #	  return (T)
  
  #	if((a$SD_Close < 4 && a$SD_Diff < 13) || (a$SD_Close > 6 && a$SD_Diff > 10))
  #		return (a$SD_Diff)
  
  #	if (!(a$SD_Diff > 12))
  #		return (T)
  return (F)
}

getNoralizedVal <- function(a){
  result <- list()
  result$name = a$Symbol[1]
  result$Close = round((a$Close / max(a$Close)) * 100, 0)
  
  todayClose = result$Close[-1]
  yestrdayClose = result$Close[-length(result$Close)]
  percentageClose = ((todayClose - yestrdayClose) / yestrdayClose) * 100
  
  result$percentageClose <- percentageClose
  
  result$SD_Close <- round(sd(result$Close) * 10)
  result$SD_Diff <- round(sd(todayClose - yestrdayClose) * 10 )
  result$Date <- a$Date
  return(result)
  
}

plotPart <- function(a){
  a <- getNoralizedVal(a)
  
  if(filter(a))
    return (0)
	
	xlabVal = paste0('SD Close = ', a$SD_Close)
	
 	plot(x=a$Date, y = a$Close, main=a$name, xlab = xlabVal, 
 		  ylab = 'Market Value', col='blue', ylim=c(40, 110))
 	
 	lines(x=a$Date, y = a$Close)
 	abline(lm(a$Close ~ a$Date ))
	
	xlabVal = paste0('SD * 10 = ', a$SD_Diff)
	
 	plot(x=a$Date[-1], y = a$percentageClose, main=paste(a$name, '%'), 
 		  xlab = xlabVal, ylab = 'Market Value', col='brown')
 	lines(x=a$Date[-1], y = a$percentageClose)

}
plotAll <- function(a){
  png(filename=paste0("stocks/", a$Symbol[1], ".png"),width = 800, height = 700)
  par(mfrow= c(3, 2))
  plotPart(a)
  plotPart(a[which(a$Date<20), ])
  plotPart(a[which(a$Date>20), ])
  dev.off()
  
}
stock <- read.csv('C://Users/pqf/Google Drive/PhD/Codes/R/SP500 StockMarket 1-1-15 to 1-3-15.csv')
stockSplit <- split(stock, stock$Symbol)
for (a in stockSplit){
  cat(a$Symbol[1], '\n')
  plotAll(a)
	
}