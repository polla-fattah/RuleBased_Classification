library(data.table)

trans <- transpos(DATA_FRAME)
sample1 <- trans[, -1]
#sample1 <- t(sample1)
for(i in seq(1,ncol(sample1)-1, by=2)){
	cat(i, ' ', i + 1, '\n')
	plot.ts(sample1[,i:(1+i)], main = paste(i, i+1))

}

hc <- hclust(dist(sample1), method = "ave")
plot(hc)

