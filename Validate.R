#TODO Benchmarking the speed of algorithmes 
#TODO 
require(mcclust)
require(pROC)
require(clv)

Rand <- function(clust1, clust2) clv.Rand(std.ext(clust1, clust2))
Jaccard <- function(clust1, clust2) clv.Jaccard(std.ext(clust1, clust2))
FM <- function(clust1, clust2) clv.Folkes.Mallows(std.ext(clust1, clust2))
VI <- function(clust1, clust2) vi.dist(clust1, clust2)


# classification 
AUC <- function(clust1, clust2) multiclass.roc(clust1, clust2)$auc[1]
