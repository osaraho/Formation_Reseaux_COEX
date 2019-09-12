rm(list=ls())
#---------------------------------------------
### datasets
#---------------------------------------------

load('../data/Kabururu_data.Rdata')
ls()

#---------------------------------------------
#### usefull packages
#---------------------------------------------
# install.packages("blockmodels")
library(blockmodels)
library(igraph)
source('function_for_blockmodels.R')

#---------------------------------------------
#### SBM for binary incidence matrices
#-------------------------------------------

# data
plotMatrix(Mat = kabururu_bin_adjacency,rowFG = 'donneurs', colFG  = 'receveurs') 

# SBM for bernoulli
  sbm.kabururu_bin <- BM_bernoulli("SBM",kabururu_bin_adjacency)
  sbm.kabururu_bin$estimate()

# optimal number of blocks / cluster
Q = which.max(sbm.kabururu_bin$ICL)
Q

#- extract the estimated parameters
paramEstimSBM <- extractParamBM(sbm.kabururu_bin,Q)
paramEstimSBM$pi
paramEstimSBM$alpha
paramEstimSBM$Z

#- rearrange the matrix followwing blocks
plotMatrix(kabururu_bin_adjacency,'donneurs', 'receveurs', fileNameSave = NULL, clustering = list(row=paramEstimSBM$Z))

#- macropscopic view of the network
G <- graph_from_adjacency_matrix(paramEstimSBM$alpha, mode = c("directed"), weighted = TRUE, diag = TRUE)
plot.igraph(G,vertex.size = paramEstimSBM$pi*100,edge.width= (E(G)$weight)*100,vertex.color = 1:Q, layout = layout_nicely)

#-  composition of the clusters
lapply(1:Q,function(q){nodeVar$name1[paramEstimSBM$Z == q]})
names(nodeVar)
lapply(1:Q,function(q){table(nodeVar$dialect[paramEstimSBM$Z == q])})
lapply(1:Q,function(q){table(nodeVar$sex[paramEstimSBM$Z == q])})
sum(is.na(nodeVar$birth))
sum(is.na(nodeVar$clan))


