require(ggplot2)
require(igraph)

################################ PLOT Matacency   Matrix
plotMatrix = function(Mat,rowFG,colFG, fileNameSave = NULL, clustering = NULL){

  n1 <- dim(Mat)[1]
  n2 <- dim(Mat)[2]
  u <- range(c(Mat))

  if (!is.null(clustering)){
     l <- length(clustering)
     if (l == 1){
       oRow <- oCol <- order(clustering$row)
       uRow <- cumsum(table(clustering$row)) + 0.5
       uRow <- uRow[-length(uRow)]
       sepRow <- as.data.frame(uRow)
       sepCol <- sepRow
     }
     if (l == 2){
       oRow <- order(clustering$row)
       oCol <- order(clustering$col)
       uRow <- cumsum(table(clustering$row)) + 0.5
       uRow <- uRow[-length(uRow)]
       sepRow <- as.data.frame(uRow)
       uCol <- cumsum(table(clustering$col)) + 0.5
       uCol <- uCol[-length(uCol)]
       sepCol <- as.data.frame(uCol)
     }
     Mat <- Mat[oRow,oCol]
     names(sepCol) = names(sepRow) = 'sep'


     sepRow = n1 - sepRow
  }

  index_row = rep(1:dim(Mat)[1],each = dim(Mat)[2])
  index_col = rep(1:dim(Mat)[2],dim(Mat)[1])


  melted_Mat =  data.frame(n1 - index_row , index_col)
  link = rep(-10,dim(Mat)[2]*dim(Mat)[1])
  for (k in 1:(dim(Mat)[2] * dim(Mat)[1])){ link[k] = Mat[index_row[k],index_col[k]]}
  melted_Mat$link = link
  colnames(melted_Mat) <- c('index_row', 'index_col', 'link')

  g <- ggplot(data = melted_Mat, aes(y=index_row, x=index_col, fill=link)) + geom_tile() + scale_fill_gradient(low="white", high="black", limits=u)
  g <- g + theme_bw() +  scale_x_discrete(drop = FALSE) + scale_y_discrete(drop = FALSE)
  g <- g + theme(
    # Rotate the x-axis lables so they are legible
    axis.text.x = element_text(angle = 270, hjust = 0))
    # Force the plot into a square aspect ratio
    # Hide the legend (optional)
    # legend.position = "none")
  g <- g +  labs(x = colFG, y = rowFG)
  g <- g + theme(aspect.ratio = n1/n2)
  if (!is.null(clustering)){
    g <- g + geom_vline(data = sepCol,mapping=aes(xintercept=sep),col = 'grey')
    g <- g + geom_hline(data = sepRow,mapping=aes(yintercept=sep),col = 'grey')


  }
  if (!is.null(fileNameSave)) { ggsave(fileNameSave, width = 20, height = 20, units = "cm") }else{g}
  return(g)
}



################################ Extract results from BlockModels object

extractParamBM <- function(BMobject,Q){
  model <- BMobject$model_name
  membership_name <-  BMobject$membership_name

  res <- list()
  if (model == 'bernoulli') {
    res$alpha <- BMobject$model_parameters[Q][[1]]$pi
  }

  if (model == 'bernoulli_multiplex') {
    res$alpha <- BMobject$model_parameters[Q][[1]]$pi
  }

  if (model == 'poisson') {
    res$alpha <- log(BMobject$model_parameters[Q][[1]]$lambda)
    res$lambda <- BMobject$model_parameters[Q][[1]]$lambda
  }
  if (model == 'poisson_covariates') {
    res$lambda <- BMobject$model_parameters[Q][[1]]$lambda
    res$alpha <- log(BMobject$model_parameters[Q][[1]]$lambda)
    res$theta <-  BMobject$model_parameters[Q][[1]]$beta
  }

  if (model == 'bernoulli_covariates') { ### a vérifier???
    res$alpha <- BMobject$model_parameters[Q][[1]]$pi
    res$theta <-  BMobject$model_parameters[Q][[1]]$beta
  }



  if ((membership_name == 'SBM') |  (membership_name == 'SBM_sym')) {
    res$tau <-  BMobject$memberships[[Q]]$Z
    res$Z <- apply(res$tau, 1, which.max)
    n <- nrow(BMobject$memberships[[Q]]$Z)
    res$pi <-  colSums(BMobject$memberships[[Q]]$Z)/n
    res$Q <- length(res$pi)
  }



  if (membership_name == 'LBM'){
    res$tauRow <-  BMobject$memberships[[Q]]$Z1
    res$tauCol <-  BMobject$memberships[[Q]]$Z2

    if(is.vector(res$tauRow)){ res$ZRow <- which.max(res$tauRow)}
    else {res$ZRow <- apply(res$tauRow, 1, which.max)}
    res$ZCol <- apply(res$tauCol, 1, which.max)
    nRow <- nrow(BMobject$memberships[[Q]]$Z1)
    nCol <- nrow(BMobject$memberships[[Q]]$Z2)
    res$piRow <-  colSums(BMobject$memberships[[Q]]$Z1)/nRow
    res$piCol <-  colSums(BMobject$memberships[[Q]]$Z2)/nCol
    res$Q <- c(length(res$piRow ),length(res$piCol))
    names(res$Q) <- c('QRow','QCol')
  }


  return(res)
}


################################ Plot the BlockModel Network

plotNetBM = function(BMobject,Q){
  membership_name <-  BMobject$membership_name
  a <- extractParamBM(BMobject,Q)$alpha
  b <- extractParamBM(BMobject,Q)$pi
  c <- extractParamBM(BMobject,Q)$piRow
  d <- extractParamBM(BMobject,Q)$piCol

  if (membership_name == 'SBM') {
    G <- graph_from_adjacency_matrix(a, mode = c("undirected"), weighted = TRUE, diag = TRUE)
    plot.igraph(G,vertex.size = b * 100,edge.width = sqrt(abs(E(G)$weight)),vertex.color = 1:Q, layout = layout_nicely)
    # vertex.label=""
  }

  if (membership_name == 'SBM_sym') {
    G <- graph_from_adjacency_matrix(a, mode = c("directed"), weighted = TRUE, diag = TRUE)
    plot.igraph(G,vertex.size = b * 100,edge.width = sqrt(abs(E(G)$weight)),vertex.color = 1:Q, layout = layout_nicely)
    # vertex.label=""
  }

  if (membership_name == 'LBM') {
    G <- graph_from_incidence_matrix(a, weighted = TRUE)
    plot(G,vertex.size = c(c * 100, d * 100), vertex.shape = c("circle", "square")[V(G)$type + 1],
              edge.width = abs(E(G)$weight * 2),vertex.color = 1:Q, layout = layout.bipartite)
  }
  #return(g)
}

#vertex.size=c*100, vertex.size2=d*100,

