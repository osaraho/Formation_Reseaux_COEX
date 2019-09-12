#----------------------------------------#
# Analyse descriptive de graphe          #
#----------------------------------------#

library(igraph)
library(sand)
library(igraphdata)

#----------------------------------------------
# 1) Parcourir un graphe : quelques définitions

# Simple ou Multiple :
g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,
4-7, 5-6, 6-7)
plot(g)
is.simple(g)
mg <- g + edge(2,3)
plot(mg)
is.simple(mg)


# Voisins : 
neighbors(g,5)

# Chemin (path) :
shortest_paths(g, 3,7)

# Distance :
mean_distance(g) 

# Diamètre :
diameter(g) 

# Composante connexe :
comps <- decompose.graph(g)
table(sapply(comps, vcount))

# Connexe
is.connected(g)
#----------------------------------------------
# 2) Caractéristiques des noeuds : mesures de centralité

## 2.1) Degré d'un noeud
data(karate)

hist(degree(karate), col="lightblue", xlim=c(0,50),
xlab="Vertex Degree", ylab="Frequency", main="")

hist(graph.strength(karate), col="pink",
xlab="Vertex Strength", ylab="Frequency", main="")

data(yeast)
ecount(yeast)
vcount(yeast)

d.yeast <- degree(yeast)
hist(d.yeast,col="blue",
xlab="Degree", ylab="Frequency",
main="Degree Distribution")

dd.yeast <- degree.distribution(yeast)
d <- 1:max(d.yeast)-1
ind <- (dd.yeast != 0)
plot(d[ind], dd.yeast[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

a.nn.deg.yeast <- graph.knn(yeast,V(yeast))$knn
plot(d.yeast, a.nn.deg.yeast, log="xy", 
col="goldenrod", xlab=c("Log Vertex Degree"),
ylab=c("Log Average Neighbor Degree"))

## 2.2) Closeness
hist(closeness(karate), col="lightblue", xlab="Vertex Closeness", ylab="Frequency", main="")

## 2.3) Betweenness
hist(betweenness(karate), col="lightblue", xlab="Vertex Beteenness", ylab="Frequency", main="")

A <- get.adjacency(karate, sparse=FALSE)
library(sna)
degree_radial_plot=sna::gplot.target(A,degree(karate),
                  circ.col="skyblue", usearrows = FALSE,vertex.col=c("blue", rep("red", 32), "yellow"),
                  edge.col="darkgray",  main="Degree", circ.lab = FALSE)
closeness_radial_plot=sna::gplot.target(A,closeness(karate)*100,
                                     circ.col="skyblue", usearrows = FALSE,vertex.col=c("blue", rep("red", 32), "yellow"),
                                     edge.col="darkgray",  main="Closeness", circ.lab = FALSE)
betweenness_radial_plot=sna::gplot.target(A,betweenness(karate),
                                     circ.col="skyblue", usearrows = FALSE,vertex.col=c("blue", rep("red", 32), "yellow"),
                                     edge.col="darkgray",  main="Betweenness", circ.lab = FALSE)

#----------------------------------------------
# 3) Caractéristiques du réseau : 
# que dire de la "densité" du réseau ?
## 3.1) Cliques et densité

# Clique :
table(sapply(cliques(karate), length))

plot(karate, layout=l)

cliques(karate, min = 5)

ego.instr <- induced.subgraph(karate, neighborhood(karate, 1, 1)[[1]])
plot(ego.instr)
ego.admin <- induced.subgraph(karate, neighborhood(karate, 1, 34)[[1]])

table(sapply(maximal.cliques(karate), length))

clique.number(karate)
clique.number(ego.instr)
clique.number(ego.admin)

# Densité :
graph.density(karate)
graph.density(ego.instr)
graph.density(ego.admin)

#----------------------------------------------
## 3.2) Motifs : coefficient de clustering, transitivité et fonction dans le réseau 
# Diades 
data(aidsblog)
l <- layout.kamada.kawai(aidsblog)
plot(aidsblog, layout=l, main="", vertex.label="",
     vertex.size=10 * sqrt(hub.score(aidsblog)$vector))

aidsblog <- simplify(aidsblog)
graph.density(aidsblog)
dyad_census(aidsblog)

# Triade : Triplets et triangle
length(triangles(karate))
length(triangles(ego.admin))
count_triangles(karate)
sum(count_triangles(karate))

# Coefficient de clustering - Transitivité
transitivity(karate)
transitivity(karate, "local", vids=c(1,34))

# Motifs
motifs(karate, 3)
count_motifs(karate, 3)
# expliquer les sorties
motifs(karate, 4)
count_motifs(karate, 4)

#----------------------------------------------
## 3.3) Partitionnement : modularité
# Modularité 
kc <- fastgreedy.community(karate)
length(kc)
sizes(kc)
membership(kc)
modularity(kc)
plot(kc,karate)


