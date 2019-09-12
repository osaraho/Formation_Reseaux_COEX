#-------------------------------------------------------------------#
# Logiciel R et données de réseau avec le Package R igraph  #
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# I) Présentation du logiciel R

## Importation
data <- read.table(file, header = FALSE, sep = "", dec = ".")
data <- read.csv(file, header = TRUE, sep = ",", quote = "\"", dec = ".")

##  Objets et Création de variables
# un vecteur d'attributs quantitatifs
x1 <- c(1,7,8,3,1,8,1,1,2)
x1 = c(1,7,8,3,1,8,1,1,2)

# un vecteur d'attributs qualitatifs 
x2 = c("F","M","F", "F","F", "M", "M","M")

# une matrice
x3=matrix(1:6, nrow=3, ncol=3)


x1
print(x1)
x2
x3

class(x1)
class(x2)
class(x3)


## Opérateurs
# Opérateurs mathématiques 
x1/(10*100)^2

# Opérateurs logiques 
(x1 < 5) & (x1 ==1)
x2=="F"

# Fonctions mathématiques 
sqrt(x1)

# Fonctions statistiques 
mean(x1)
var(x1)
min(x1)
range(x1)
summary(x1)
sort(x1)

# Opérations vectorielles :
x1[9]
x3[1,2]
x3[,2]
x1[-5]

# Sauvegarde
x = runif(20)
y = list(a = 1, b = TRUE, c = "oops")
save(x, y, file = "xy.RData")
rm(x)
rm(y)
load("xy.RData")
x
y
# pour sauvegarder un tableau de donnéees 
x3
write.table(x3,file="matrix.txt",quote=FALSE,row.names=FALSE,col.names=c("A1","A2", "A3"))

### Packages
library(igraph)
library(igraphdata)
library(sand)
library(blockmodels)
library(ggplot2)

#-------------------------------------------------------------------#
# II) Objet de type graphe avec le package R igraph

# deja objet igraph, i.e. liste d'adjacence
g.lazega <- upgrade_graph(lazega)
plot(g.lazega,vertex.label=NA)

### Liste d'arêtes
aretes=get.edgelist(g.lazega)
aretes[1:10,]

g.lazega=graph_from_edgelist(aretes)
g.lazega

### Matrice d'adjacence
adjacence=get.adjacency(g.lazega)
get.adjacency(g.lazega)[1:20,1:20]
g.lazega=graph_from_adjacency_matrix(adjacence)

### Liste d'adjacence 

g.lazega
V(g.lazega)
E(g.lazega)

## Attributs de noeuds et d'arêtes

g.lazega <- upgrade_graph(lazega)
vcount(g.lazega)
list.vertex.attributes(g.lazega)
V(g.lazega)$Age
V(g.lazega)$Gender

ecount(g.lazega)

dg <- graph.formula(1-+2, 1-+3, 2++3)
dg
V(dg)
E(dg)
plot(dg)
V(dg)$name <- c("Adrien", "Claire", "Alex")
V(dg)$name
V(dg)$gender <- c("M","F","M")
V(dg)$gender
V(dg)$color <- "red"
V(dg)$color

is.weighted(dg)
wg <- dg
E(wg)$weight <- runif(ecount(wg))
E(wg)$weight
is.weighted(wg)

## Opérations sur les graphes

g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
g
plot(g)

# Graphe induit
# via les noms de noeuds :
h <- induced.subgraph(g, 1:5)
h
h.lazega <- induced.subgraph(g.lazega, c("V1","V2","V3","V29","V30","V31"))
h.lazega

# via des attributs de noeuds :
h.lazega <- induced.subgraph(g.lazega, V(g.lazega)$Office==1)
h.lazega
V(h.lazega)$Office

# Suppression de noeuds 
h <- g - vertices(c(6,7))
h
plot(h)

h.lazega <- g.lazega - V(g.lazega)[V(g.lazega)$School==3]
h.lazega
V(h.lazega)$School

# Ajout de noeuds et d'arêtes
h <- h + vertices(c(6,7))
h
g <- h + edges(c(4,6),c(4,7),c(5,6),c(6,7))
g

# Union de graphes 
h1 <- h
h1
h2 <- graph.formula(4-6, 4-7, 5-6, 6-7)
h2
g <- graph.union(h1,h2)
g



