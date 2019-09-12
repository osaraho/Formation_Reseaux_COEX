## Les packages

library(igraph)
library(igraphdata)


# 1) Agencements de graphes (graph layouts) 

data(karate)
karate


## 1.2) simple
plot(karate, layout=layout.circle)
title("circular layout")

## 1.2) force
plot(karate, layout=layout.fruchterman.reingold)
title("Fruchterman and Reingold layout")

## 1.3) force et distance
plot(karate, layout=layout.kamada.kawai)
title("Kamada and Kawai layout")

## 1.4) distance
l <- layout_with_mds(karate)
plot(karate, layout=l)
title("MDS layout")

# 2) Décorer un graphe selon les attributs sur les noeuds et les arêtes
# - Choix du layout :
l <- layout.kamada.kawai(karate)
plot(karate, layout=l)

# Formes différentes de noeuds :
V(karate)$shape <- "circle"
V(karate)[c("Mr Hi", "John A")]$shape <- "rectangle"
plot(karate, layout=l)

# Couleurs différentes de noeuds selon des attributs :
V(karate)[Faction == 1]$color <- "red"
V(karate)[Faction == 2]$color <- "dodgerblue"
plot(karate, layout=l)

# Taille des noeuds proportionnelle à leur force (somme des poids des arêtes incidentes) :
V(karate)$size <- 4*sqrt(graph.strength(karate))
V(karate)$size2 <- V(karate)$size * .5
plot(karate, layout=l)

# Epaisseur des arêtes selon des attributs :
E(karate)$width <- E(karate)$weight
plot(karate, layout=l)

# Couleur des arêtes selon les attributs :
F1 <- V(karate)[Faction==1]
F2 <- V(karate)[Faction==2]
E(karate)[ F1 %--% F1 ]$color <- "pink"
E(karate)[ F2 %--% F2 ]$color <- "lightblue"
E(karate)[ F1 %--% F2 ]$color <- "yellow"
plot(karate, layout=l)

# Décaler le label pour les noeuds très petit :
V(karate)$label.dist <- 
ifelse(V(karate)$size >= 10, 0, 0.75)
plot(karate, layout=l)

# 3) graphe bipartite

g.bip <- graph.formula(actor1:actor2:actor3,
movie1:movie2, actor1:actor2 - movie1,
actor2:actor3 - movie2)
V(g.bip)$type <- grepl("^movie", V(g.bip)$name)
g.bip
plot(g.bip, layout=-layout.bipartite(g.bip)[,2:1], 
vertex.size=40, vertex.shape=ifelse(V(g.bip)$type, 
"rectangle", "circle"),
vertex.color=ifelse(V(g.bip)$type, "red", "cyan"))



