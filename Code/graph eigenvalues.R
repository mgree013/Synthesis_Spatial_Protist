library(igraph)
#########################################################################################################
#Anderson lab grpahs
#Dend 15
gt <- graph.tree(15, 2, "undirected") # Make the dendritic graph. 'undirected' option needs to be included to get the correct adjacency matrix
plot(gt) # Make a picture to check that it is right
gt.matrix <- as_adjacency_matrix(gt) # Get the adjacency matrix from the graph
gt.ev <- eigen(gt.matrix) # Get the eigenvalues of the adjacency matrix
gt.lambda_m <- max(gt.ev$values) # The max is what we want
gt.lambda_m

#dend.7
gt <- graph.tree(7, 2, "undirected") # Make the dendritic graph. 'undirected' option needs to be included to get the correct adjacency matrix
plot(gt) # Make a picture to check that it is right
gt.matrix <- as_adjacency_matrix(gt) # Get the adjacency matrix from the graph
gt.ev <- eigen(gt.matrix) # Get the eigenvalues of the adjacency matrix
gt.lambda_m <- max(gt.ev$values) # The max is what we want
gt.lambda_m

#lattice 15
gf <- make_lattice(c(5, 3), "undirected") # Make the lattice graph
plot(gf) # Picture
gf.matrix <- as_adjacency_matrix(gf) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

#lattice 7
#gf <- make_lattice(c(7, 1),circular=T) # Make the lattice graph
gf <- graph( edges=c(1,2, 1,7, 1,3, 1,6, 2,3, 2,4, 2,7, 3,4, 3,5, 4,5, 4,6, 5,6, 7,5, 7,6 ), n=7, directed=F ) 
plot(gf)
plot(gf) # Picture
gf.matrix <- as_adjacency_matrix(gf) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

#straight 7
str.7 <- graph( edges=c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7), n=7, directed=F ) 
plot(str.7)
gf.matrix <- as_adjacency_matrix(str.7) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

#########################################################################################################

#Sean Graphs
sean.big <- graph( edges=c(1,2, 1,3,1,4, 2,3, 2,4, 3,5,4,6, 6,8,6,7,5,7,5,8,7,8), n=8, directed=F ) 
plot(sean.big) # A simple plot of the network - we'll talk more about plots later
gf.matrix <- as_adjacency_matrix(sean.big) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

latt.big.eigen<-eigen(gf.matrix)$values
syn.latt.big<-max(latt.big.eigen)/latt.big.eigen[length(latt.big.eigen)-1]

sean.small <- graph( edges=c(1,3,1,2, 3,5, 3,7, 2,5, 2,6, 4,7, 4,6, 1,4,8,5,8,7,8,6), n=8, directed=F ) 
plot(sean.small)
gf.matrix <- as_adjacency_matrix(sean.small) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

latt.small.eigen<-eigen(gf.matrix)$values
syn.latt.small<-max(latt.small.eigen)/latt.small.eigen[lengthlatt.small.eigen-1]


#########################################################################################################

#Holyoak structures (Habitat Attributes and Persistence AmNat 2000)

A <- graph( edges=c(1,2), n=2, directed=F ) 
plot(A) 
gf.matrix <- as_adjacency_matrix(A) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

A.eigen<-eigen(gf.matrix)$values
syn.A<-max(A.eigen)/A.eigen[length(A.eigen)-1]


B <- graph( edges=c(1,2, 2,3, 3,1), n=3, directed=F ) 
plot(B) #
gf.matrix <- as_adjacency_matrix(B) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m


B.eigen<-eigen(gf.matrix)$values
syn.B<-max(B.eigen)/B.eigen[length(B.eigen)-1]

C <- graph( edges=c(1,2, 2,3), n=3, directed=F ) 
plot(C)
gf.matrix <- as_adjacency_matrix(C) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

C.eigen<-eigen(gf.matrix)$values
syn.C<-max(C.eigen)/C.eigen[length(C.eigen)-1]



D <- graph( edges=c(1,2, 2,3, 3,1 ), n=3, directed=F ) 
plot(D) 
gf.matrix <- as_adjacency_matrix(D) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

D.eigen<-eigen(gf.matrix)$values
syn.D<-max(D.eigen)/D.eigen[length(D.eigen)-1]



E <- graph( edges=c(1,2, 2,3, 3,1, 2,1, 3,2, 3,1), n=3, directed=F ) 
plot(E) #wrong
gf.matrix <- as_adjacency_matrix(E) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

E.eigen<-eigen(gf.matrix)$values
syn.E<-max(E.eigen)/E.eigen[length(E.eigen)-1]


Fa <- graph( edges=c(1,2, 2,3, 3,4, 4,1, 1,3, 2,4), n=4, directed=F ) 
plot(Fa) #wroong
gf.matrix <- as_adjacency_matrix(Fa) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

Fa.eigen<-eigen(gf.matrix)$values
syn.Fa<-max(Fa.eigen)/Fa.eigen[length(Fa.eigen)-1]


G <- graph( edges=c(1,2, 2,3, 3,4), n=4, directed=F ) 
plot(G) 
gf.matrix <- as_adjacency_matrix(G) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

G.eigen<-eigen(gf.matrix)$values
syn.G<-max(G.eigen)/G.eigen[length(G.eigen)-1]


H <- graph( edges=c(1,2, 2,3, 3,4, 4,1), n=4, directed=F ) 
plot(H) 
gf.matrix <- as_adjacency_matrix(H) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

H.eigen<-eigen(gf.matrix)$values
syn.H<-max(H.eigen)/H.eigen[length(H.eigen)-1]


I <- graph( edges=c(1,2, 2,3, 3,4, 4,1,  1,3, 4,2), n=4, directed=F ) 
plot(I) #
gf.matrix <- as_adjacency_matrix(I) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

I.eigen<-eigen(gf.matrix)$values
syn.I<-max(I.eigen)/I.eigen[length(I.eigen)-1]
#########################################################################################################
#holy 9 and 15

big <- graph( edges=c(1,2, 1,6,1,7, 2,7, 2,8, 2,6, 2,3, 3,7,3,8, 3,9, 3,4, 4,8, 4,9, 4,10, 4,5, 5,9, 5,10, 10,9, 10,15, 10,14, 9,8, 9,13, 9,14, 9,15, 8,14, 8,13, 8,12, 8,7, 7,13, 7,12, 7,11, 7,6, 6,11, 6,12, 11,12, 11,16, 11,17, 12,13, 12,17 ,12,16, 12,18, 13,14 ,13,17, 13,18 ,13,19, 14,18, 14,19, 14,15, 14,20, 15,19, 15,20, 16,21, 20,19, 20,25, 20,24, 19,25, 19,24, 19,23, 19,18, 18,24, 18,23, 18,22, 18,17, 17,23, 17,22, 17,21, 17,16,  16,22, 21,22, 22,23, 23,24, 24,25 ), n=25, directed=F ) 
plot(big) # A simple plot of the network - we'll talk more about plots later
gf.matrix <- as_adjacency_matrix(big) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

small <- graph( edges=c(1,2, 1,4, 1,5, 2,4, 2,5, 2,3,2,6, 3,5, 3,6, 6,5, 6,9, 6,8, 5,4, 5,8, 5,9, 5,7, 4,7, 4,8, 7,8, 8,9), n=9, directed=F ) 
plot(small)
gf.matrix <- as_adjacency_matrix(small) # Get the adjacency matrix from the graph
gf.ev <- eigen(gf.matrix) # Get the eigenvalues of the adjacency matrix 
gf.lambda_m <- max(gf.ev$values) # The max is what we want
gf.lambda_m

