library(igraph)

#To Do: Calculate lambda for predicted occupancy
#Replace 0's along diagonal and change sign to positive from negative


#Figure 1: Networks
par(mfrow=c(6,3),mar= c(2, 4, 1, 1), mgp=c(2,1,0))
plot(A)
plot(B)
plot(C)
plot(D)
plot(E)
plot(Fa)
plot(G)
plot(H)
plot(I)
plot(dend.7)
plot(latt.7)
plot(str.7)
plot(sean.big)
plot(sean.small)
plot(small)
plot(dend.15)
plot(latt.15)
#plot(fox)
plot(big)


#Example first
ex.graph <- sample_gnp(10,0.4)
plot(ex.graph)

ex.lap <- laplacian_matrix(ex.graph)

A <- as.matrix(ex.lap,"adjecency")

ev <- eigen(A)$values

lambda.n <- max(ev)
lambda.two <- ev[length(ev)-1]

ev.ratio <- lambda.n/lambda.two


###########IGrpah Det Asycnrony of network structures
#install.packages("igraph")
library(igraph)

#7 bottle figs
dend.7 <- graph( edges=c(1,2,1,3, 2,4, 2,5,  3,6, 3,7), n=7, directed=F ) 
plot(dend.7) # A simple plot of the network - we'll talk more about plots later

latt.7 <- graph( edges=c(1,2, 1,7, 1,3, 1,6, 2,3, 2,4, 2,7, 3,4, 3,5, 4,5, 4,6, 5,6, 7,5, 7,6 ), n=7, directed=F ) 
plot(latt.7)

str.7 <- graph( edges=c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7), n=7, directed=F ) 
plot(str.7)

#15 bottle figs
dend.15 <- graph( edges=c(1,2, 1,3, 2,4, 2,5,  3,6, 3,7, 4,8, 4,9, 5,10, 5,11, 6,12, 6,13, 7,14, 7,15), n=7, directed=F ) 
plot(dend.15) # A simple plot of the network - we'll talk more about plots later

latt.15 <- graph( edges=c(1,2, 1,6, 2,7, 2,3, 3,8,3,4, 4,9, 4,5, 5,10, 6,7, 6,11, 7,12, 7,8, 8,13, 8,9, 9,14, 9,10, 10,15, 11,12, 12,13, 13,14, 14,15 ), n=7, directed=F ) 
plot(latt.15)

latt.mat.15 <- laplacian_matrix(latt.15,sparse=TRUE)
#latt.mat.15_1 <- abs(latt.mat.15)
adj.mat.lat.15 <- as.matrix(latt.mat.15,"adjecency")
adj.mat.lat.15_1 <- abs(adj.mat.lat.15)
latt.15.eigen<-eigen(adj.mat.lat.15_1)$values


latt.mat.15 <- laplacian_matrix(latt.15,sparse=TRUE)
latt.mat.7 <- laplacian_matrix(latt.7,sparse=TRUE)
dend.mat.15 <- laplacian_matrix(dend.15,sparse=TRUE)
dend.mat.7 <- laplacian_matrix(dend.7,sparse=TRUE)
str.mat.7 <- laplacian_matrix(str.7,sparse=TRUE)

adj.mat.lat.15 <- as.matrix(latt.mat.15,"adjecency")
adj.mat.lat.7 <- as.matrix(latt.mat.7,"adjecency")
adj.mat.dend.15 <-as.matrix(dend.mat.15,"adjecency")
adj.mat.dend.7 <- as.matrix(dend.mat.7,"adjecency")
adj.mat.str.7 <-as.matrix(str.mat.7,"adjecency")


latt.15.eigen<-eigen(adj.mat.lat.15)$values
latt.7.eigen<-eigen(adj.mat.lat.7)$values
dend.15.eigen<-eigen(adj.mat.dend.15)$values
dend.7.eigen<-eigen(adj.mat.dend.7)$values
str.7.eigen<-eigen(adj.mat.str.7)$values

syn.latt.15<-max(latt.15.eigen)/latt.15.eigen[length(latt.15.eigen)-1]
syn.latt.7<-max(latt.7.eigen)/latt.7.eigen[length(latt.7.eigen)-1]
syn.dend.15<-max(dend.15.eigen)/dend.15.eigen[length(dend.15.eigen)-1]
syn.dend.7<-max(dend.7.eigen)/dend.7.eigen[length(dend.7.eigen)-1]
syn.str.7<-max(str.7.eigen)/str.7.eigen[length(str.7.eigen)-1]


synch.datas<-data.frame(syn.latt.15,syn.latt.7,syn.dend.15,syn.dend.7,syn.str.7)

############################################################################################################
#Holyoak structures (Habitat Attributes and Persistence AmNat 2000)


A <- graph( edges=c(1,2), n=2, directed=F ) 
plot(A) 

B <- graph( edges=c(1,2, 2,3, 3,1), n=3, directed=F ) 
plot(B) #wrong

C <- graph( edges=c(1,2, 2,3), n=3, directed=F ) 
plot(C)

D <- graph( edges=c(1,2, 2,3, 3,1 ), n=3, directed=F ) 
plot(D) 

E <- graph( edges=c(1,2, 2,3, 3,1, 2,1, 3,2, 3,1), n=3, directed=F ) 
plot(E) #wrong

Fa <- graph( edges=c(1,2, 2,3, 3,4, 4,1, 1,3, 2,4), n=4, directed=F ) 
plot(Fa) #wroong

G <- graph( edges=c(1,2, 2,3, 3,4), n=4, directed=F ) 
plot(G) 

H <- graph( edges=c(1,2, 2,3, 3,4, 4,1), n=4, directed=F ) 
plot(H) 

I <- graph( edges=c(1,2, 2,3, 3,4, 4,1,  1,3, 4,2), n=4, directed=F ) 
plot(I) #wrinog




A.mat <- laplacian_matrix(A,sparse=TRUE)
B.mat <- laplacian_matrix(B,sparse=TRUE)
C.mat <- laplacian_matrix(C,sparse=TRUE)
D.mat <- laplacian_matrix(D,sparse=TRUE)
E.mat <- laplacian_matrix(E,sparse=TRUE)
Fa.mat <- laplacian_matrix(Fa,sparse=TRUE)
G.mat <- laplacian_matrix(G,sparse=TRUE)
H.mat <- laplacian_matrix(H,sparse=TRUE)
I.mat <- laplacian_matrix(I,sparse=TRUE)

adj.mat.A <- as.matrix(A.mat,"adjecency")
adj.mat.B <- as.matrix(B.mat,"adjecency")
adj.mat.C <-as.matrix(C.mat,"adjecency")
adj.mat.D <- as.matrix(D.mat,"adjecency")
adj.mat.E <-as.matrix(E.mat,"adjecency")
adj.mat.Fa <-as.matrix(Fa.mat,"adjecency")
adj.mat.G <-as.matrix(G.mat,"adjecency")
adj.mat.H <-as.matrix(H.mat,"adjecency")
adj.mat.I <-as.matrix(I.mat,"adjecency")


A.eigen<-eigen(adj.mat.A)$values
B.eigen<-eigen(adj.mat.B)$values
C.eigen<-eigen(adj.mat.C)$values
D.eigen<-eigen(adj.mat.D)$values
E.eigen<-eigen(adj.mat.E)$values
Fa.eigen<-eigen(adj.mat.Fa)$values
G.eigen<-eigen(adj.mat.G)$values
H.eigen<-eigen(adj.mat.H)$values
I.eigen<-eigen(adj.mat.I)$values

syn.A<-max(A.eigen)/A.eigen[length(A.eigen)-1]
syn.B<-max(B.eigen)/B.eigen[length(B.eigen)-1]
syn.C<-max(C.eigen)/C.eigen[length(C.eigen)-1]
syn.D<-max(D.eigen)/D.eigen[length(D.eigen)-1]
syn.E<-max(E.eigen)/E.eigen[length(E.eigen)-1]
syn.Fa<-max(Fa.eigen)/Fa.eigen[length(Fa.eigen)-1]
syn.G<-max(G.eigen)/G.eigen[length(G.eigen)-1]
syn.H<-max(H.eigen)/H.eigen[length(H.eigen)-1]
syn.I<-max(I.eigen)/I.eigen[length(I.eigen)-1]


synch.datas<-data.frame(syn.A,syn.B,syn.C,syn.D,syn.E,syn.Fa,syn.G,syn.H,syn.I)
##################################################################################################################################################################
#Sean structure

sean.big <- graph( edges=c(1,2, 1,3,1,4, 2,3, 2,4, 3,5,4,6, 6,8,6,7,5,7,5,8,7,8), n=8, directed=F ) 
plot(sean.big) # A simple plot of the network - we'll talk more about plots later

sean.small <- graph( edges=c(1,3,1,2, 3,5, 3,7, 2,5, 2,6, 4,7, 4,6, 1,4,8,5,8,7,8,6), n=8, directed=F ) 
plot(sean.small)


latt.mat.big <- laplacian_matrix(sean.big,sparse=TRUE)
latt.mat.small <- laplacian_matrix(sean.small,sparse=TRUE)


adj.mat.lat.big <- as.matrix(latt.mat.big,"adjecency")
adj.mat.lat.small <- as.matrix(latt.mat.small,"adjecency")



latt.big.eigen<-eigen(adj.mat.lat.big)$values
latt.small.eigen<-eigen(adj.mat.lat.small)$values


syn.latt.big<-max(latt.big.eigen)/latt.big.eigen[length(latt.big.eigen)-1]
syn.latt.small<-max(latt.small.eigen)/latt.small.eigen[length(latt.small.eigen)-1]



synch.datas<-data.frame(syn.latt.big,syn.latt.small)





##########Holyoak 1996 structures, need data

big <- graph( edges=c(1,2, 1,6,1,7, 2,7, 2,8, 2,6, 2,3, 3,7,3,8, 3,9, 3,4, 4,8, 4,9, 4,10, 4,5, 5,9, 5,10, 10,9, 10,15, 10,14, 9,8, 9,13, 9,14, 9,15, 8,14, 8,13, 8,12, 8,7, 7,13, 7,12, 7,11, 7,6, 6,11, 6,12, 11,12, 11,16, 11,17, 12,13, 12,17 ,12,16, 12,18, 13,14 ,13,17, 13,18 ,13,19, 14,18, 14,19, 14,15, 14,20, 15,19, 15,20, 16,21, 20,19, 20,25, 20,24, 19,25, 19,24, 19,23, 19,18, 18,24, 18,23, 18,22, 18,17, 17,23, 17,22, 17,21, 17,16,  16,22, 21,22, 22,23, 23,24, 24,25 ), n=25, directed=F ) 
plot(big) # A simple plot of the network - we'll talk more about plots later

small <- graph( edges=c(1,2, 1,4, 1,5, 2,4, 2,5, 2,3,2,6, 3,5, 3,6, 6,5, 6,9, 6,8, 5,4, 5,8, 5,9, 5,7, 4,7, 4,8, 7,8, 8,9), n=9, directed=F ) 
plot(small)


latt.mat.big <- laplacian_matrix(big,sparse=TRUE)
latt.mat.small <- laplacian_matrix(small,sparse=TRUE)


adj.mat.big <- as.matrix(latt.mat.big)
adj.mat.lat.small <- as.matrix(latt.mat.small)



latt.big.eigen<-eigen(adj.mat.big)$values
latt.small.eigen<-eigen(adj.mat.lat.small)$values


syn.latt.big<-max(latt.big.eigen)/latt.big.eigen[length(latt.big.eigen)-1]
syn.latt.small<-max(latt.small.eigen)/latt.small.eigen[length(latt.small.eigen)-1]



synch.datas<-data.frame(syn.latt.big,syn.latt.small)


##########Fox  2018 structures, need data
fox <- graph( edges=c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,10, 10,11, 11,12, 12,13, 13,14, 14,15, 15,1 ), n=15, directed=F ) 
plot(fox) # A simple plot of the network - we'll talk more about plots later

latt.mat.big <- laplacian_matrix(fox,sparse=TRUE)

adj.mat.big <- as.matrix(latt.mat.big)

latt.big.eigen<-eigen(adj.mat.big)$values


syn.latt.big<-max(latt.big.eigen)/latt.big.eigen[length(latt.big.eigen)-1]


