library(ggplot2)
library(tidyverse)
library(raster)
library(corrr)
library(TSclust)

setwd("~/Dropbox/Protist Lab Data/Kurt_Matthew_Shared Data/Dendritic Data/Holistic/All.Matt")

#Data = read.csv("eup.tetra.18.19.csv")
Data=read.csv("datas.all.csv")
summary(Data)
str(Data)


################################################################
#all
D1<-Data%>%
  filter(predator=="didinium" & prey=="colpidium")%>%
  dplyr::select(bottle,ln.prey,day)
DA<-D1%>%pivot_wider(names_from = bottle, values_from = ln.prey)

DA1 <- diss(DA[,2:121], "DTW")
summary(DA1)
DA2 <- hclust(DA1)
plot(DA2)
################################################################################################################################
#Dendritic
D1<-Data%>%
  filter(structure=="high.asynchrony.high.asynch" & replicate=="A")%>%
  dplyr::select(bottle,ln.prey,day)
DA<-D1%>%pivot_wider(names_from = bottle, values_from = ln.prey)


DA1 <- diss(DA[,2:9], "COR")
summary(DA1)
DA2 <- hclust(DA1)
plot(DA2)

D2<-Data%>%
  filter(structure=="high.asynchrony.high.asynch" & replicate=="B")%>%
  dplyr::select(bottle,ln.prey,day)
DB<-D2%>%pivot_wider(names_from = bottle, values_from = ln.prey)


DB1 <- diss(DB[,2:9], "COR")
summary(DB1)
DB2 <- hclust(DB1)
plot(DB2)

D3<-Data%>%
  filter(structure=="high.asynchrony.high.asynch" & replicate=="C")%>%
  dplyr::select(bottle,ln.prey,day)
DC<-D3%>%pivot_wider(names_from = bottle, values_from = ln.prey)


DC1 <- diss(DC[,2:9], "COR")
summary(DC1)
DC2 <- hclust(DC1)
plot(DC2)

D4<-Data%>%
  filter(structure=="high.asynchrony.high.asynch" & replicate=="D")%>%
  dplyr::select(bottle,ln.prey,day)
DD<-D4%>%pivot_wider(names_from = bottle, values_from = ln.prey)


DD1 <- diss(DD[,2:9], "COR")
summary(DD1)
DD2 <- hclust(DD1)
plot(DD2)


#Lattice
L1<-Data%>%
  filter(structure=="high.asynchrony.low.asy" & replicate=="A")%>%
  dplyr::select(bottle,ln.prey,day)
LA<-L1%>%pivot_wider(names_from = bottle, values_from = ln.prey)


LA1 <- diss(LA[,2:9], "COR")
summary(LA1)
LA2 <- hclust(LA1)
plot(LA2)

L2<-Data%>%
  filter(structure=="high.asynchrony.low.asy" & replicate=="B")%>%
  dplyr::select(bottle,ln.prey,day)
LB<-L2%>%pivot_wider(names_from = bottle, values_from = ln.prey)


LB1 <- diss(LB[,2:9], "COR")
summary(LB1)
LB2 <- hclust(LB1)
plot(LB2)

L3<-Data%>%
  filter(structure=="high.asynchrony.low.asy" & replicate=="C")%>%
  dplyr::select(bottle,ln.prey,day)
LC<-L3%>%pivot_wider(names_from = bottle, values_from = ln.prey)


LC1 <- diss(LC[,2:9], "COR")
summary(LC1)
LC2 <- hclust(LC1)
plot(LC2)

L4<-Data%>%
  filter(structure=="high.asynchrony.low.asy" & replicate=="D")%>%
  dplyr::select(bottle,ln.prey,day)
LD<-L4%>%pivot_wider(names_from = bottle, values_from = ln.prey)


LD1 <- diss(LD[,2:9], "COR")
summary(LD1)
LD2 <- hclust(LD1)
plot(LD2)

#Lattice
L1<-Data%>%
  filter(structure=="low.asynchrony" & replicate=="A")%>%
  dplyr::select(bottle,ln.prey,day)
LA<-as.data.frame(L1%>%pivot_wider(names_from = bottle, values_from = ln.prey))


LA1 <- diss(LA[,2:9], "COR")
summary(LA1)
LA2 <- hclust(LA1)
plot(LA2)

L2<-Data%>%
  filter(structure=="low.asynchrony" & replicate=="B")%>%
  dplyr::select(bottle,ln.prey,day)
LB<-as.data.frame(L2%>%pivot_wider(names_from = bottle, values_from = ln.prey))


LB1 <- diss(LB[,2:9], "COR")
summary(LB1)
LB2 <- hclust(LB1)
plot(LB2)

L3<-Data%>%
  filter(structure=="low.asynchrony" & replicate=="C")%>%
  dplyr::select(bottle,ln.prey,day)
LC<-as.data.frame(L3%>%pivot_wider(names_from = bottle, values_from = ln.prey))


LC1 <- diss(LC[,2:9], "COR")
summary(LC1)
LC2 <- hclust(LC1)
plot(LC2)

L4<-Data%>%
  filter(structure=="low.asynchrony " & replicate=="D")%>%
  dplyr::select(bottle,ln.prey,day)
LD<-as.data.frame(L4%>%pivot_wider(names_from = bottle, values_from = ln.prey))
summary(LD)

LD1 <- diss(LD[,2:9], "COR")
summary(LD1)
LD2 <- hclust(LD1)
plot(LD2)

Ls<-left_join(LA,LB,LC, by="day")
Lss<-left_join(Ls,LD, by="day")

Ds<-left_join(DA,DB,DC, by="day")
Dss<-left_join(Ds,DD, by="day")
alls<-left_join(Dss,Lss,by="day")%>%drop_na()

Ls1 <- diss(alls[,2:49], "COR")
summary(Ls1)
Ls2 <- hclust(Ls1)
plot(Ls2)

#######################