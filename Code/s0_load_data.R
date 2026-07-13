#paper title: Synthesizing the Effects of Spatial Network Structure on Predator Prey Dynamics
#Author: Matthew Douglas Green; Kurt E. Anderson
#Date: September 21, 2021

#Data: 
#1) https://datadryad.org/stash/dataset/doi:10.5061/dryad.3j9kd51kx
#2) http://datadryad.org/stash/dataset/doi:10.5061/dryad.sc1pq
#3) http://datadryad.org/stash/dataset/doi:10.5061/dryad.p1n86


########################################################################################################################
#Load Libraries

library(ggplot2)
library(tidyverse)
library(raster)
library(corrr)
library(viridis)
library(cowplot)
library(betareg)
library(DataCombine)
library(performance)
library(ggh4x)
library(spaMM)
library(igraph)

########################################################################################################################
#setwd("~/Dropbox/Users/matthewdouglasgreen/Dropbox/Manuscipts/Protist_Meta_Spatial_structure/Synthesis_Spatial_Portist")
getwd()
Data=read.csv("~/Dropbox/Manuscipts/Protist_Meta_Spatial_structure/Synthesis_Spatial_Portist/Data/upd.datas.all.csv")
summary(Data)
str(Data)

Data<-Data%>%
  mutate(pred.prey.oc=if_else(pred.oc==1 &prey.oc==1,1,0))%>%
  mutate(predator=if_else(predator=="didinium", "Didinium", "Euplotes"))
########################################################################################################################
#Plot Time Series
Data%>%
  filter(day > 3 & day < 100)%>%
  filter(structure=="HL25array")%>%
  dplyr::group_by(structure,day,year,media,replicate)%>%
  summarise(density.prey =mean(ln.prey), density.pred= mean(ln.pred))%>%
  ggplot(mapping=aes(x=day,y=density.prey, colour=replicate))+ 
  geom_line(aes(x=day,y=density.prey, linetype="dotted")) +
  geom_line( aes(x=day,y=density.pred, linetype="dashed"))+ 
  labs(x="day",y="ln(density)+1")+
  facet_wrap(~replicate)


Data%>%
  filter(day > 3 & day < 100)%>%
  #filter(structure!="isolated")%>%
  dplyr::group_by(structure,day,year,media,replicate)%>%
  summarise(density.prey =mean(ln.prey), density.pred= mean(ln.pred))%>%
  ggplot(mapping=aes(x=day,y=density.prey, colour=replicate))+ 
  geom_line(aes(x=day,y=density.prey, linetype="dotted")) +
  geom_line( aes(x=day,y=density.pred, linetype="dashed"))+ 
  labs(x="day",y="ln(density)+1")+
  facet_wrap(~interaction(year,media,structure))

Data%>%
  filter(connectivity>0)%>%
  filter(day > 3 & day < 75)%>%
  dplyr::group_by(structure,day,year,media,replicate)%>%
  dplyr::summarize(prey =mean(prey.oc), predator= mean(pred.oc))%>%
  pivot_longer(cols=prey:predator,names_to = "species", values_to="density")%>%
  ggplot(mapping=aes(x=day,y=density, colour=species))+ 
  geom_line() +
  labs(x="Day",y="Occupancy")+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~interaction(replicate,year,media,structure))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

################################################################################################################################################################################################################
  
#Regional
reg.all <- Data %>%
  filter(day > 3 & day < 75) %>%
  filter(number.bottles > 1) %>%
  
  unite("newID", number.bottles:predator, remove=FALSE) %>%
  
  group_by(predator,prey,productivity,network.syn.lap,
           number.bottles,replicate,structure,
           media,year,day,volume.L,newID) %>%
  
  summarise(
    prey.dens=sum(ln.prey),
    pred.dens=sum(ln.pred),
    
    total.vol=sum(volume.L),
    av.nghbr.connect=mean(nghbr.connect),
    
    prey.net.oc=as.integer(any(prey.oc>0)),
    pred.net.oc=as.integer(any(pred.oc>0)),
    
    .groups="drop"
  ) %>%
  
  group_by(predator,prey,productivity,network.syn.lap,
           number.bottles,replicate,structure,
           media,year,total.vol,av.nghbr.connect,newID) %>%
  
  summarise(
    sampling.days=n(),
    
    prod=mean(productivity),
    bottle.number=mean(number.bottles),
    meta.size=mean(number.bottles),
    
    prey.oc=mean(prey.net.oc),
    pred.oc=mean(pred.net.oc),
    
    prey.dens=mean(prey.dens)/mean(number.bottles),
    pred.dens=mean(pred.dens)/mean(number.bottles),
    
    prey.quasi.ext.ten =
      as.integer(any(prey.dens <= .10*mean(prey.dens))),
    
    pred.quasi.ext.ten =
      as.integer(any(pred.dens <= .10*mean(pred.dens))),
    
    prey.quasi.ext.five =
      as.integer(any(prey.dens <= .05*mean(prey.dens))),
    
    pred.quasi.ext.five =
      as.integer(any(pred.dens <= .05*mean(pred.dens))),
    
    prey.quasi.ext.one =
      as.integer(any(prey.dens <= .01*mean(prey.dens))),
    
    pred.quasi.ext.one =
      as.integer(any(pred.dens <= .01*mean(pred.dens))),
    
    # KEEP LABELS
    reg.prey.ext =
      if_else(any(prey.dens<=0),
              "Extinctions",
              "No Extinctions"),
    
    reg.pred.ext =
      if_else(any(pred.dens<=0),
              "Extinctions",
              "No Extinctions"),
    
    prey.minimia=min(prey.dens),
    pred.minimia=min(pred.dens),
    
    day.prey.min=day[which.min(prey.dens)],
    day.pred.min=day[which.min(pred.dens)],
    
    prey.amp=max(prey.dens),
    pred.amp=max(pred.dens),
    
    day.prey.max=day[which.max(prey.dens)],
    day.pred.max=day[which.max(pred.dens)],
    
    prey.persistence =
      sum(prey.dens>1)/(mean(number.bottles)+n()),
    
    pred.persistence =
      sum(pred.dens>1)/(mean(number.bottles)+n()),
    
    Sum.Zero.Prey.Densities.Locally=sum(prey.dens<1),
    Sum.Zero.Predator.Densities.Locally=sum(pred.dens<1),
    
    prey.time.2.ext=first(day[prey.dens<=0]),
    pred.time.2.ext=first(day[pred.dens<=0]),
    
    cv.prey=raster::cv(prey.dens,na.rm=TRUE),
    cv.pred=raster::cv(pred.dens,na.rm=TRUE),
    
    high.oc.prey=sum(prey.oc>.75)/(mean(number.bottles)+n()),
    
    .groups="drop"
  ) %>%
  
  mutate(
    log.number.bottles=log(number.bottles+1),
    log.network.syn.lap=log(network.syn.lap+1),
    log.total.vol=log(total.vol+1),
    
    pred.prey.prod =
      paste(predator,prey,productivity,sep="_")
  ) %>%
  
  distinct(
    newID,
    cv.prey,
    reg.pred.ext,
    prey.persistence,
    .keep_all=TRUE
  )

cor(reg.all$log.number.bottles,reg.all$log.total.vol)
#Regional Ext Data to add to Local data
reg.ext<-reg.all%>%
  ungroup()%>%
  dplyr::select(c(newID,reg.prey.ext,reg.pred.ext))

#Local
loc.all <- Data %>%
  filter(day > 3 & day < 75) %>%
  filter(number.bottles > 1) %>%
  
  unite("newID", number.bottles:predator, remove = FALSE) %>%
  unite("newBottleID", number.bottles:predator:bottle, remove = FALSE) %>%
  
  group_by(predator, prey, network.syn.lap, number.bottles,
           structure, replicate, media, year, bottle.number,
           nghbr.connect, productivity, newID, newBottleID) %>%
  
  summarise(
    sampling.days = n(),
    
    prod = mean(productivity),
    patch.degree = mean(connectivity),
    patch.deg2 = mean(disp.connect),
    tubelength = mean(tube.length),
    connect.per = mean(Connectivity.per),
    
    total.vol = sum(volume.L),
    nghbr.connect = mean(nghbr.connect),
    
    prey.growth = mean(Prey.growth.rate),
    prey.size = mean(Prey.size),
    prey.k.cap = mean(Prey.K.cap),
    prey.disp = mean(Prey.disp.rate),
    
    pred.size = mean(Pred.size),
    pred.attack = mean(Pred.attack.rate),
    
    meta.size = mean(number.bottles),
    
    prey.oc = mean(prey.oc),
    pred.oc = mean(pred.oc),
    
    prey.minimia = min(prey.density),
    pred.minimia = min(pred.density),
    
    day.prey.min = day[which.min(prey.density)],
    day.pred.min = day[which.min(pred.density)],
    
    prey.amp = max(prey.density),
    pred.amp = max(pred.density),
    
    day.prey.max = day[which.max(prey.density)],
    day.pred.max = day[which.max(pred.density)],
    
    prey.den = mean(prey.density),
    pred.den = mean(pred.density),
    
    # binary quasi-extinction
    prey.quasi.ext.ten =
      as.integer(any(prey.density <= .10 * mean(prey.density))),
    
    pred.quasi.ext.ten =
      as.integer(any(pred.density <= .10 * mean(pred.density))),
    
    prey.quasi.ext.five =
      as.integer(any(prey.density <= .05 * mean(prey.density))),
    
    pred.quasi.ext.five =
      as.integer(any(pred.density <= .05 * mean(pred.density))),
    
    prey.quasi.ext.one =
      as.integer(any(prey.density <= .01 * mean(prey.density))),
    
    pred.quasi.ext.one =
      as.integer(any(pred.density <= .01 * mean(pred.density))),
    
    # KEEP ORIGINAL LABELS FOR PLOTTING
    prey.ext =
      if_else(any(prey.density <= 0),
              "Extinctions",
              "No Extinctions"),
    
    pred.ext =
      if_else(any(pred.density <= 0),
              "Extinctions",
              "No Extinctions"),
    
    prey.ext.quant = sum(prey.density <= 0),
    pred.ext.quant = sum(pred.density <= 0),
    
    prey.meta.ext = sum(prey.density <= 0),
    pred.meta.ext = sum(pred.density <= 0),
    
    prey.persistence =
      sum(ln.prey > 1)/(mean(number.bottles)+n()),
    
    pred.persistence =
      sum(ln.pred > 1)/(mean(number.bottles)+n()),
    
    prey.nmbr.ext.days =
      sum(prey.density <= 0)/n(),
    
    pred.nmbr.ext.days =
      sum(pred.density <= 0)/n(),
    
    prey.time.2.ext =
      first(day[ln.prey <= .05]),
    
    pred.time.2.ext =
      first(day[ln.pred <= .05]),
    
    Sum.Zero.Prey.Densities.Locally =
      sum(ln.prey < 1)/n(),
    
    Sum.Zero.Predator.Densities.Locally =
      sum(ln.pred < 1)/n(),
    
    cv.prey = raster::cv(prey.density,na.rm=TRUE),
    cv.pred = raster::cv(pred.density,na.rm=TRUE),
    
    prey.time.high.oc =
      sum(prey.density <= .10*mean(prey.density))/n(),
    
    pred.time.high.oc =
      sum(pred.density <= .10*mean(pred.density))/n(),
    
    .groups="drop"
  ) %>%
  
  mutate(
    log.number.bottles = log(number.bottles+1),
    log.network.syn.lap = log(network.syn.lap+1),
    log.total.vol = log(total.vol+1),
    
    pred.prey.prod =
      paste(predator,prey,productivity,sep="_")
  ) %>%
  
  left_join(reg.ext,by="newID") %>%
  
  distinct(
    newBottleID,
    cv.prey,
    pred.meta.ext,
    bottle.number,
    reg.pred.ext,
    .keep_all=TRUE
  )

#############################################################################################################################
