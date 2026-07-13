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
  
  unite("newID", number.bottles:predator, remove = FALSE) %>%
  arrange(newID, day) %>%
  
  group_by(newID, predator, prey, productivity, network.syn.lap,
           number.bottles, replicate, structure, media, year) %>%
  
  summarise(
    sampling.days = n(),
    total.vol = sum(volume.L, na.rm = TRUE),
    av.nghbr.connect = mean(nghbr.connect, na.rm = TRUE),
    
    prey.oc = mean(prey.oc > 0, na.rm = TRUE),
    pred.oc = mean(pred.oc > 0, na.rm = TRUE),
    
    prey.dens = mean(prey.density, na.rm = TRUE),
    pred.dens = mean(pred.density, na.rm = TRUE),
    
    # === FIXED TIME TO EXTINCTION ===
    prey.time.2.ext = {
      idx <- which(prey.density <= 0)
      if (length(idx) > 0) day[idx[1]] else NA_real_
    },
    
    pred.time.2.ext = {
      idx <- which(pred.density <= 0)
      if (length(idx) > 0) day[idx[1]] else NA_real_
    },
    
    prey.minimia = min(prey.density, na.rm = TRUE),
    pred.minimia = min(pred.density, na.rm = TRUE),
    
    prey.amp = max(prey.density, na.rm = TRUE),
    pred.amp = max(pred.density, na.rm = TRUE),
    
    prey.persistence = sum(prey.density > 0.1, na.rm = TRUE) / (mean(number.bottles, na.rm = TRUE) + n()),
    pred.persistence = sum(pred.density > 0.1, na.rm = TRUE) / (mean(number.bottles, na.rm = TRUE) + n()),
    
    cv.prey = sd(prey.density, na.rm = TRUE) / mean(prey.density, na.rm = TRUE),
    cv.pred = sd(pred.density, na.rm = TRUE) / mean(pred.density, na.rm = TRUE),
    
    reg.prey.ext = if_else(any(prey.density <= 0, na.rm = TRUE), 
                           "Extinctions", "No Extinctions"),
    reg.pred.ext = if_else(any(pred.density <= 0, na.rm = TRUE), 
                           "Extinctions", "No Extinctions"),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    log.number.bottles = log(number.bottles + 1),
    log.network.syn.lap = log(network.syn.lap + 1),
    log.total.vol = log(total.vol + 1),
    pred.prey.prod = paste(predator, prey, productivity, sep = "_")
  ) %>%
  
  distinct(newID, .keep_all = TRUE)

cor(reg.all$log.number.bottles,reg.all$log.total.vol)
#Regional Ext Data to add to Local data
reg.ext<-reg.all%>%
  ungroup()%>%
  dplyr::select(c(newID,reg.prey.ext,reg.pred.ext))

#Local
library(tidyverse)
# library(raster)  # if still needed for cv; otherwise use base or psych::cv

loc.all <- Data %>%
  filter(day > 3 & day < 75) %>%
  filter(number.bottles > 1) %>%
  
  unite("newID", c(number.bottles, predator, prey, replicate, structure, media, year, network.syn.lap, productivity), remove = FALSE) %>%
  unite("newBottleID", c(number.bottles, predator, prey, replicate, structure, media, year, bottle.number), remove = FALSE) %>%
  
  dplyr::group_by(predator, prey, network.syn.lap, number.bottles,
                  structure, replicate, media, year, bottle.number,
                  nghbr.connect, productivity, newID, newBottleID) %>%
  
  dplyr::summarise(
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
    
    prey.oc = mean(prey.oc, na.rm = TRUE),
    pred.oc = mean(pred.oc, na.rm = TRUE),
    
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
      paste(predator,prey,productivity,sep="_"),
    
    pred.attack.pair = paste(predator, prey, sep = "-")
  ) %>%
  
  left_join(reg.ext, by="newID") %>%
  
  distinct(
    newBottleID,
    cv.prey,
    pred.meta.ext,
    bottle.number,
    reg.pred.ext,
    .keep_all=TRUE
  )
#############################################################################################################################
