#paper title: Synthesizing the Effects of Spatial Network Structure on Predator Prey Dynamics
#Author: Matthew Douglas Green; Kurt E. Anderson
#Date: September 21, 2021

########################################################################################################################
library(ggplot2)
library(tidyverse)
library(raster)
library(corrr)
library(viridis)
library(lme4)
library(cowplot)
library(betareg)
########################################################################################################################
setwd("~/Dropbox/Protist Lab Data/Kurt_Matthew_Shared Data/Dendritic Data/Holistic/Data/All.Matt/")

Data=read.csv("data/upd.datas.all.csv")
summary(Data)
str(Data)

Data<-Data%>%
  mutate(pred.prey.oc=if_else(pred.oc==1 &prey.oc==1,1,0))
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
  dplyr::group_by(structure,day,year,media)%>%
  dplyr::summarize(density.prey =mean(ln.prey), density.pred= mean(ln.pred))%>%
  pivot_longer(cols=density.prey:density.pred,names_to = "species", values_to="density")%>%
  ggplot(mapping=aes(x=day,y=density, colour=species))+ 
  geom_line() +
  labs(x="day",y="ln(density)+1")+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~interaction(year,media,structure))+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())
################################################################################################################################################################################################################
  
#Regional
reg.all<-Data %>%
  #filter(day > 5)%>%
  filter(day > 3 & day < 75)%>%
  #filter(media=="medium")%>%
  #filter(predator=="didinium")%>%
  #filter(number.bottles > 1)%>%
  unite("newID", number.bottles:predator, remove=FALSE)%>%
  group_by(predator,prey,productivity,network.syn.lap,number.bottles,replicate,structure,media,year,day,volume.L,newID) %>%
  summarise(prey.dens=sum(ln.prey),pred.dens=sum(ln.pred),
            total.vol=sum(volume.L),
            av.nghbr.connect=mean(nghbr.connect),
            prey.net.oc=if_else(prey.oc>0,1,0),pred.net.oc=if_else(pred.oc>0,1,0))%>%
  ungroup()%>%
  group_by(predator,prey,productivity,network.syn.lap,number.bottles,replicate,structure,media,year,total.vol,av.nghbr.connect,newID) %>%
  summarise( sampling.days=n(),
             prod=mean(productivity),
             bottle.number=mean(number.bottles),
             #patch.degree= mean(connectivity),
             #patch.deg2=mean(disp.connect),
             #tubelength=mean(tube.length),
             #connect.per=mean(Connectivity.per),
             #total.vol=sum(volume.L),
             meta.size=mean(number.bottles),
             #prey.growth=mean(Prey.growth.rate),
             #prey.size=mean(Prey.size),
             #prey.k.cap=mean(Prey.K.cap),
            # prey.disp=mean(Prey.disp.rate),
             prey.oc=mean(prey.net.oc),pred.oc=mean(pred.net.oc),pred.prey.oc.mean=mean(pred.prey.oc),
             prey.dens=mean(prey.dens)/number.bottles,pred.dens=mean(pred.dens)/number.bottles,
            # prey.den=sum(ln.prey),pred.den=sum(ln.pred), 
             prey.quasi.ext.ten=if_else(prey.dens<=.1*(mean(prey.dens)), "yes", "no"),pred.quasi.ext.ten=if_else(pred.dens<=.1*(mean(pred.dens)), "yes", "no"),
             prey.quasi.ext.five=if_else(prey.dens<=.05*(mean(prey.dens)), "yes", "no"),pred.quasi.ext.five=if_else(pred.dens<=.05*(mean(pred.dens)), "yes", "no"),
             prey.quasi.ext.one=if_else(prey.dens<=.01*(mean(prey.dens)), "yes", "no"),pred.quasi.ext.one=if_else(pred.dens<=.01*(mean(pred.dens)), "yes", "no"),
             reg.prey.ext=if_else(prey.dens<=0, "Extinctions", "No Extinctions"),reg.pred.ext=if_else(pred.dens<=0, "Extinctions", "No Extinctions"),
             prey.minimia=min(prey.dens),pred.minimia=min(pred.dens),                                #Minima density
             day.prey.min=day[which.min(prey.dens)],day.pred.min=day[which.min(pred.dens)],          #Day of Minimia
             prey.amp=max(prey.dens),pred.amp=max(pred.dens),                                        #Amp density
             day.prey.max=day[which.max(prey.dens)],day.pred.max=day[which.max(pred.dens)],          #Day of Amp
             prey.persistence=sum(prey.dens>1)/(meta.size+sampling.days),pred.persistence=sum(pred.dens>1)/(meta.size+sampling.days),                    #Number of days Persistence
             Sum.Zero.Prey.Densities.Locally=sum(prey.dens<1),Sum.Zero.Predator.Densities.Locally=sum(pred.dens<1),# Number of days  Zero
             prey.time.2.ext=first(day[prey.dens<=0]),pred.time.2.ext=first(day[pred.dens<=0]),
             cv.prey=raster::cv(prey.dens,na.rm=T), cv.pred=raster::cv(pred.dens,na.rm=T),
             high.oc.prey=sum(prey.oc>0.75)/(meta.size+sampling.days)) %>%
  mutate(log.number.bottles=log(number.bottles+1),log.network.syn.lap=log(network.syn.lap+1),log.total.vol=log(total.vol+1))%>%
  dplyr::distinct(newID,cv.prey,reg.pred.ext,prey.persistence,.keep_all = TRUE)

#Regional Ext Data to add to Local data
reg.ext<-reg.all%>%
  ungroup()%>%
  dplyr::select(c(newID,reg.prey.ext,reg.pred.ext))

#Local
loc.all<-Data %>%
  #filter(day > 4)%>%
  filter(day > 3 & day < 75)%>%
  #filter(number.bottles > 1)%>%
  unite("newID", number.bottles:predator, remove=FALSE)%>%
  unite("newBottleID", number.bottles:predator,bottle, remove=FALSE)%>%
  group_by(predator,prey,network.syn.lap,number.bottles, structure,replicate,media,year,bottle.number,nghbr.connect,productivity,newID,newBottleID)%>%
  dplyr::summarize(sampling.days=n(),
                   prod=mean(productivity),
                   patch.degree=mean(connectivity),
                   patch.deg2=mean(disp.connect),
                   tubelength=mean(tube.length),
                   connect.per=mean(Connectivity.per),
                   total.vol=sum(volume.L),
                   nghbr.connect=mean(nghbr.connect),
                   prey.growth=mean(Prey.growth.rate),
                   prey.size=mean(Prey.size),
                   prey.k.cap=mean(Prey.K.cap),
                   prey.disp=mean(Prey.disp.rate),
                   pred.size=mean(Pred.size),
                   pred.attack=mean(Pred.attack.rate),
                   meta.size=mean(number.bottles),
                   prey.oc=mean(prey.oc),pred.oc=mean(pred.oc),pred.prey.oc.mean=mean(pred.prey.oc),
                   prey.minimia=min(prey.density),pred.minimia=min(pred.density),                                #Minima density
                   day.prey.min=day[which.min(prey.density)],day.pred.min=day[which.min(pred.density)],          #Day of Minimia
                   prey.amp=max(prey.density),pred.amp=max(pred.density),                                        #Amp density
                   day.prey.max=day[which.max(prey.density)],day.pred.max=day[which.max(pred.density)],          #Day of Amp
                   prey.den=mean(prey.density),pred.den=mean(pred.density),                                                #Mean Density
                   prey.quasi.ext.ten=if_else(prey.density<=.1*(mean(prey.density)), "yes", "no"),pred.quasi.ext.ten=if_else(pred.density<=.1*(mean(pred.density)), "yes", "no"),
                   prey.quasi.ext.five=if_else(prey.density<=.05*(mean(prey.density)), "yes", "no"),pred.quasi.ext.five=if_else(pred.density<=.05*(mean(pred.density)), "yes", "no"),
                   prey.quasi.ext.one=if_else(prey.density<=.01*(mean(prey.density)), "yes", "no"),pred.quasi.ext.one=if_else(pred.density<=.01*(mean(pred.density)), "yes", "no"),
                   prey.ext=if_else(prey.density<=0, "Extinctions", "No Extinctions"),pred.ext=if_else(pred.density<=0, "Extinctions", "No Extinctions"),       
                   prey.ext.quant=if_else(prey.density<=0, 1, 0),pred.ext.quant=if_else(pred.density<=0, 1,0),
                   prey.meta.ext=sum(prey.ext.quant),pred.meta.ext=sum(pred.ext.quant),
                   prey.persistence=sum(ln.prey>1)/(meta.size+sampling.days),pred.persistence=sum(ln.pred>1)/(meta.size+sampling.days),                    #Number of days Persistence
                   prey.nmbr.ext.days=sum(prey.density<=0)/sampling.days,pred.nmbr.ext.days=sum(pred.density<=0)/sampling.days,                   #Number of days Persistence
                   prey.time.2.ext=first(day[ln.prey<=.05]),pred.time.2.ext=first(day[ln.pred<=.05]),
                   Sum.Zero.Prey.Densities.Locally=sum(ln.prey<1)/sampling.days,Sum.Zero.Predator.Densities.Locally=sum(ln.pred<1)/sampling.days,# Number of days  Zero
                   cv.prey=raster::cv(prey.density,na.rm = T), cv.pred=raster::cv(pred.density,na.rm = T),
                   prey.time.high.oc=sum(prey.density<=.1*(mean(prey.density)))/sampling.days,pred.time.high.oc=sum(pred.density<=.1*(mean(pred.density)))/sampling.days)%>%
  ungroup()%>%
  mutate(log.number.bottles=log(number.bottles+1))%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))%>%
  mutate(log.total.vol=log(total.vol+1))%>%
  left_join(reg.ext, by="newID")%>%
  dplyr::distinct(newBottleID,cv.prey,pred.meta.ext,bottle.number,reg.pred.ext,.keep_all = TRUE)
################################################################################################################################################################################################################################################################
#Days to Extinction

#local plots
loc.all %>%
  filter(nghbr.connect>0)%>%
  ggplot(aes(x =total.vol, y = pred.oc)) +
  geom_point() +
  geom_smooth(method='glm')+xlab("Metacommunity Size")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

loc.all %>%
  gather(day.prey.min,day.pred.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,prey.persistence,pred.persistence, key = "var", value = "value") %>% 
  ggplot(aes(x =(log.number.bottles) , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='glm')+xlab("Metacommunity Size")+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()+ theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

loc.all %>%
  gather(day.prey.min,day.pred.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =(nghbr.connect) , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='glm')+xlab("Connectivity")+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")
loc.all %>%
  gather(day.prey.min,day.pred.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =(log.network.syn.lap) , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='glm')+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

loc.all %>%
  gather(day.prey.min,day.pred.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =as.factor(prod) , y = value, fill=var)) +
  geom_boxplot()+xlab("Productivity (g)")+
  scale_fill_viridis(discrete=T)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

#Regional Plots
reg.all %>%
  gather(day.prey.min,day.pred.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =(log.number.bottles) , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='glm')+
  facet_wrap(~ var, scales = "free") +
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")
reg.all %>%
  gather(day.prey.min,day.pred.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =(av.nghbr.connect) , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='glm')+
  facet_wrap(~ var, scales = "free") +
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")
reg.all %>%
  gather(day.prey.min,day.pred.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =(log.network.syn.lap) , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='glm')+
  facet_wrap(~ var, scales = "free") +
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")
reg.all %>%
  gather(day.prey.min,day.pred.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =(log.total.vol) , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='glm')+
  facet_wrap(~ var, scales = "free") +
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

reg.all %>%
  gather(day.prey.min,day.pred.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =as.factor(prod) , y = value, fill=as.factor(prod) )) +
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+xlab("Productivity (g)")+
  facet_wrap(~ var, scales = "free") +
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

loc.all %>%
  gather(prey.minimia,pred.minimia,day.prey.min,day.pred.min,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den,prey.persistence,pred.persistence,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,cv.prey,cv.pred, key = "var", value = "value") %>% 
  ggplot(aes(x =meta.size , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")
loc.all %>%
  gather(prey.oc,pred.oc, key = "var", value = "value") %>% 
  ggplot(aes(x =network.syn.lap , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")
loc.all %>%
  gather(prey.minimia,pred.minimia,day.prey.min,day.pred.min,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den,prey.persistence,pred.persistence,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,cv.prey,cv.pred, key = "var", value = "value") %>% 
  ggplot(aes(x =connect.per , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

loc.all %>%
  gather(prey.minimia,prey.amp,pred.amp,day.pred.min,day.prey.min,day.prey.max,day.pred.max,cv.pred,cv.prey,prey.den,pred.den,pred.persistence,Sum.Zero.Predator.Densities.Locally,Sum.Zero.Prey.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =(meta.size) , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

loc.all %>%
  gather(prey.minimia,prey.amp,pred.amp,day.pred.min,day.prey.min,day.prey.max,day.pred.max,cv.pred,cv.prey,prey.den,pred.den,pred.persistence,Sum.Zero.Predator.Densities.Locally,Sum.Zero.Prey.Densities.Locally, key = "var", value = "value") %>% 
  ggplot(aes(x =(nghbr.connect) , y = value, colour=var)) +
  geom_point() +
  geom_smooth(method='lm')+
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

loc.all %>%
  ggplot(aes(x =prey.persistence , y = pred.persistence, colour=structure)) +
  geom_point() +
  geom_smooth(method='lm')+
  theme_bw()+
  scale_color_viridis(discrete = TRUE)+
  facet_grid(~structure)

#Grpahs from present(just sig factrtos)

loc.all %>%
  gather(day.prey.min,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den,prey.persistence,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,cv.prey,cv.pred, key = "var", value = "value") %>% 
  ggplot(aes(x =meta.size , y = value)) +
  geom_point() +
  geom_smooth(method='lm')+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

reg.all %>%
  gather(day.prey.min,day.pred.min,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den,prey.persistence,Sum.Zero.Predator.Densities.Locally,cv.prey,key = "var", value = "value") %>% 
  ggplot(aes(x =meta.size , y = value)) +
  geom_point() +
  geom_smooth(method='glm')+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

reg.all %>%
  gather(day.prey.max,key = "var", value = "value") %>% 
  ggplot(aes(x =patch.degree , y = value)) +
  geom_point() +
  geom_smooth(method='glm')+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

reg.all %>%
  gather(prey.persistence,pred.den,prey.den,day.pred.max,pred.amp,prey.amp,key = "var", value = "value") %>% 
  ggplot(aes(x =network.syn.lap , y = value)) +
  geom_point() +
  geom_smooth(method='glm')+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

loc.all %>%
  gather(day.prey.min,day.pred.min,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den,prey.persistence,pred.persistence,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,cv.prey,cv.pred, key = "var", value = "value") %>% 
  ggplot(aes(x =as.factor(patch.degree) , y = value, fill=as.factor(patch.degree))) +
  geom_boxplot()+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

loc.all %>%
  gather(prey.minimia,day.prey.min,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,cv.prey,cv.pred,prey.persistence,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den, key = "var", value = "value") %>% 
  ggplot(aes(x =network.syn.lap , y = value)) +
  geom_point() +
  geom_smooth(method='lm')+
  facet_wrap(~ var, scales = "free") +
  theme_bw()
#############################################################################################################################
