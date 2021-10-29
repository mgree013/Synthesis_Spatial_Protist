#paper title: Synthesizing the Effects of Spatial Network Structure on Predator Prey Dynamics
#Author: Matthew Douglas Green; Kurt E. Anderson
#Date: September 21, 2021

########################################################################################################################library(ggplot2)
library(tidyverse)
library(raster)
library(corrr)
library(viridis)
library(lme4)
########################################################################################################################
setwd("~/Dropbox/Protist Lab Data/Kurt_Matthew_Shared Data/Dendritic Data/Holistic/Data/All.Matt/")

Data=read.csv("data/upd.datas.all.csv")
summary(Data)
str(Data)
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
  filter(day > 3 & day < 100)%>%
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
#Local
loc.all<-Data %>%
  filter(day > 3 & day < 75)%>%
  filter(number.bottles > 1)%>%
  group_by(predator,prey,network.syn.lap,number.bottles, structure,replicate,media,year,bottle.number,nghbr.connect)%>%
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
             prey.oc=mean(prey.oc),pred.oc=mean(pred.oc),
             prey.minimia=min(prey.density),pred.minimia=min(pred.density),                                #Minima density
             day.prey.min=day[which.min(prey.density)],day.pred.min=day[which.min(pred.density)],          #Day of Minimia
             prey.amp=max(prey.density),pred.amp=max(pred.density),                                        #Amp density
             day.prey.max=day[which.max(prey.density)],day.pred.max=day[which.max(pred.density)],          #Day of Amp
             prey.den=mean(prey.density),pred.den=mean(pred.density),                                                #Mean Density
             prey.persistence=sum(prey.density>1)/bottle.number,pred.persistence=sum(pred.density>1)/bottle.number,                    #Number of days Persistence
             Sum.Zero.Prey.Densities.Locally=sum(prey.density==0)/bottle.number,Sum.Zero.Predator.Densities.Locally=sum(pred.density==0)/bottle.number,# Number of days  Zero
             cv.prey=raster::cv(prey.density,na.rm = T), cv.pred=raster::cv(pred.density,na.rm = T))%>%
  ungroup()%>%
  dplyr::distinct(predator,prey,bottle.number,media,year,structure,replicate,.keep_all = TRUE)%>%
  mutate(log.number.bottles=log(number.bottles+1))%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))%>%
  mutate(log.total.vol=log(total.vol+1))
# CV 
            # prey.cor= corrr::correlate(prey.den,pred.den, method="pearson"))#, pred.cor=correlate(pred.den, method="pearson"))

#Regional
reg.all<-Data %>%
  #filter(day > 5)%>%
  filter(day > 3 & day < 75)%>%
  #filter(media=="medium")%>%
  #filter(predator=="didinium")%>%
  filter(number.bottles > 1)%>%
  group_by(predator,prey,productivity,network.syn.lap,number.bottles,replicate,structure,media,year,day,volume.L) %>%
  summarise(prey.dens=sum(ln.prey),pred.dens=sum(ln.pred),
            total.vol=sum(volume.L),
            av.nghbr.connect=mean(nghbr.connect),
            prey.net.oc=if_else(prey.oc>0,1,0),pred.net.oc=if_else(pred.oc>0,1,0))%>%
  ungroup()%>%
  group_by(predator,prey,productivity,network.syn.lap,number.bottles,replicate,structure,media,year,total.vol,av.nghbr.connect) %>%
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
             prey.oc=mean(prey.net.oc),pred.oc=mean(pred.net.oc),
            # prey.den=sum(ln.prey),pred.den=sum(ln.pred), 
             prey.minimia=min(prey.dens),pred.minimia=min(pred.dens),                                #Minima density
             day.prey.min=day[which.min(prey.dens)],day.pred.min=day[which.min(pred.dens)],          #Day of Minimia
             prey.amp=max(prey.dens),pred.amp=max(pred.dens),                                        #Amp density
             day.prey.max=day[which.max(prey.dens)],day.pred.max=day[which.max(pred.dens)],          #Day of Amp
             prey.persistence=sum(prey.dens>1),pred.persistence=sum(pred.dens>1),                    #Number of days Persistence
             Sum.Zero.Prey.Densities.Locally=sum(prey.dens<1),Sum.Zero.Predator.Densities.Locally=sum(pred.dens<1),# Number of days  Zero
             cv.prey=raster::cv(prey.dens,na.rm=T), cv.pred=raster::cv(pred.dens,na.rm=T)) %>%
  mutate(log.number.bottles=log(number.bottles+1),log.network.syn.lap=log(network.syn.lap+1),log.total.vol=log(total.vol+1))

################################################################################################################################################################################################################################################################
#Days to Extinction
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
#Easy Data ANalysis

#gather(prey.minimia,pred.minimia,day.prey.min,day.pred.min,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den,prey.persistence,pred.persistence,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,cv.prey,cv.pred, key = "var", value = "value") %>% 
  
  
#LM Loop
varlist<-names(loc.all)[c(13:28)]
models <- lapply(varlist, function(y) {
  lm(substitute(i~patch.degree, list(i = as.name(y))), data = loc.all)
})
lapply(models, summary)

dog<-lm(day.pred.max~meta.size,loc.all)
summary(dog)

#ANOVA LOOP
dog<-aov(prey.amp~as.factor(patch.degree), data = loc.all)
summary(dog)
########################################################################################################################################################################################################################################################################################################################
#GLMMS

####REGIONAL#######

library(MuMIn)
library(lme4)

options(na.action = "na.fail")

all.parms<-lmer(pred.persistence~ meta.size+patch.degree+prey+network.syn.lap+media+total.vol+connect.per+(1|replicate), data = loc.all )
#all.parms<-glmer(N1~log(Local.Distance+1)+log(Regional.Distance+1)+log(Elevation+1)+  PC1+PC2+PC3+PC4+(1|Network), data = Dist.loc.alls)
#all.parms<-glmer((`bad$LCBD`*10)~log(Local.Distance+1)+log(Regional.Distance+1)+log(Elevation+1)+ PC1+PC2+PC3+PC4+(1|Network), data = Dist.loc.alls)

# the dredge function fits all combinations
# of the variables in the all.parms model fit above
results<-dredge(all.parms)
results
subset(results, delta <5)
importance(results)
anova(all.parms)

loc.all %>%
  ggplot(aes(x =meta.size , y = pred.persistence)) +
  geom_point() +
  geom_smooth(method='lm')+
  facet_wrap(~ prey, scales = "free") +
  theme_bw()


mod_gam2<-glmer(pred.persistence~connect.per+(1|replicate), data = reg.all )
summary(mod_gam2)
plot(mod_gam2,col=reg.all$media,id=0.05)
plot(mod_gam2,sqrt(abs(residuals(.))) ~ fitted(.),type=c("p","smooth"))
qqmath(mod_gam2,col=reg.all$prey)

pred <- predict(mod_gam2,re.form=NA)  ## population level
pred1 <- predict(mod_gam2) 

ggplot(reg.all, aes(connect.per,pred.persistence,group=prey))+geom_point()+geom_smooth(method="lm")+
  facet_grid(.~prey)+ geom_line(colour="red", aes(y=pred, group=prey))

ggplot(reg.all, aes(x=total.vol,y=cv.prey))+geom_point()+geom_smooth(method="lm")

dog<-lm(pred.persistence~connect.per, reg.all)
summary(dog)

options(na.action = "na.fail")

all.parms<-lmer(cv.pred~ meta.size+patch.degree+predator+prey+network.syn.lap+media+total.vol+connect.per+(1|replicate), data = reg.all )
#all.parms<-glmer(N1~log(Local.Distance+1)+log(Regional.Distance+1)+log(Elevation+1)+  PC1+PC2+PC3+PC4+(1|Network), data = Dist.loc.alls)
#all.parms<-glmer((`bad$LCBD`*10)~log(Local.Distance+1)+log(Regional.Distance+1)+log(Elevation+1)+ PC1+PC2+PC3+PC4+(1|Network), data = Dist.loc.alls)

# the dredge function fits all combinations
# of the variables in the all.parms model fit above
results<-dredge(all.parms)
results
subset(results, delta <5)
importance(results)
anova(all.parms)

mod_gam2<-glmer(cv.pred~(total.vol)+(1|replicate), data = reg.all )
summary(mod_gam2)
plot(mod_gam2,col=reg.all$media,id=0.05)
plot(mod_gam2,sqrt(abs(residuals(.))) ~ fitted(.),type=c("p","smooth"))
qqmath(mod_gam2,col=reg.all$pred)

pred <- predict(mod_gam2,re.form=NA)  ## population level
pred1 <- predict(mod_gam2) 

ggplot(reg.all, aes(total.vol,cv.pred,group=pred))+geom_point()+geom_smooth(method="lm")+
  facet_grid(.~pred)+ geom_line(colour="red", aes(y=pred, group=pred))

ggplot(reg.all, aes(log(meta.size),cv.pred))+geom_point()+geom_smooth(method="lm")+ geom_line(colour="red", aes(y=pred))



##############
all.parms<-lmer(pred.persistence~ meta.size+patch.degree+predator+prey+network.syn.lap+media+(1|replicate), data = reg.all )

mod1<-glm(pred.persistence~(meta.size),family=gaussian(link = "identity"),data=loc.all)
mod2<-glm(pred.persistence~(patch.degree),family=gaussian(link = "identity"),data=loc.all)
mod3<-glm(pred.persistence~(network.syn.lap),family=gaussian(link = "identity"),data=loc.all)
mod4<-glm(pred.persistence~(network.syn.lap)*meta.size,family=gaussian(link = "identity"),data=loc.all)
mod5<-glm(pred.persistence~(network.syn.lap)*patch.degree,family=gaussian(link = "identity"),data=loc.all)
mod6<-glm(pred.persistence~(patch.degree)*meta.size,family=gaussian(link = "identity"),data=loc.all)
mod7<-glm(pred.persistence~(network.syn.lap)*meta.size*patch.degree,family=gaussian(link = "identity"),data=loc.all)
nullmod<-glm(pred.persistence~1,family=gaussian(link = "identity"),data=loc.all)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,nullmod,weights = TRUE, sort = T)

mod1<-glm(cv.pred~(meta.size),family=gaussian(link = "identity"),data=loc.all)
mod2<-glm(cv.pred~(patch.degree),family=gaussian(link = "identity"),data=loc.all)
mod3<-glm(cv.pred~(network.syn.lap),family=gaussian(link = "identity"),data=loc.all)
nullmod<-glm(cv.pred~1,family=gaussian(link = "identity"),data=loc.all)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,nullmod,weights = TRUE, sort = T)

mod1<-glm(pred.den~(meta.size),family=gaussian(link = "identity"),data=loc.all)
mod2<-glm(pred.den~(patch.degree),family=gaussian(link = "identity"),data=loc.all)
mod3<-glm(pred.den~(network.syn.lap),family=gaussian(link = "identity"),data=loc.all)
nullmod<-glm(pred.den~1,family=gaussian(link = "identity"),data=loc.all)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,nullmod,weights = TRUE, sort = T)


loc.all%>%
  ggplot(aes(x=network.syn.lap, y=(pred.den)))+
  geom_point()+
  geom_smooth(method = "lm")


pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoR7 <- ((mod7$null.deviance-mod7$deviance)/mod7$null.deviance)

pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)
pseudoRnull <- ((null$null.deviance-null$deviance)/null$null.deviance)

psuedoR2<-rbind(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoRnullmod,pseudoRnull)


all.parms<-lmer(pred.persistence~ meta.size+patch.degree+prey+network.syn.lap+connect.per+patch.deg2+total.vol+(1|replicate), data = loc.all )
results<-dredge(all.parms)
results
subset(results, delta <5)
importance(results)
anova(all.parms)

mod1<-glm(Sum.Zero.Predator.Densities.Locally~(total.),family=gaussian(link = "identity"),data=loc.all)
mod2<-glm(Sum.Zero.Predator.Densities.Locally~(patch.degree),family=gaussian(link = "identity"),data=loc.all)
mod3<-glm(Sum.Zero.Predator.Densities.Locally~(network.syn.lap),family=gaussian(link = "identity"),data=loc.all)
nullmod<-glm(Sum.Zero.Predator.Densities.Locally~1,family=gaussian(link = "identity"),data=loc.all)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,nullmod,weights = TRUE, sort = T)

loc.all%>%
  ggplot(aes(x=meta.size, y=(pred.persistence)))+
  geom_point()+
  geom_smooth(method = "lm")

mod1<-glm(pred.persistence~(meta.size),family=gaussian(link = "identity"),data=loc.all)
mod2<-glm(pred.persistence~(patch.degree),family=gaussian(link = "identity"),data=loc.all)
mod3<-glm(pred.persistence~(network.syn.lap),family=gaussian(link = "identity"),data=loc.all)
mod4<-glm(pred.persistence~(connect.per),family=gaussian(link = "identity"),data=loc.all)
mod6<-glm(pred.persistence~(tube.length),family=gaussian(link = "identity"),data=loc.all)
mod7<-glm(pred.persistence~(patch.deg2),family=gaussian(link = "identity"),data=loc.all)
mod8<-glm(pred.persistence~(total.vol),family=gaussian(link = "identity"),data=loc.all)
mod9<-glm(pred.persistence~(nghbr.connect),family=gaussian(link = "identity"),data=loc.all)
nullmod<-glm(pred.persistence~1,family=gaussian(link = "identity"),data=loc.all)

reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod7,mod8,mod9,nullmod,weights = TRUE, sort = T)
reported.table2

mod1<-glm(pred.persistence~(meta.size),family=gaussian(link = "identity"),data=loc.all)
mod2<-glm(pred.persistence~(patch.degree),family=gaussian(link = "identity"),data=loc.all)
mod3<-glm(pred.persistence~(network.syn.lap),family=gaussian(link = "identity"),data=loc.all)
mod4<-glm(pred.persistence~(connect.per),family=gaussian(link = "identity"),data=loc.all)
mod6<-glm(pred.persistence~(tube.length),family=gaussian(link = "identity"),data=loc.all)
mod7<-glm(pred.persistence~(patch.deg2),family=gaussian(link = "identity"),data=loc.all)
mod8<-glm(pred.persistence~(total.vol),family=gaussian(link = "identity"),data=loc.all)
mod9<-glm(pred.persistence~(nghbr.connect),family=gaussian(link = "identity"),data=loc.all)
nullmod<-glm(pred.persistence~1,family=gaussian(link = "identity"),data=loc.all)

reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod7,mod8,mod9,nullmod,weights = TRUE, sort = T)
reported.table2

pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)
pseudoRnull <- ((null$null.deviance-null$deviance)/null$null.deviance)