library(ggplot2)
library(tidyverse)
library(raster)

setwd("~/Dropbox/Protist Lab Data/Kurt_Matthew_Shared Data/Dendritic Data/Holistic/Data")

#Data = read.csv("eup.tetra.18.19.csv")
Data=read.csv("Data/datas.all.csv")
summary(Data)

#Average for three structure types Normal
#all<-Data %>%
#  group_by(structure,day) %>%
#  summarise(density.prey =mean(tetra.density),sd.prey = sd(tetra.density), cv.prey= (sd.prey/density.prey) *100, density.pred= mean(eup.density), sd.pred=sd(eup.density), cv.pred=(sd.pred/density.pred)*100)

#ggplot(all, aes(x=day, y=density.prey, linetype="dotted", colour=factor(structure)))+ geom_line() +geom_line(data=all, aes(x=day,y=density.pred, linetype="dashed"))+ labs(x="day",y="ln(density)") +scale_colour_discrete(labels=c("Dendritic", "Lattice"))+ guides(colour=guide_legend(title="Treatment"))+scale_linetype(labels=c("Euplotes", "Tetrahymnea"))  + guides(colour=guide_legend(title="Network Structure"), linetype=guide_legend(title = "Predator or Prey"))
########################

#Average for three structure types Log
all<-Data %>%
  group_by(structure,day,year) %>%
  summarise(density.prey =mean(ln.prey),sd.prey = sd(ln.prey), cv.prey= (sd.prey/density.prey) *100, density.pred= mean(ln.pred), sd.pred=sd(ln.pred), cv.pred=(sd.pred/density.pred)*100)

ggplot(all, aes(x=day, y=density.prey, linetype="dotted", colour=interaction(structure,as.factor(year))))+ geom_line() +geom_line(data=all, aes(x=day,y=density.pred, linetype="dashed", colour=interaction(structure,as.factor(year))))+ labs(x="day",y="ln(density)") +scale_colour_discrete(labels=c("Dendritic", "Lattice"))+ guides(colour=guide_legend(title="Treatment"))+scale_linetype(labels=c("Euplotes", "Tetrahymnea"))  + guides(colour=guide_legend(title="Network Structure"), linetype=guide_legend(title = "Predator or Prey"))

ggplot(all, aes(x=day,  colour=interaction(structure,as.factor(year))))+ 
  geom_line(data=all,aes(x=day, y=density.prey, linetype="dotted")) +geom_line(data=all, aes(x=day,y=density.pred, linetype="dashed"))+ 
  labs(x="day",y="ln(density)")  +
  scale_linetype(labels=c("Euplotes", "Tetrahymnea"))  + 
  guides(colour=guide_legend(title="Network Structure"), linetype=guide_legend(title = "Predator or Prey"))


##########################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################
#Patch Occupancy through time
Datasrr<-Data%>%
  filter(prey=="paramecium")%>%
  group_by(day,day,year,media)%>%
  summarise(prey.occ=mean(prey.oc), pred.occ=mean(pred.oc))

ggplot(Datasrr, aes(x=day))  +geom_line(aes(y=prey.occ, linetype="dotted", colour=interaction(as.factor(media))))+ 
  geom_line(aes(y=pred.occ, linetype="dashed", color=as.factor(media))) + 
  labs(x="Time (days)",y="Average Probability of Predator-Prey Patch Occupancy")+scale_linetype(labels=c("Euplotes", "Tetrahymena"))  + guides(colour=guide_legend(title="Prodcutivity"), linetype=guide_legend(title = "Predator or Prey"))

Datasrr<-Data%>%
  filter(structure=="dendritic")%>%
  group_by(day,day,year,media)%>%
  summarise(prey.occ=mean(prey.oc), pred.occ=mean(pred.oc))

ggplot(Datasrr, aes(x=day))  +geom_line(aes(y=prey.occ, linetype="dotted", colour=interaction(as.factor(year))))+ 
  geom_line(aes(y=pred.occ, linetype="dashed", color=as.factor(year))) + 
  labs(x="Time (days)",y="Average Probability of Predator-Prey Patch Occupancy")+scale_linetype(labels=c("Euplotes", "Tetrahymena"))  + guides(colour=guide_legend(title="Year"), linetype=guide_legend(title = "Predator or Prey"))

Datasrr<-Data%>%
  filter(structure=="lattice")%>%
  group_by(day,day,year,media)%>%
  summarise(prey.occ=mean(prey.oc), pred.occ=mean(pred.oc))

ggplot(Datasrr, aes(x=day))  +geom_line(aes(y=prey.occ, linetype="dotted", colour=interaction(as.factor(year))))+ 
  geom_line(aes(y=pred.occ, linetype="dashed", color=as.factor(year))) + 
  labs(x="Time (days)",y="Average Probability of Predator-Prey Patch Occupancy")+scale_linetype(labels=c("Euplotes", "Tetrahymena"))  + guides(colour=guide_legend(title="Year"), linetype=guide_legend(title = "Predator or Prey"))

Datasrr<-Data%>%
  filter(structure=="straight")%>%
  group_by(day,day,year,media)%>%
  summarise(prey.occ=mean(prey.oc), pred.occ=mean(pred.oc))

ggplot(Datasrr, aes(x=day))  +geom_line(aes(y=prey.occ, linetype="dotted", colour=interaction(as.factor(year))))+ 
  geom_line(aes(y=pred.occ, linetype="dashed", color=as.factor(year))) + 
  labs(x="Time (days)",y="Average Probability of Predator-Prey Patch Occupancy")+scale_linetype(labels=c("Euplotes", "Tetrahymena"))  + guides(colour=guide_legend(title="Year"), linetype=guide_legend(title = "Predator or Prey"))

########################################################################################################################################################################################################################################################################
#Calculate Holyoak Stats

#Total days where prey/predator are extinct locally or regionally
#Local
loc.all<-Data %>%
  #filter(day > 4 & day < 44)%>%
  filter(media=="medium" | media=="high")%>%
  group_by(predator,prey,network.syn.lap,replicate,media,bottle.number, number.bottles,structure) %>%
  summarise( sampling.days=n(),
             meta.size=mean(number.bottles),
             prey.minimia=min(prey.density),pred.minimia=min(pred.density),                                #Minima density
             day.prey.min=day[which.min(prey.density)]/sampling.days,day.pred.min=day[which.min(pred.density)]/sampling.days,          #Day of Minimia
             prey.amp=max(prey.density),pred.amp=max(pred.density),                                        #Amp density
             day.prey.max=day[which.max(prey.density)]/sampling.days,day.pred.max=day[which.max(pred.density)]/sampling.days,          #Day of Amp
             prey.den=mean(ln.prey),pred.den=mean(ln.pred),                                                #Mean Density
             prey.persistence=sum(prey.density>1)/sampling.days,pred.persistence=sum(pred.density>1)/sampling.days,                    #Number of days Persistence
             Sum.Zero.Prey.Densities.Locally=sum(prey.density==0)/sampling.days,Sum.Zero.Predator.Densities.Locally=sum(pred.density==0)/sampling.days,# Number of days  Zero
             cv.prey=cv(prey.density,na.rm = T), cv.pred=cv(pred.density,na.rm = T))# CV 

ggplot(loc.all, aes(x=structure, y=Sum.Zero.Prey.Densities.Locally, colour=interaction(media, prey)))+geom_boxplot()
ggplot(loc.all, aes(x=structure, y=Sum.Zero.Predator.Densities.Locally, colour=as.factor(media)))+geom_boxplot()

ggplot(loc.all, aes(x=structure, y=prey.persistence, colour=interaction(media, prey)))+geom_boxplot()
ggplot(loc.all, aes(x=structure, y=pred.persistence, colour=interaction(media, prey)))+geom_boxplot()

ggplot(loc.all, aes(x=structure, y=prey.den, colour=as.factor(media)))+geom_boxplot()
ggplot(loc.all, aes(x=structure, y=pred.den, colour=as.factor(media)))+geom_boxplot()

ggplot(loc.all, aes(x=structure, y=prey.minimia, colour=as.factor(media)))+geom_boxplot()
ggplot(loc.all, aes(x=structure, y=pred.minimia, colour=as.factor(media)))+geom_boxplot()

ggplot(loc.all, aes(x=structure, y=day.prey.max, colour=as.factor(media)))+geom_boxplot()
ggplot(loc.all, aes(x=structure, y=day.pred.max, colour=as.factor(media)))+geom_boxplot()

ggplot(loc.all, aes(x=structure, y=day.prey.min, colour=as.factor(media)))+geom_boxplot()
ggplot(loc.all, aes(x=structure, y=day.pred.min, colour=as.factor(media)))+geom_boxplot()

ggplot(loc.all, aes(x=structure, y=cv.prey, colour=as.factor(media)))+geom_boxplot()
ggplot(loc.all, aes(x=structure, y=cv.pred, colour=as.factor(media)))+geom_boxplot()

ggplot(loc.all, aes(x=network.syn.lap, y=prey.persistence, colour=as.factor(media)))+geom_point()+geom_smooth(method="lm")
ggplot(loc.all, aes(x=network.syn.lap, y=prey.persistence))+geom_point()+geom_smooth(method="lm")
ggplot(loc.all, aes(x=network.syn.lap, y=pred.persistence))+geom_point()+geom_smooth(method="lm")
ggplot(loc.all, aes(x=network.syn.lap, y=pred.persistence, colour=as.factor(media)))+geom_point()+geom_smooth(method="lm")
ggplot(loc.all, aes(x=meta.size, y=pred.persistence, colour=as.factor(media)))+geom_point()+geom_smooth(method="lm")
ggplot(loc.all, aes(x=meta.size, y=prey.persistence, colour=as.factor(media)))+geom_point()+geom_smooth(method="lm")

#Notes: High medium, predators more likely to go extinct, even in isoalted treatments

#Regional
reg.all<-Data %>%
  #filter(day > 4 & day < 44)%>%
  #filter(media=="medium")%>%
  group_by(predator,prey,network.syn.lap,number.bottles, structure,replicate,media) %>%
  summarise( sampling.days=n(),
             meta.size=mean(number.bottles),
             prey.minimia=min(prey.density),pred.minimia=min(pred.density),                                #Minima density
             day.prey.min=day[which.min(prey.density)]/sampling.days,day.pred.min=day[which.min(pred.density)]/sampling.days,          #Day of Minimia
             prey.amp=max(ln.prey),pred.amp=max(ln.pred),                                        #Amp density
             day.prey.max=day[which.max(prey.density)]/sampling.days,day.pred.max=day[which.max(pred.density)]/sampling.days,          #Day of Amp
             prey.den=mean(ln.prey),pred.den=mean(ln.pred),                                                #Mean Density
             prey.persistence=sum(prey.density>1)/sampling.days,pred.persistence=sum(pred.density>1)/sampling.days,                    #Number of days Persistence
             Sum.Zero.Prey.Densities.Locally=sum(prey.density==0)/sampling.days,Sum.Zero.Predator.Densities.Locally=sum(pred.density==0)/sampling.days,# Number of days  Zero
             cv.prey=cv(ln.prey,na.rm=T), cv.pred=cv(ln.pred,na.rm=T))# CV 





ggplot(reg.all, aes(x=structure, y=Sum.Zero.Prey.Densities.Regionally, colour=interaction(media,predator)))+geom_boxplot()
ggplot(reg.all, aes(x=structure, y=Sum.Zero.Predator.Densities.Regionally, colour=as.factor(media)))+geom_boxplot()

ggplot(reg.all, aes(x=structure, y=prey.persistence,  colour=interaction(media,predator)))+geom_boxplot()
ggplot(reg.all, aes(x=structure, y=pred.persistence, colour=interaction(media,predator)))+geom_boxplot()

ggplot(reg.all, aes(x=structure, y=prey.den, colour=as.factor(media)))+geom_boxplot()
ggplot(reg.all, aes(x=structure, y=pred.den, colour=as.factor(media)))+geom_boxplot()

ggplot(reg.all, aes(x=structure, y=prey.minimia, colour=as.factor(media)))+geom_boxplot()
ggplot(reg.all, aes(x=structure, y=pred.minimia, colour=as.factor(media)))+geom_boxplot()

ggplot(reg.all, aes(x=structure, y=day.prey.max, colour=as.factor(media)))+geom_boxplot()
ggplot(reg.all, aes(x=structure, y=day.pred.max, colour=as.factor(media)))+geom_boxplot()

ggplot(reg.all, aes(x=structure, y=day.prey.min, colour=as.factor(media)))+geom_boxplot()
ggplot(reg.all, aes(x=structure, y=day.pred.min, colour=as.factor(media)))+geom_boxplot()

ggplot(reg.all, aes(x=structure, y=Sum.Zero.Prey.Densities.Locally, colour=(media)))+geom_boxplot()
ggplot(reg.all, aes(x=structure, y=Sum.Zero.Predator.Densities.Locally, colour=as.factor(media)))+geom_boxplot()

ggplot(reg.all, aes(x=structure, y=cv.prey, colour=as.factor(media)))+geom_boxplot()
ggplot(reg.all, aes(x=structure, y=cv.pred, colour=as.factor(media)))+geom_boxplot()

ggplot(reg.all, aes(x=meta.size, y=Sum.Zero.Predator.Densities.Locally, colour=as.factor(media)))+geom_point()+geom_smooth(method = "lm")
ggplot(reg.all, aes(x=meta.size, y=Sum.Zero.Predator.Densities.Locally))+geom_point()+geom_smooth(method = "lm")

ggplot(reg.all, aes(x=meta.size, y=Sum.Zero.Prey.Densities.Locally, colour=as.factor(media)))+geom_point()+geom_smooth(method = "lm")
ggplot(reg.all, aes(x=meta.size, y=cv.pred))+geom_point()+geom_smooth(method = "lm")

ggplot(reg.all, aes(x=meta.size, y=day.pred.min))+geom_point()+geom_smooth(method = "lm")
ggplot(reg.all, aes(x=meta.size, y=day.prey.min))+geom_point()+geom_smooth(method = "lm")
dog<-lm(day.prey.min~meta.size, reg.all)
summary(dog)


ggplot(reg.all, aes(x=network.syn.lap, y=Sum.Zero.Predator.Densities.Locally, colour=as.factor(media)))+geom_point()+geom_smooth(method = "lm")
ggplot(reg.all, aes(x=network.syn.lap, y=Sum.Zero.Predator.Densities.Locally))+geom_point()+geom_smooth(method = "lm")

ggplot(reg.all, aes(x=network.syn.lap, y=Sum.Zero.Prey.Densities.Locally, colour=as.factor(media)))+geom_point()+geom_smooth(method = "lm")
ggplot(reg.all, aes(x=network.syn.lap, y=cv.pred))+geom_point()+geom_smooth(method = "lm")

ggplot(reg.all, aes(x=network.syn.lap, y=day.pred.min))+geom_point()+geom_smooth(method = "lm")
ggplot(reg.all, aes(x=network.syn.lap, y=day.prey.min))+geom_point()+geom_smooth(method = "lm")
################################################################################################################################################################################################################################################################################################################################
##########GLMMS################
library(MuMIn)
library(lme4)

options(na.action = "na.fail")

all.parms<-glmer(prey.amp~ predator+network.syn.lap+media+(1|replicate), data = reg.all )
#all.parms<-glmer(N1~log(Local.Distance+1)+log(Regional.Distance+1)+log(Elevation+1)+  PC1+PC2+PC3+PC4+(1|Network), data = Dist.species)
#all.parms<-glmer((`bad$LCBD`*10)~log(Local.Distance+1)+log(Regional.Distance+1)+log(Elevation+1)+ PC1+PC2+PC3+PC4+(1|Network), data = Dist.species)

# the dredge function fits all combinations
# of the variables in the all.parms model fit above
results<-dredge(all.parms)
results
subset(results, delta <5)
importance(results)
anova(all.parms)

mod_gam2<-glmer(prey.amp~network.syn.lap+(1|replicate), data = reg.all )
summary(mod_gam2)
plot(mod_gam2,col=reg.all$media,id=0.05)
plot(mod_gam2,sqrt(abs(residuals(.))) ~ fitted(.),type=c("p","smooth"))
qqmath(mod_gam2,col=Dist.species$Network)

pred <- predict(mod_gam2,re.form=NA)  ## population level
pred1 <- predict(mod_gam2) 

ggplot(reg.all, aes(network.syn.lap,prey.amp,group=prey))+geom_point()+geom_smooth(method="lm")+
  facet_grid(.~prey)+ geom_line(colour="red", aes(y=pred, group=prey))

ggplot(reg.all, aes(network.syn.lap,prey.amp))+geom_point()+geom_smooth(method="lm")+ geom_line(colour="red", aes(y=pred))


#####################################################################################################################################
#Plot outputs Regional
dev.off()
par(mfrow=c(4,4),mar= c(2, 4, 1, 1), mgp=c(2,1,0))
envlist=names(reg.all)
reg.all<-as.data.frame(reg.all)
for(i in c(10:25)){
  plot(reg.all$network.syn.lap,reg.all[,i],col=reg.all$media,ylab=envlist[c(i)],xlab="Structure Synchrony") 
  #test2 = lm(reg.all[,i],network.syn.lap, data=reg.all)
  #abline(test2)
}

varlist<-names(reg.all)[c(10:25)]
models <- lapply(varlist, function(y) {
  lm(substitute(i~reg.all$network.syn.lap, list(i = as.name(y))), data = reg.all)
})
lapply(models, summary)

#####################################################################################################################
#Plot outputs Local
dev.off()
par(mfrow=c(4,4),mar= c(2, 4, 1, 1), mgp=c(2,1,0))
envlist=names(loc.all)
loc.all<-as.data.frame(loc.all)
for(i in c(11:26)){
  plot(loc.all$network.syn.lap,loc.all[,i],col=loc.all$media,ylab=envlist[c(i)],xlab="Structure Synchrony") 
  #test2 = lm(loc.all[,i],network.syn.lap, data=loc.all)
  #abline(test2)
}

varlist<-names(loc.all)[c(10:25)]
models <- lapply(varlist, function(y) {
  lm(substitute(i~loc.all$network.syn.lap, list(i = as.name(y))), data = loc.all)
})
lapply(models, summary)













