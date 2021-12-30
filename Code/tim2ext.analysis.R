#Local
loc.all<-Data %>%
  #filter(day > 4)%>%
  filter(day > 3 & day < 75)%>%
  filter(number.bottles>1)%>%
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
                   prey.oc=mean(prey.oc),pred.oc=mean(pred.oc),
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
####################################################################################################################################

#Focus on extinctions


#Time to Ext Figs
e1<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(productivity),y=prey.time.2.ext, fill=as.factor(productivity)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ggtitle("a)") +
  ylab("Prey Time to Extinction")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e5<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(productivity),y=pred.time.2.ext, fill=as.factor(productivity)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  #ggtitle("e)") +
  ggtitle("d)") +
  ylab("Predator Time to Extinction")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e2<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=log.number.bottles,y=prey.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ggtitle("b)") +
  xlab("Metacommunity Size")+ ylab("Prey Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e3<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=log.network.syn.lap,y=prey.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ggtitle("c)") +
  xlab("Network Synchrony")+ ylab("Prey Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e4<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=nghbr.connect,y=prey.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  #ggtitle("d)") +
  ggtitle("c)") +
  xlab("Nearest Neighboor Connectivity")+ ylab("Prey Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e6<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=log.number.bottles,y=pred.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  #ggtitle("f)") +
  ggtitle("e)") +
  xlab("Metacommunity Size")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e7<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=log.network.syn.lap,y=pred.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ggtitle("g)") +
  xlab("Network Synchrony")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e8<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=nghbr.connect,y=pred.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  #ggtitle("h)") +
  ggtitle("f)") +
  xlab("Nearest Neighboor Connectivity")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

plot_grid(e1,e2,e3,e4,e5,e6,e7,e8, nrow=2)
plot_grid(e1,e2,e4,e5,e6,e8, nrow=2)

####################################################################################################################################
#GLMS

#Prey
y<-loc.all$prey.time.2.ext

mod0<-glm(y~as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod1<-glm(y~log.number.bottles,family=poisson(link="log"),data=loc.all)
mod2<-glm(y~(nghbr.connect),family=poisson(link="log"),data=loc.all)
mod3<-glm(y~log.network.syn.lap,family=poisson(link="log"),data=loc.all)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=poisson(link="log"),data=loc.all)
mod5<-glm(y~log.network.syn.lap+nghbr.connect,family=poisson(link="log"),data=loc.all)
mod6<-glm(y~(nghbr.connect)+log.number.bottles,family=poisson(link="log"),data=loc.all)
mod7<-glm(y~(nghbr.connect)+as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect,family=poisson(link="log"),data=loc.all)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+nghbr.connect,family=poisson(link="log"),data=loc.all)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+nghbr.connect,family=poisson(link="log"),data=loc.all)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect+as.factor(productivity),family=poisson(link="log"),data=loc.all)
nullmod<-glm(y~1,family=poisson(link="log"),data=occupnacy)

#reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod6,mod7,mod9,mod13,nullmod,weights = TRUE, sort = F)
reported.table2

pseudoR0 <- ((mod0$null.deviance-mod0$deviance)/mod0$null.deviance)
pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoR7 <- ((mod7$null.deviance-mod7$deviance)/mod7$null.deviance)
pseudoR8 <- ((mod8$null.deviance-mod8$deviance)/mod8$null.deviance)
pseudoR9 <- ((mod9$null.deviance-mod9$deviance)/mod9$null.deviance)
pseudoR10 <- ((mod10$null.deviance-mod10$deviance)/mod10$null.deviance)
pseudoR11 <- ((mod11$null.deviance-mod11$deviance)/mod11$null.deviance)
pseudoR12 <- ((mod12$null.deviance-mod12$deviance)/mod12$null.deviance)
pseudoR13 <- ((mod13$null.deviance-mod13$deviance)/mod13$null.deviance)
pseudoR14 <- ((mod14$null.deviance-mod14$deviance)/mod14$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR6,pseudoR7,pseudoR9,pseudoR13,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22


############################################################################################################
#Pred
y<-loc.all$pred.time.2.ext

mod0<-glm(y~as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod1<-glm(y~log.number.bottles,family=poisson(link="log"),data=loc.all)
mod2<-glm(y~(nghbr.connect),family=poisson(link="log"),data=loc.all)
mod3<-glm(y~log.network.syn.lap,family=poisson(link="log"),data=loc.all)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=poisson(link="log"),data=loc.all)
mod5<-glm(y~log.network.syn.lap+nghbr.connect,family=poisson(link="log"),data=loc.all)
mod6<-glm(y~(nghbr.connect)+log.number.bottles,family=poisson(link="log"),data=loc.all)
mod7<-glm(y~(nghbr.connect)+as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect,family=poisson(link="log"),data=loc.all)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=poisson(link="log"),data=loc.all)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+nghbr.connect,family=poisson(link="log"),data=loc.all)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+nghbr.connect,family=poisson(link="log"),data=loc.all)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect+as.factor(productivity),family=poisson(link="log"),data=loc.all)
nullmod<-glm(y~1,family=poisson(link="log"),data=occupnacy)

#reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod6,mod7,mod9,mod13,nullmod,weights = TRUE, sort = F)
reported.table2

pseudoR0 <- ((mod0$null.deviance-mod0$deviance)/mod0$null.deviance)
pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoR7 <- ((mod7$null.deviance-mod7$deviance)/mod7$null.deviance)
pseudoR8 <- ((mod8$null.deviance-mod8$deviance)/mod8$null.deviance)
pseudoR9 <- ((mod9$null.deviance-mod9$deviance)/mod9$null.deviance)
pseudoR10 <- ((mod10$null.deviance-mod10$deviance)/mod10$null.deviance)
pseudoR11 <- ((mod11$null.deviance-mod11$deviance)/mod11$null.deviance)
pseudoR12 <- ((mod12$null.deviance-mod12$deviance)/mod12$null.deviance)
pseudoR13 <- ((mod13$null.deviance-mod13$deviance)/mod13$null.deviance)
pseudoR14 <- ((mod14$null.deviance-mod14$deviance)/mod14$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR6,pseudoR7,pseudoR9,pseudoR13,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22