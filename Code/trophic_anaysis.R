#Trophic Analysis

loc.all<-Data %>%
  #filter(day > 4)%>%
  filter(day > 3 & day < 75)%>%
  filter(number.bottles > 1)%>%
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
                   prey.sum.oc=sum(prey.oc), pred.sum.oc=sum(pred.oc),
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
                   prey.persistence=sum(ln.prey>0)/(meta.size+sampling.days),pred.persistence=sum(ln.pred>0)/(meta.size+sampling.days),                    #Number of days Persistence
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
##############################################################################################################################
#Plotting

#Figure 8

#take1
loc.all%>%
  ggplot(aes(x=pred.oc,y=prey.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Occupancy")+ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(pred.attack~prod)

loc.all.plot<-loc.all%>%
  mutate(productivity=if_else(productivity ==  "0.56","Low",if_else(productivity ==  "0.76","Medium", "High")))%>%
  mutate(pred.attack=if_else(pred.attack ==  "0.31925","Euplotes-Tetrahymena",if_else(pred.attack ==  "0.4272","Didinium-Colpidium", "Didinium-Paramecium")))

loc.all.plot$productivity = factor(loc.all.plot$productivity, levels=c('Low','Medium','High'))
loc.all.plot$pred.attack = factor(loc.all.plot$pred.attack, levels=c('Euplotes-Tetrahymena','Didinium-Colpidium','Didinium-Paramecium'))

loc.all.plot%>%
  ggplot(aes(x=pred.oc,y=prey.oc, colour=interaction(productivity,pred.attack)))+
  geom_point()+
  geom_smooth(data=filter(loc.all.plot,productivity=="Low"& pred.attack=="Euplotes-Tetrahymena"),  method = "lm")+
  geom_smooth(data=filter(loc.all.plot,productivity=="Low"& pred.attack=="Didinium-Paramecium"),  method = "lm")+
  geom_smooth(data=filter(loc.all.plot,productivity=="High"& pred.attack=="Didinium-Paramecium"),  method = "lm")+
  geom_smooth(data=filter(loc.all.plot,productivity=="Medium"& pred.attack=="Didinium-Colpidium"),  method = "lm")+
  geom_text(data=filter(loc.all.plot,productivity=="Low"& pred.attack=="Euplotes-Tetrahymena"),x = .3, y = .75, label = "R^2 == 0.28", parse = TRUE, colour="black")+
  geom_text(data=filter(loc.all.plot,productivity=="Low"& pred.attack=="Didinium-Paramecium"),x = .75, y = .6, label = "R^2 == 0.50", parse = TRUE, colour="black")+
  geom_text(data=filter(loc.all.plot,productivity=="High"& pred.attack=="Didinium-Paramecium"),x = .3, y = .2, label = "R^2 == 0.28", parse = TRUE, colour="black")+
  geom_text(data=filter(loc.all.plot,productivity=="Medium"& pred.attack=="Didinium-Colpidium"),x = .3, y = .2, label = "R^2 == 0.32", parse = TRUE, colour="black")+
  #geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Occupancy")+ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(pred.attack~productivity)

  
#take2
d1<-did_para_high%>%
  ggplot(aes(x=pred.oc,y=prey.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Occupancy")+ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

d2<-did_para_low%>%
  ggplot(aes(x=pred.oc,y=prey.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Occupancy")+ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

d3<-did_colp_med%>%
  ggplot(aes(x=pred.oc,y=prey.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Occupancy")+ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e1<-eup_tetra_high%>%
  ggplot(aes(x=pred.oc,y=prey.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Occupancy")+ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e2<-eup_tetra_low%>%
  ggplot(aes(x=pred.oc,y=prey.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Occupancy")+ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")



plot_grid(d1,d2,d3,e1,e2, nrow=2)
##############################################################################################################################

#Analysis

#Organaize pred/prod combos
did_para_high<-loc.all%>%
  filter(productivity==1.28)%>%
  filter(pred.attack>0.48)

did_para_low<-loc.all%>%
  filter(productivity=="0.56")%>%
  filter(pred.attack>0.48)

did_colp_med<-loc.all%>%
  filter(productivity=="0.76")%>%
  filter(pred.attack=="0.4272")

eup_tetra_high<-loc.all%>%
  filter(productivity=="1.28")%>%
  filter(pred.attack=="0.31925")

eup_tetra_low<-loc.all%>%
  filter(productivity=="0.56")%>%
  filter(pred.attack=="0.31925")


#GLMS
#Did Para High
y <- cbind(did_para_high$prey.sum.oc, did_para_high$sampling.days)
glm1<-glm(y~pred.oc, family=binomial(link="logit"),data=did_para_high)
nullglm<-glm(y~1, family=binomial(link="logit"),data=did_para_high)

reported.table2 <- bbmle::AICtab(glm1,nullglm,weights = TRUE, sort = F)
reported.table2

pseudoR0 <- ((glm1$null.deviance-glm1$deviance)/glm1$null.deviance)
pseudoR0

#Did Para Low
y <- cbind(did_para_low$prey.sum.oc, did_para_low$sampling.days)
glm1<-glm(y~pred.oc, family=binomial(link="logit"),data=did_para_low)
nullglm<-glm(y~1, family=binomial(link="logit"),data=did_para_low)

reported.table2 <- bbmle::AICtab(glm1,nullglm,weights = TRUE, sort = F)
reported.table2

pseudoR0 <- ((glm1$null.deviance-glm1$deviance)/glm1$null.deviance)
pseudoR0

#Did Col Med
y <- cbind(did_colp_med$prey.sum.oc, did_colp_med$sampling.days)
glm1<-glm(y~pred.oc, family=binomial(link="logit"),data=did_colp_med)
nullglm<-glm(y~1, family=binomial(link="logit"),data=did_colp_med)

reported.table2 <- bbmle::AICtab(glm1,nullglm,weights = TRUE, sort = F)
reported.table2

pseudoR0 <- ((glm1$null.deviance-glm1$deviance)/glm1$null.deviance)
pseudoR0

#Eup tetra high
y <- cbind(eup_tetra_high$prey.sum.oc, eup_tetra_high$sampling.days)
glm1<-glm(y~pred.oc, family=binomial(link="logit"),data=eup_tetra_high)
nullglm<-glm(y~1, family=binomial(link="logit"),data=eup_tetra_high)

reported.table2 <- bbmle::AICtab(glm1,nullglm,weights = TRUE, sort = F)
reported.table2

pseudoR0 <- ((glm1$null.deviance-glm1$deviance)/glm1$null.deviance)
pseudoR0

#Eup tetra low
y <- cbind(eup_tetra_low$prey.sum.oc, eup_tetra_low$sampling.days)
glm1<-glm(y~pred.oc, family=binomial(link="logit"),data=eup_tetra_low)
nullglm<-glm(y~1, family=binomial(link="logit"),data=eup_tetra_low)

reported.table2 <- bbmle::AICtab(glm1,nullglm,weights = TRUE, sort = F)
reported.table2

pseudoR0 <- ((glm1$null.deviance-glm1$deviance)/glm1$null.deviance)
pseudoR0
