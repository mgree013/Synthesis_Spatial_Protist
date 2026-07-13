#Trophic Analysis


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

loc.all.plot <- transform(
  loc.all.plot, 
  Nester = ifelse(pred.attack == "Euplotes-Tetrahymena", "Predator-Prey", "Predator-Prey"),
  Nester.2 = ifelse(productivity == "Low", "Productivity", "Productivity"))

loc.all.plot%>%
  ggplot(aes(x=pred.oc,y=prey.oc, colour=interaction(productivity,pred.attack)))+
  geom_point()+
  geom_smooth(data=filter(loc.all.plot,productivity=="Low"& pred.attack=="Euplotes-Tetrahymena"),  method = "lm")+
  geom_smooth(data=filter(loc.all.plot,productivity=="Low"& pred.attack=="Didinium-Paramecium"),  method = "lm")+
  geom_smooth(data=filter(loc.all.plot,productivity=="High"& pred.attack=="Didinium-Paramecium"),  method = "lm")+
  geom_smooth(data=filter(loc.all.plot,productivity=="Medium"& pred.attack=="Didinium-Colpidium"),  method = "lm")+
  #geom_text(data=filter(loc.all.plot,productivity=="Low"& pred.attack=="Euplotes-Tetrahymena"),x = .3, y = .75, label = "R^2 == 0.28", parse = TRUE, colour="black")+
  #geom_text(data=filter(loc.all.plot,productivity=="Low"& pred.attack=="Didinium-Paramecium"),x = .75, y = .6, label = "R^2 == 0.50", parse = TRUE, colour="black")+
  #geom_text(data=filter(loc.all.plot,productivity=="High"& pred.attack=="Didinium-Paramecium"),x = .3, y = .2, label = "R^2 == 0.28", parse = TRUE, colour="black")+
  #geom_text(data=filter(loc.all.plot,productivity=="Medium"& pred.attack=="Didinium-Colpidium"),x = .3, y = .2, label = "R^2 == 0.32", parse = TRUE, colour="black")+
  #geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Occupancy")+ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_nested(Nester+ pred.attack~ Nester.2+ productivity)+ theme(strip.text.y = element_text(face = "italic"))
  
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
