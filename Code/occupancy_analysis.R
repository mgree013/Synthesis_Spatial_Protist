#2)Occupnacy
occupnacy<-newer_pa_datas%>%
  filter(day > 3 & day < 75)%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate, connectivity, bottle,newID)%>%
  summarise(n=n(),
            pred.attack=mean(Pred.attack.rate),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  left_join(all_pa_dataz, by="newID")%>%
  mutate(log.number.bottles=log(number.bottles+1))%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))%>%
  mutate(pred.size=log(Pred.size+1))%>%
  mutate(prey.size=log(Prey.size+1))%>%
  filter(connectivity.x>0)

occupnacy$pred.attack<-as.factor(occupnacy$pred.attack)
occupnacy$productivity<-as.factor(occupnacy$productivity)

########################################################################
#Plots
a<-occupnacy%>%
  ggplot(aes(x=log.number.bottles,y=prey.oc))+ 
  geom_point()+
  ggtitle("a)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

b<-occupnacy%>%
  ggplot(aes(x=log.network.syn.lap,y=prey.oc))+ 
  geom_point()+
  ggtitle("b)") +
  #geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

c<-occupnacy%>%
  ggplot(aes(x=nghbr.connect,y=prey.oc))+ 
  geom_point()+
  #ggtitle("d)") +
  ggtitle("c)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

d<-occupnacy%>%
  ggplot(aes(x=log.number.bottles,y=pred.oc))+ 
  geom_point()+
  #ggtitle("f)") +
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

e<-occupnacy%>%
  ggplot(aes(x=log.network.syn.lap,y=pred.oc))+ 
  geom_point()+
  ggtitle("e)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-occupnacy%>%
  ggplot(aes(x=nghbr.connect,y=pred.oc))+ 
  geom_point()+
  #ggtitle("h)") +
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

op1<-occupnacy%>%
  filter(connectivity.x>0)%>%
  ggplot(aes(x=as.factor(productivity),y=pred.oc,fill=as.factor(productivity)))+ 
  geom_boxplot()+
  #ggtitle("e)") +
  ggtitle("b)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Productivity (g)",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

op2<-occupnacy%>%
  filter(connectivity.x>0)%>%
  ggplot(aes(x=as.factor(productivity),y=prey.oc,fill=as.factor(productivity)))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Productivity (g)",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

opp1<-occupnacy%>%
  filter(connectivity.x>0)%>%
  ggplot(aes(x=as.factor(predator),y=pred.oc,fill=as.factor(predator)))+ 
  geom_boxplot()+
  #ggtitle("e)") +
  ggtitle("d)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Predator Identity",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

opp2<-occupnacy%>%
  filter(connectivity.x>0)%>%
  ggplot(aes(x=as.factor(predator),y=prey.oc,fill=as.factor(predator)))+ 
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Predator Identity",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")


plot_grid(op2,a,b,c,op1,d,e,f,nrow=2)
plot_grid(op2,opp2,a,b,c,op1,opp1,d,e,f,nrow=2)

plot_grid(a,b,c,d,e,f,nrow=2)

plot_grid(op2,a,c,op1,d,f,nrow=2)

plot_grid(op2,opp2,op1,opp1,nrow=2)


occupnacy%>%
  #filter(connectivity.y >= 1)%>%
  ggplot(aes(x=pred.oc,y=prey.oc, colour=prey.y))+ 
  geom_point()+
  #ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Occupancy",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+facet_wrap(number.bottles~structure.x, scales="free") #+facet_grid(media~predator)

################################################################################################################################################
#Prey
y <- cbind(occupnacy$prey.occupany, occupnacy$n)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod2<-glm(y~(nghbr.connect),family=binomial(link="logit"),data=occupnacy)
mod3<-glm(y~log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod5<-glm(y~log.network.syn.lap+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod6<-glm(y~(nghbr.connect)+log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod7<-glm(y~(nghbr.connect)+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
nullmod<-glm(y~1,family=binomial(link="logit"),data=occupnacy)

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

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16,pseudoR17,pseudoR18,pseudoR19,pseudoR20,pseudoR21,pseudoR22,pseudoR23,pseudoR24,pseudoR25,pseudoR26,pseudoR27,pseudoR28,pseudoR29,pseudoR30,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR6,pseudoR7,pseudoR9,pseudoR13,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

#New take Bigger model
#https://math.stackexchange.com/questions/161565/what-is-the-total-number-of-combinations-of-5-items-together-when-there-are-no-d
y <- cbind(occupnacy$prey.occupany, occupnacy$n)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod2<-glm(y~nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod3<-glm(y~log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod4<-glm(y~as.factor(predator),family=binomial(link="logit"),data=occupnacy)

mod5<-glm(y~as.factor(productivity)+log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod6<-glm(y~as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod7<-glm(y~as.factor(productivity)+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod8<-glm(y~as.factor(productivity)+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod9<-glm(y~log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod10<-glm(y~log.number.bottles+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod11<-glm(y~log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod12<-glm(y~nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod13<-glm(y~nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod14<-glm(y~log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)

mod15<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod16<-glm(y~as.factor(productivity)+log.number.bottles+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod17<-glm(y~as.factor(productivity)+log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod18<-glm(y~as.factor(productivity)+nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod19<-glm(y~as.factor(productivity)+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod20<-glm(y~as.factor(productivity)+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod21<-glm(y~log.number.bottles+nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod22<-glm(y~log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod23<-glm(y~log.number.bottles+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod24<-glm(y~nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)

mod25<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod26<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod27<-glm(y~as.factor(productivity)+log.number.bottles+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod28<-glm(y~as.factor(productivity)+nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod29<-glm(y~log.number.bottles+nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)

mod30<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
nullmod<-glm(y~1,family=binomial(link="logit"),data=occupnacy)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,mod19,mod20,mod21,mod22,mod23,mod24,mod25,mod26,mod27,mod28,mod29,mod30,nullmod,weights = TRUE, sort = F)
reported.table2

plot_model(mod30)
summary(mod30)

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
pseudoR15 <- ((mod15$null.deviance-mod15$deviance)/mod15$null.deviance)
pseudoR16 <- ((mod16$null.deviance-mod16$deviance)/mod16$null.deviance)
pseudoR17 <- ((mod17$null.deviance-mod17$deviance)/mod17$null.deviance)
pseudoR18 <- ((mod18$null.deviance-mod18$deviance)/mod18$null.deviance)
pseudoR19 <- ((mod19$null.deviance-mod19$deviance)/mod19$null.deviance)
pseudoR20 <- ((mod20$null.deviance-mod20$deviance)/mod20$null.deviance)
pseudoR21 <- ((mod21$null.deviance-mod21$deviance)/mod21$null.deviance)
pseudoR22 <- ((mod22$null.deviance-mod22$deviance)/mod22$null.deviance)
pseudoR23 <- ((mod23$null.deviance-mod23$deviance)/mod23$null.deviance)
pseudoR24 <- ((mod24$null.deviance-mod24$deviance)/mod24$null.deviance)
pseudoR25 <- ((mod25$null.deviance-mod25$deviance)/mod25$null.deviance)
pseudoR26 <- ((mod26$null.deviance-mod26$deviance)/mod26$null.deviance)
pseudoR27 <- ((mod27$null.deviance-mod27$deviance)/mod27$null.deviance)
pseudoR28 <- ((mod28$null.deviance-mod28$deviance)/mod28$null.deviance)
pseudoR29 <- ((mod29$null.deviance-mod29$deviance)/mod29$null.deviance)
pseudoR30 <- ((mod30$null.deviance-mod30$deviance)/mod30$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16,pseudoR17,pseudoR18,pseudoR19,pseudoR20,pseudoR21,pseudoR22,pseudoR23,pseudoR24,pseudoR25,pseudoR26,pseudoR27,pseudoR28,pseudoR29,pseudoR30,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR6,pseudoR7,pseudoR9,pseudoR13,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

#######pred 
y <- cbind(occupnacy$pred.occupany, occupnacy$n)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod2<-glm(y~(nghbr.connect),family=binomial(link="logit"),data=occupnacy)
mod3<-glm(y~log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod5<-glm(y~log.network.syn.lap+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod6<-glm(y~(nghbr.connect)+log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod7<-glm(y~(nghbr.connect)+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect+as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
nullmod<-glm(y~1,family=binomial(link="logit"),data=occupnacy)

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

#Bigger Models
y <- cbind(occupnacy$pred.occupany, occupnacy$n)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=occupnacy)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod2<-glm(y~nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod3<-glm(y~log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod4<-glm(y~as.factor(predator),family=binomial(link="logit"),data=occupnacy)

mod5<-glm(y~as.factor(productivity)+log.number.bottles,family=binomial(link="logit"),data=occupnacy)
mod6<-glm(y~as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod7<-glm(y~as.factor(productivity)+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod8<-glm(y~as.factor(productivity)+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod9<-glm(y~log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod10<-glm(y~log.number.bottles+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod11<-glm(y~log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod12<-glm(y~nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod13<-glm(y~nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod14<-glm(y~log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)

mod15<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=occupnacy)
mod16<-glm(y~as.factor(productivity)+log.number.bottles+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod17<-glm(y~as.factor(productivity)+log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod18<-glm(y~as.factor(productivity)+nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod19<-glm(y~as.factor(productivity)+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod20<-glm(y~as.factor(productivity)+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod21<-glm(y~log.number.bottles+nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod22<-glm(y~log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod23<-glm(y~log.number.bottles+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod24<-glm(y~nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)

mod25<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=occupnacy)
mod26<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod27<-glm(y~as.factor(productivity)+log.number.bottles+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod28<-glm(y~as.factor(productivity)+nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
mod29<-glm(y~log.number.bottles+nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)

mod30<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
nullmod<-glm(y~1,family=binomial(link="logit"),data=occupnacy)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,mod19,mod20,mod21,mod22,mod23,mod24,mod25,mod26,mod27,mod28,mod29,mod30,nullmod,weights = TRUE, sort = F)
reported.table2

summary(mod30)
plot_model(mod30)
check_collinearity(mod30)

summary(mod23)
plot_model(mod23)
check_collinearity(mod30)

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
pseudoR15 <- ((mod15$null.deviance-mod15$deviance)/mod15$null.deviance)
pseudoR16 <- ((mod16$null.deviance-mod16$deviance)/mod16$null.deviance)
pseudoR17 <- ((mod17$null.deviance-mod17$deviance)/mod17$null.deviance)
pseudoR18 <- ((mod18$null.deviance-mod18$deviance)/mod18$null.deviance)
pseudoR19 <- ((mod19$null.deviance-mod19$deviance)/mod19$null.deviance)
pseudoR20 <- ((mod20$null.deviance-mod20$deviance)/mod20$null.deviance)
pseudoR21 <- ((mod21$null.deviance-mod21$deviance)/mod21$null.deviance)
pseudoR22 <- ((mod22$null.deviance-mod22$deviance)/mod22$null.deviance)
pseudoR23 <- ((mod23$null.deviance-mod23$deviance)/mod23$null.deviance)
pseudoR24 <- ((mod24$null.deviance-mod24$deviance)/mod24$null.deviance)
pseudoR25 <- ((mod25$null.deviance-mod25$deviance)/mod25$null.deviance)
pseudoR26 <- ((mod26$null.deviance-mod26$deviance)/mod26$null.deviance)
pseudoR27 <- ((mod27$null.deviance-mod27$deviance)/mod27$null.deviance)
pseudoR28 <- ((mod28$null.deviance-mod28$deviance)/mod28$null.deviance)
pseudoR29 <- ((mod29$null.deviance-mod29$deviance)/mod29$null.deviance)
pseudoR30 <- ((mod30$null.deviance-mod30$deviance)/mod30$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16,pseudoR17,pseudoR18,pseudoR19,pseudoR20,pseudoR21,pseudoR22,pseudoR23,pseudoR24,pseudoR25,pseudoR26,pseudoR27,pseudoR28,pseudoR29,pseudoR30,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR6,pseudoR7,pseudoR9,pseudoR13,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

######################################################################################################################################################
#Network level

Ext_col_data_network<-newer_pa_datas%>%
  filter(day > 3 & day < 75)%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  group_by(predator,prey,productivity,network.syn.lap,number.bottles,replicate,structure,media,year,day,volume.L,Pred.attack.rate) %>%
  summarise(prey.dens=sum(ln.prey),pred.dens=sum(ln.pred),
            total.vol=sum(volume.L),
            av.nghbr.connect=mean(nghbr.connect),
            prey.net.oc=if_else(prey.oc>0,1,0),pred.net.oc=if_else(pred.oc>0,1,0))%>%
  ungroup()%>%
  mutate(prey.unoccupied=if_else(prey.net.oc==1,0,1),prey.occupied=if_else(prey.net.oc==1,1,0),
         pred.unoccupied=if_else(pred.net.oc==1,0,1),pred.occupied=if_else(pred.net.oc==1,1,0))%>%
  group_by(structure,replicate, av.nghbr.connect,predator,prey, productivity,year,number.bottles,network.syn.lap)%>%
  summarise(n=n(),
            pred.attack=mean(Pred.attack.rate),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  #left_join(reg.all, by=c("structure","replicate", "av.nghbr.connect","predator","prey", "productivity","year"))%>%
  mutate(log.number.bottles=log(number.bottles+1))%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))%>%
  filter(av.nghbr.connect>0)


#Network Occupnacy

prey.a<-Ext_col_data_network%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(productivity),y=prey.oc,fill=as.factor(productivity)))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Productivity (g)",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.a<-Ext_col_data_network%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(productivity),y=pred.oc,fill=as.factor(productivity)))+ 
  geom_boxplot()+
  #ggtitle("e)") +
  ggtitle("f)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Productivity (g)",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

prey.atk.a<-Ext_col_data_network%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=prey.oc,fill=as.factor(predator)))+ 
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Predator Identity",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.atk.a<-Ext_col_data_network%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=pred.oc,fill=as.factor(predator)))+ 
  geom_boxplot()+
  #ggtitle("e)") +
  ggtitle("g)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Predator Identity",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

a<-Ext_col_data_network%>%
  ggplot(aes(x=log.number.bottles,y=prey.oc))+ 
  geom_point()+
  ggtitle("c)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

b<-Ext_col_data_network%>%
  ggplot(aes(x=log.network.syn.lap,y=prey.oc))+ 
  geom_point()+
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Netwokr Synchrony",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

c<-Ext_col_data_network%>%
  ggplot(aes(x=av.nghbr.connect,y=prey.oc))+ 
  geom_point()+
  #ggtitle("d)") +
  ggtitle("e)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

d<-Ext_col_data_network%>%
  ggplot(aes(x=log.number.bottles,y=pred.oc))+ 
  geom_point()+
  #ggtitle("f)") +
  ggtitle("h)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

e<-Ext_col_data_network%>%
  ggplot(aes(x=log.network.syn.lap,y=pred.oc))+ 
  geom_point()+
  ggtitle("i)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-Ext_col_data_network%>%
  ggplot(aes(x=av.nghbr.connect,y=pred.oc))+ 
  geom_point()+
  #ggtitle("h)") +
  ggtitle("j)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

plot_grid(a,b,c,d,e,f,nrow=2)
plot_grid(prey.a,a,b,c,pred.a,d,e,f,nrow=2)

plot_grid(prey.a,a,c,pred.a,d,f,nrow=2)
plot_grid(prey.a,prey.atk.a,a,b,c,pred.a,pred.atk.a,d,e,f,nrow=2)

plot_grid(prey.a,prey.atk.a,pred.a,pred.atk.a,nrow=2)

##########################################################################
#2) Regional GLMs

################################################################################################################################################
#Prey
y <- cbind(Ext_col_data_network$prey.occupany, Ext_col_data_network$prey.absence)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod2<-glm(y~(av.nghbr.connect),family=binomial(link="logit"),data=Ext_col_data_network)
mod3<-glm(y~log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod5<-glm(y~log.network.syn.lap+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod6<-glm(y~(av.nghbr.connect)+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod7<-glm(y~(av.nghbr.connect)+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+av.nghbr.connect+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
nullmod<-glm(y~1,family=binomial(link="logit"),data=Ext_col_data_network)

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

#New big Model
y <- cbind(Ext_col_data_network$prey.occupany, Ext_col_data_network$prey.absence)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod2<-glm(y~av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod3<-glm(y~log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod4<-glm(y~as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)

mod5<-glm(y~as.factor(productivity)+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod6<-glm(y~as.factor(productivity)+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod7<-glm(y~as.factor(productivity)+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod8<-glm(y~as.factor(productivity)+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod9<-glm(y~log.number.bottles+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod10<-glm(y~log.number.bottles+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod11<-glm(y~log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod12<-glm(y~av.nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod13<-glm(y~av.nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod14<-glm(y~log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)

mod15<-glm(y~as.factor(productivity)+log.number.bottles+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod16<-glm(y~as.factor(productivity)+log.number.bottles+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod17<-glm(y~as.factor(productivity)+log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod18<-glm(y~as.factor(productivity)+av.nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod19<-glm(y~as.factor(productivity)+av.nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod20<-glm(y~as.factor(productivity)+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod21<-glm(y~log.number.bottles+av.nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod22<-glm(y~log.number.bottles+av.nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod23<-glm(y~log.number.bottles+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod24<-glm(y~av.nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)

mod25<-glm(y~as.factor(productivity)+log.number.bottles+av.nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod26<-glm(y~as.factor(productivity)+log.number.bottles+av.nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod27<-glm(y~as.factor(productivity)+log.number.bottles+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod28<-glm(y~as.factor(productivity)+av.nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod29<-glm(y~log.number.bottles+av.nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)

mod30<-glm(y~as.factor(productivity)+log.number.bottles+av.nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
nullmod<-glm(y~1,family=binomial(link="logit"),data=Ext_col_data_network)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,mod19,mod20,mod21,mod22,mod23,mod24,mod25,mod26,mod27,mod28,mod29,mod30,nullmod,weights = TRUE, sort = F)
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
pseudoR15 <- ((mod15$null.deviance-mod15$deviance)/mod15$null.deviance)
pseudoR16 <- ((mod16$null.deviance-mod16$deviance)/mod16$null.deviance)
pseudoR17 <- ((mod17$null.deviance-mod17$deviance)/mod17$null.deviance)
pseudoR18 <- ((mod18$null.deviance-mod18$deviance)/mod18$null.deviance)
pseudoR19 <- ((mod19$null.deviance-mod19$deviance)/mod19$null.deviance)
pseudoR20 <- ((mod20$null.deviance-mod20$deviance)/mod20$null.deviance)
pseudoR21 <- ((mod21$null.deviance-mod21$deviance)/mod21$null.deviance)
pseudoR22 <- ((mod22$null.deviance-mod22$deviance)/mod22$null.deviance)
pseudoR23 <- ((mod23$null.deviance-mod23$deviance)/mod23$null.deviance)
pseudoR24 <- ((mod24$null.deviance-mod24$deviance)/mod24$null.deviance)
pseudoR25 <- ((mod25$null.deviance-mod25$deviance)/mod25$null.deviance)
pseudoR26 <- ((mod26$null.deviance-mod26$deviance)/mod26$null.deviance)
pseudoR27 <- ((mod27$null.deviance-mod27$deviance)/mod27$null.deviance)
pseudoR28 <- ((mod28$null.deviance-mod28$deviance)/mod28$null.deviance)
pseudoR29 <- ((mod29$null.deviance-mod29$deviance)/mod29$null.deviance)
pseudoR30 <- ((mod30$null.deviance-mod30$deviance)/mod30$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16,pseudoR17,pseudoR18,pseudoR19,pseudoR20,pseudoR21,pseudoR22,pseudoR23,pseudoR24,pseudoR25,pseudoR26,pseudoR27,pseudoR28,pseudoR29,pseudoR30,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR6,pseudoR7,pseudoR9,pseudoR13,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22


###pred 
y <- cbind(Ext_col_data_network$pred.occupany, Ext_col_data_network$pred.absence)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod2<-glm(y~(av.nghbr.connect),family=binomial(link="logit"),data=Ext_col_data_network)
mod3<-glm(y~log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod5<-glm(y~log.network.syn.lap+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod6<-glm(y~(av.nghbr.connect)+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod7<-glm(y~(av.nghbr.connect)+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+av.nghbr.connect+as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
nullmod<-glm(y~1,family=binomial(link="logit"),data=Ext_col_data_network)

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

#New Big model
y <- cbind(Ext_col_data_network$pred.occupany, Ext_col_data_network$pred.absence)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data_network)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod2<-glm(y~av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod3<-glm(y~log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod4<-glm(y~as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)

mod5<-glm(y~as.factor(productivity)+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data_network)
mod6<-glm(y~as.factor(productivity)+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod7<-glm(y~as.factor(productivity)+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod8<-glm(y~as.factor(productivity)+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod9<-glm(y~log.number.bottles+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod10<-glm(y~log.number.bottles+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod11<-glm(y~log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod12<-glm(y~av.nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod13<-glm(y~av.nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod14<-glm(y~log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)

mod15<-glm(y~as.factor(productivity)+log.number.bottles+av.nghbr.connect,family=binomial(link="logit"),data=Ext_col_data_network)
mod16<-glm(y~as.factor(productivity)+log.number.bottles+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod17<-glm(y~as.factor(productivity)+log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod18<-glm(y~as.factor(productivity)+av.nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod19<-glm(y~as.factor(productivity)+av.nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod20<-glm(y~as.factor(productivity)+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod21<-glm(y~log.number.bottles+av.nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod22<-glm(y~log.number.bottles+av.nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod23<-glm(y~log.number.bottles+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod24<-glm(y~av.nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)

mod25<-glm(y~as.factor(productivity)+log.number.bottles+av.nghbr.connect+log.network.syn.lap,family=binomial(link="logit"),data=Ext_col_data_network)
mod26<-glm(y~as.factor(productivity)+log.number.bottles+av.nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod27<-glm(y~as.factor(productivity)+log.number.bottles+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod28<-glm(y~as.factor(productivity)+av.nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
mod29<-glm(y~log.number.bottles+av.nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)

mod30<-glm(y~as.factor(productivity)+log.number.bottles+av.nghbr.connect+log.network.syn.lap+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data_network)
nullmod<-glm(y~1,family=binomial(link="logit"),data=Ext_col_data_network)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,mod19,mod20,mod21,mod22,mod23,mod24,mod25,mod26,mod27,mod28,mod29,mod30,nullmod,weights = TRUE, sort = F)
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
pseudoR15 <- ((mod15$null.deviance-mod15$deviance)/mod15$null.deviance)
pseudoR16 <- ((mod16$null.deviance-mod16$deviance)/mod16$null.deviance)
pseudoR17 <- ((mod17$null.deviance-mod17$deviance)/mod17$null.deviance)
pseudoR18 <- ((mod18$null.deviance-mod18$deviance)/mod18$null.deviance)
pseudoR19 <- ((mod19$null.deviance-mod19$deviance)/mod19$null.deviance)
pseudoR20 <- ((mod20$null.deviance-mod20$deviance)/mod20$null.deviance)
pseudoR21 <- ((mod21$null.deviance-mod21$deviance)/mod21$null.deviance)
pseudoR22 <- ((mod22$null.deviance-mod22$deviance)/mod22$null.deviance)
pseudoR23 <- ((mod23$null.deviance-mod23$deviance)/mod23$null.deviance)
pseudoR24 <- ((mod24$null.deviance-mod24$deviance)/mod24$null.deviance)
pseudoR25 <- ((mod25$null.deviance-mod25$deviance)/mod25$null.deviance)
pseudoR26 <- ((mod26$null.deviance-mod26$deviance)/mod26$null.deviance)
pseudoR27 <- ((mod27$null.deviance-mod27$deviance)/mod27$null.deviance)
pseudoR28 <- ((mod28$null.deviance-mod28$deviance)/mod28$null.deviance)
pseudoR29 <- ((mod29$null.deviance-mod29$deviance)/mod29$null.deviance)
pseudoR30 <- ((mod30$null.deviance-mod30$deviance)/mod30$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoR15,pseudoR16,pseudoR17,pseudoR18,pseudoR19,pseudoR20,pseudoR21,pseudoR22,pseudoR23,pseudoR24,pseudoR25,pseudoR26,pseudoR27,pseudoR28,pseudoR29,pseudoR30,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
#r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR6,pseudoR7,pseudoR9,pseudoR13,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22
###########################################################################################################################