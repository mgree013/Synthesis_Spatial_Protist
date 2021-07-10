library(cowplot)
library(tidyverse)
library(ggplot2)

#1)Ext-COlon
########################################################################################################################
Ext_col_data<-Ext_col_data%>%
  mutate(log.number.bottles=log(number.bottles+1))%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))
Ext_col_data$log.network.syn.lap

#EXT PLOTS
a<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=extinction_prob_prey))+ 
  geom_point()+
  ggtitle("a)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

b<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=extinction_prob_prey))+ 
  geom_point()+
  ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

c<-Ext_col_data%>%
  ggplot(aes(x=log.network.syn.lap,y=extinction_prob_prey))+ 
  geom_point()+
  ggtitle("c)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

d<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

e<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("e)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-Ext_col_data%>%
  ggplot(aes(x=log.network.syn.lap,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

plot_grid(a,b,c,d,e,f, nrow=2)

#COL PLOTS
a<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=colonization_prob_prey))+ 
  geom_point()+
  ggtitle("a)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

b<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=colonization_prob_prey))+ 
  geom_point()+
  ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

c<-Ext_col_data%>%
  ggplot(aes(x=log.network.syn.lap,y=colonization_prob_prey))+ 
  geom_point()+
  ggtitle("c)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

d<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=colonization_prob_pred))+ 
  geom_point()+
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

e<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=colonization_prob_pred))+ 
  geom_point()+
  ggtitle("e)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-Ext_col_data%>%
  ggplot(aes(x=log.network.syn.lap,y=colonization_prob_pred))+ 
  geom_point()+
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

plot_grid(a,b,c,d,e,f, nrow=2)

################################################################################################################################################
#MODELS
#prey colon
y <- cbind(Ext_col_data$colonization_sum_prey, Ext_col_data$colonization_potenital_prey)
str(Ext_col_data)
mod0<-glm(y~productivity,family=binomial(link = "logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod8<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect*productivity,family=binomial(link = "logit"),data=Ext_col_data)

nullmod<-glm(y~1,family=binomial(link = "logit"),data=Ext_col_data)
reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,nullmod,weights = TRUE, sort = F)

pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoR7 <- ((mod7$null.deviance-mod7$deviance)/mod7$null.deviance)
pseudoR8 <- ((mod8$null.deviance-mod8$deviance)/mod8$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###pred colon
y <- cbind(Ext_col_data$colonization_sum_pred, Ext_col_data$colonization_potenital_pred)

mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=Ext_col_data)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,nullmod,weights = TRUE, sort = F)

pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoR7 <- ((mod7$null.deviance-mod7$deviance)/mod7$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###prey ext
y <- cbind(Ext_col_data$extinction_sum_prey, Ext_col_data$extinction_potenital_prey)

mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=Ext_col_data)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,nullmod,weights = TRUE, sort = F)

pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoR7 <- ((mod7$null.deviance-mod7$deviance)/mod7$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###pred ext
y <- cbind(Ext_col_data$extinction_sum_pred, Ext_col_data$extinction_potenital_pred)

mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=Ext_col_data)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,nullmod,weights = TRUE, sort = F)

pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoR7 <- ((mod7$null.deviance-mod7$deviance)/mod7$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22
########################################################################################################################
#2)Occupnacy
occupnacy<-newer_pa_datas%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 150)%>%
  #filter(day  > 150)%>%
  mutate(prey.unoccupied=if_else(prey.oc==1,0,1),prey.occupied=if_else(prey.oc==1,1,0),
         pred.unoccupied=if_else(pred.oc==1,0,1),pred.occupied=if_else(pred.oc==1,1,0))%>%
  group_by(structure,replicate, connectivity, bottle,newID)%>%
  summarise(n=n(),
            prey.occupany =sum(prey.occupied), prey.absence=sum(prey.unoccupied),
            pred.occupany =sum(pred.occupied), pred.absence=sum(pred.unoccupied),
            prey.oc=prey.occupany/n,pred.oc=pred.occupany/n,
            prey.non.oc=prey.absence/n,pred.non.oc=pred.absence/n,
            prey=prey.absence/prey.occupany,prey1=prey.occupany/prey.absence)%>%
  left_join(all_pa_dataz, by="newID")%>%
  mutate(log.number.bottles=log(number.bottles+1))%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))%>%
  mutate(pred.size=log(Pred.size+1))%>%
  mutate(prey.size=log(Prey.size+1))


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
  labs(x="Netwokr Synchrony",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

c<-occupnacy%>%
  ggplot(aes(x=nghbr.connect,y=prey.oc))+ 
  geom_point()+
  ggtitle("c)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

d<-occupnacy%>%
  ggplot(aes(x=log.number.bottles,y=pred.oc))+ 
  geom_point()+
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
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

plot_grid(a,b,c,d,e,f,nrow=2)

occupnacy%>%
  #filter(connectivity.y >= 1)%>%
  ggplot(aes(x=pred.oc,y=prey.oc, colour=prey.y))+ 
  geom_point()+
  #ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Occupancy",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+facet_wrap(number.bottles~structure.x, scales="free") #+facet_grid(media~predator)

################################################################################################################################################
#Prey
y <- cbind(occupnacy$prey.occupany, occupnacy$n)

mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=occupnacy)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=occupnacy)
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=occupnacy)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod7<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=occupnacy)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=occupnacy)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,nullmod,weights = TRUE, sort = F)

pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoR7 <- ((mod7$null.deviance-mod7$deviance)/mod7$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###pred 
y <- cbind(occupnacy$pred.occupany, occupnacy$n)

mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=occupnacy)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=occupnacy)
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=occupnacy)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod7<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=occupnacy)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=occupnacy)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,mod5,mod6,mod7,nullmod,weights = TRUE, sort = F)

pseudoR1 <- ((mod1$null.deviance-mod1$deviance)/mod1$null.deviance)
pseudoR2 <- ((mod2$null.deviance-mod2$deviance)/mod2$null.deviance)
pseudoR3 <- ((mod3$null.deviance-mod3$deviance)/mod3$null.deviance)
pseudoR4 <- ((mod4$null.deviance-mod4$deviance)/mod4$null.deviance)
pseudoR5 <- ((mod5$null.deviance-mod5$deviance)/mod5$null.deviance)
pseudoR6 <- ((mod6$null.deviance-mod6$deviance)/mod6$null.deviance)
pseudoR7 <- ((mod7$null.deviance-mod7$deviance)/mod7$null.deviance)
pseudoRnullmod <- ((nullmod$null.deviance-nullmod$deviance)/nullmod$null.deviance)

r2<-c(pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

########################################################################################################################