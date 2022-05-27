#Part 2: Ext-Col Analysis

Datazz<-Data%>%
  #filter(year > 2010)%>%
  unite("newID", media:predator,bottle, remove=FALSE)

all_pa_dataz<-Datazz%>%
  group_by(newID,volume.L,structure,replicate,bottle,productivity,bottle.number,connectivity,predator,prey,network.syn.lap,media,year, number.bottles,nghbr.connect, Prey.disp.rate,Prey.growth.rate,Prey.K.cap,Pred.attack.rate,Pred.size,Prey.size)%>%
  summarise(prey.occupancy =mean(prey.oc), pred.occupancy= mean(pred.oc))

new_pa_datas <- slide(Datazz, Var = "pred.oc", GroupVar = "newID",
                      slideBy = -1)

newer_pa_datas <- slide(new_pa_datas, Var = "prey.oc", GroupVar = "newID",
                        slideBy = -1)

Ext_col_data<-newer_pa_datas%>%
  #filter(structure=="isolated")%>%
  #filter(day >0)%>%
  #filter(day < 75)%>%
  #filter(day  > 75 & day < 175)%>%
  #filter(day  > 175)%>%
  ungroup()%>%
  rename(lag.pred.oc = `pred.oc-1`)%>%
  rename(lag.prey.oc = `prey.oc-1`)%>%
  replace(is.na(.), 7)%>%
  mutate(
    colonization_col_pred=if_else(pred.oc==1 & lag.pred.oc==0 ,1,0),
    non_colonization_col_pred=if_else(pred.oc==0 & lag.pred.oc==0 ,1,0), 
    extinction_col_pred=if_else(pred.oc==0 & lag.pred.oc==1 ,1,0),
    non_extinction_col_pred=if_else(pred.oc==1 & lag.pred.oc==1 ,1,0),
    colonization_col_prey=if_else(prey.oc==1 & lag.prey.oc==0 ,1,0), 
    non_colonization_col_prey=if_else(prey.oc==0 & lag.prey.oc==0 ,1,0), 
    extinction_col_prey=if_else(prey.oc==0 & lag.prey.oc== 1, 1,0),
    non_extinction_col_prey=if_else(prey.oc==1 & lag.prey.oc== 1, 1,0))%>%
  #ungroup()%>%
  replace(is.na(.), 0)%>%
  group_by(newID)%>%
  summarize(#prey.occupancy =sum(prey.oc)/sampling_days, pred.occupancy= sum(pred.oc)/sampling_days,
    pred.attack=mean(Pred.attack.rate),
    colonization_sum_pred=sum(colonization_col_pred),non_colonization_sum_pred=sum(non_colonization_col_pred),extinction_sum_pred=sum(extinction_col_pred),non_extinction_sum_pred=sum(non_extinction_col_pred),
    colonization_potenital_pred=sum(colonization_col_pred+non_colonization_col_pred),colonization_potenital_prey=sum(colonization_col_prey+non_colonization_col_prey),
    extinction_potenital_pred=sum(extinction_col_pred+non_extinction_col_pred),extinction_potenital_prey=sum(extinction_col_prey+non_extinction_col_prey),
    colonization_sum_prey=sum(colonization_col_prey),non_colonization_sum_prey=sum(non_colonization_col_prey),extinction_sum_prey=sum(extinction_col_prey),non_extinction_sum_prey=sum(non_extinction_col_prey),
    colonization_prob_pred=sum(colonization_col_pred)/sum(colonization_col_pred+non_colonization_col_pred), extinction_prob_pred=sum(extinction_col_pred)/sum(extinction_col_pred+non_extinction_col_pred),
    colonization_prob_prey=sum(colonization_col_prey)/sum(colonization_col_prey+non_colonization_col_prey), extinction_prob_prey=sum(extinction_col_prey)/sum(extinction_col_prey+non_extinction_col_prey),
    ext_colon_ratio_pred=extinction_prob_pred/colonization_prob_pred,ext_colon_ratio_prey=extinction_prob_prey/colonization_prob_prey,
    pred.prey.oc=colonization_prob_prey/(extinction_prob_prey+colonization_prob_prey),pred.pred.oc=colonization_prob_pred/(extinction_prob_pred+colonization_prob_pred))%>%
  left_join(all_pa_dataz, by="newID")

Ext_col_data<-Ext_col_data%>%
  mutate(log.number.bottles=log(number.bottles+1))%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))%>%
  filter(bottle.number>1)%>%
  filter(nghbr.connect>0)
Ext_col_data$log.network.syn.lap

Ext_col_data<-Ext_col_data%>%
  mutate(Local_volume=if_else(year=="2010" , "32mL",if_else(year =="1996" ,"32mL","50mL")))

supp.b<-Ext_col_data%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(Local_volume),y=extinction_prob_prey, fill=as.factor(Local_volume)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  #ggtitle("e)") +
  ggtitle("b)") +
  ylab("Extinction Probability of Prey")+
  xlab("Local Volume Size")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

plot_grid(supp.a, supp.b)

dog<-aov(extinction_prob_prey~as.factor(Local_volume),Ext_col_data)
summary(dog)

Ext_col_data%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(Local_volume),y=extinction_prob_pred, fill=as.factor(Local_volume)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  #ggtitle("e)") +
  ggtitle("a)") +
  ylab("Prey Occupancy")+
  xlab("Local Volume Size")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

dog<-aov(extinction_prob_pred~as.factor(Local_volume),Ext_col_data)
summary(dog)

Ext_col_data%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(Local_volume),y=colonization_prob_prey, fill=as.factor(Local_volume)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  #ggtitle("e)") +
  ggtitle("a)") +
  ylab("Prey Occupancy")+
  xlab("Local Volume Size")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

dog<-aov(colonization_prob_prey~as.factor(Local_volume),Ext_col_data)
summary(dog)

Ext_col_data%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(Local_volume),y=colonization_prob_pred, fill=as.factor(Local_volume)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  #ggtitle("e)") +
  ggtitle("a)") +
  ylab("Prey Occupancy")+
  xlab("Local Volume Size")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

dog<-aov(colonization_prob_pred~as.factor(Local_volume),Ext_col_data)
summary(dog)

#EXT PLOTS
a<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=extinction_prob_prey,weight=extinction_potenital_prey))+ 
  geom_point()+
  ggtitle("c)") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=FALSE) +
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

c<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=extinction_prob_prey,weight=extinction_potenital_prey))+ 
  geom_point()+
  ggtitle("d)") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=FALSE) +
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

d<-Ext_col_data%>% #prob
  ggplot(aes(x=log.number.bottles,y=extinction_prob_pred,weight=extinction_potenital_pred))+ 
  geom_point()+
  ggtitle("g)") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=FALSE) +
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=extinction_prob_pred,weight=extinction_potenital_pred))+ 
  geom_point()+
  ggtitle("h)") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=FALSE) +
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

pred.ext.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=extinction_prob_pred,fill=as.factor(productivity)))+ 
  geom_boxplot()+
  ggtitle("e)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Productivity",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

prey.ext.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=extinction_prob_prey,fill=as.factor(productivity)))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Productivity",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.ext.atk.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(predator),y=extinction_prob_pred,fill=as.factor(predator)))+ 
  geom_boxplot()+
  ggtitle("f)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Predator Identity",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

prey.ext.atk.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(predator),y=extinction_prob_prey,fill=as.factor(predator)))+ 
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Predator Identity",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

plot_grid(prey.ext.plot,prey.ext.atk.plot,a,c,pred.ext.plot,pred.ext.atk.plot,d,f, nrow=2)

#Colonization Plots

pred.col.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=colonization_prob_pred,fill=as.factor(productivity)))+ 
  geom_boxplot()+
  ggtitle("e)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Productivity",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

prey.col.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=colonization_prob_prey, fill=as.factor(productivity)))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Productivity",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

pred.col.atk.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(predator),y=colonization_prob_pred,fill=as.factor(predator)))+ 
  geom_boxplot()+
  ggtitle("f)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Predator Identity",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

prey.col.atk.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(predator),y=colonization_prob_prey, fill=as.factor(predator)))+ 
  geom_boxplot()+
  ggtitle("b)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Predator Identity",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

#COL PLOTS
a<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=colonization_prob_prey,weight=colonization_potenital_prey))+ 
  geom_point()+
  ggtitle("c)") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=FALSE) +
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

c<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=colonization_prob_prey,weight=colonization_potenital_prey))+ 
  geom_point()+
  ggtitle("d)") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=FALSE) +
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

d<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=colonization_prob_pred))+#,weight=colonization_potenital_pred))+ 
  geom_point()+
  ggtitle("g)") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=FALSE) +
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=colonization_prob_pred))+#,weight=colonization_potenital_pred))+ 
  geom_point()+
  ggtitle("h)") +
  stat_smooth(method="glm", method.args=list(family=binomial(link = "logit")), se=FALSE) +
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

plot_grid(prey.col.plot,prey.col.atk.plot,a,c,pred.col.plot,pred.col.atk.plot,d,f, nrow=2)

################################################################################################################################################
################################################################################################################################################
#MODELS
#prey colon
y <- cbind(Ext_col_data$colonization_sum_prey, Ext_col_data$colonization_potenital_prey)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=Ext_col_data)
mod2<-glm(y~nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod3<-glm(y~as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod4<-glm(y~as.factor(productivity)+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data)
mod5<-glm(y~as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod6<-glm(y~as.factor(productivity)+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod7<-glm(y~log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod8<-glm(y~log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod9<-glm(y~nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod10<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod11<-glm(y~as.factor(productivity)+log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod12<-glm(y~as.factor(productivity)+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod14<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

nullmod<-glm(y~1,family=binomial(link="logit"),data=Ext_col_data)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
reported.table2

plot_model(mod14)
check_collinearity(mod14)
summary(mod14)
summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)

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

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###pred colon
y <- cbind(Ext_col_data$colonization_sum_pred, Ext_col_data$colonization_potenital_pred)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=Ext_col_data)
mod2<-glm(y~nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod3<-glm(y~as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod4<-glm(y~as.factor(productivity)+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data)
mod5<-glm(y~as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod6<-glm(y~as.factor(productivity)+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod7<-glm(y~log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod8<-glm(y~log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod9<-glm(y~nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod10<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod11<-glm(y~as.factor(productivity)+log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod12<-glm(y~as.factor(productivity)+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod14<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

nullmod<-glm(y~1,family=binomial(link="logit"),data=Ext_col_data)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
reported.table2

plot_model(mod14)
summary(mod14)
summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)

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

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###prey ext
y <- cbind(Ext_col_data$extinction_sum_prey, Ext_col_data$extinction_potenital_prey)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=Ext_col_data)
mod2<-glm(y~nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod3<-glm(y~as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod4<-glm(y~as.factor(productivity)+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data)
mod5<-glm(y~as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod6<-glm(y~as.factor(productivity)+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod7<-glm(y~log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod8<-glm(y~log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod9<-glm(y~nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod10<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod11<-glm(y~as.factor(productivity)+log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod12<-glm(y~as.factor(productivity)+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod14<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

nullmod<-glm(y~1,family=binomial(link="logit"),data=Ext_col_data)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
reported.table2

plot_model(mod14)
check_collinearity(mod14)
summary(mod14)
summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)

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

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###pred ext
y <- cbind(Ext_col_data$extinction_sum_pred, Ext_col_data$extinction_potenital_pred)

mod0<-glm(y~as.factor(productivity),family=binomial(link="logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link="logit"),data=Ext_col_data)
mod2<-glm(y~nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod3<-glm(y~as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod4<-glm(y~as.factor(productivity)+log.number.bottles,family=binomial(link="logit"),data=Ext_col_data)
mod5<-glm(y~as.factor(productivity)+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod6<-glm(y~as.factor(productivity)+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod7<-glm(y~log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod8<-glm(y~log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod9<-glm(y~nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod10<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect,family=binomial(link="logit"),data=Ext_col_data)
mod11<-glm(y~as.factor(productivity)+log.number.bottles+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod12<-glm(y~as.factor(productivity)+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

mod14<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=Ext_col_data)

nullmod<-glm(y~1,family=binomial(link="logit"),data=Ext_col_data)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
reported.table2

plot_model(mod14)
check_collinearity(mod14)
summary(mod14)
summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)

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

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

########################################################################################################################

