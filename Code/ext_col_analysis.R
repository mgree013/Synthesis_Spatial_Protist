#Part 2: Ext-Col Analysis

#Local and Regional GLMS

Datazz<-Data%>%
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

#EXT PLOTS
a<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=extinction_prob_prey))+ 
  geom_point()+
  ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

b<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=extinction_prob_prey))+ 
  geom_point()+
  ggtitle("d)") +
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
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

e<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("h)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-Ext_col_data%>%
  ggplot(aes(x=log.network.syn.lap,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("g)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

plot_grid(a,b,c,d,e,f, nrow=2)
plot_grid(prey.ext.plot,a,c,b,pred.ext.plot,d,f,e, nrow=2)

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

pred.col.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=colonization_prob_pred,fill=as.factor(productivity)))+ 
  geom_boxplot()+
  ggtitle("e)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Productivity",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

prey.col.plot<-Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=colonization_prob_prey, fill=as.factor(productivity)))+ 
  geom_boxplot()+
  ggtitle("a)") +
  scale_fill_viridis(discrete=T)+
  labs(x="Productivity",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

#COL PLOTS
a<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=colonization_prob_prey))+ 
  geom_point()+
  ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

b<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=colonization_prob_prey))+ 
  geom_point()+
  ggtitle("d)") +
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
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

e<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=colonization_prob_pred))+ 
  geom_point()+
  ggtitle("h)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-Ext_col_data%>%
  ggplot(aes(x=log.network.syn.lap,y=colonization_prob_pred))+ 
  geom_point()+
  ggtitle("g)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

plot_grid(a,b,c,d,e,f, nrow=2)
plot_grid(prey.col.plot,a,c,b,pred.col.plot,d,f,e, nrow=2)

Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=colonization_prob_pred, fill=as.factor(productivity)))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Productivity (g)",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=colonization_prob_prey, fill=as.factor(productivity)))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Productivity (g)",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=extinction_prob_pred, fill=as.factor(productivity)))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Productivity (g)",y="Pred Ext. Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

Ext_col_data%>%
  ggplot(aes(x=as.factor(productivity),y=extinction_prob_prey, fill=as.factor(productivity)))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Productivity (g)",y="Prey Ext. Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")
################################################################################################################################################
################################################################################################################################################
#MODELS
#prey colon
y <- cbind(Ext_col_data$colonization_sum_pred, Ext_col_data$colonization_potenital_pred)

mod0<-glm(y~as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)+log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~(nghbr.connect)+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=Ext_col_data)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
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

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###pred colon
y <- cbind(Ext_col_data$colonization_sum_pred, Ext_col_data$colonization_potenital_pred)

mod0<-glm(y~as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)+log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~(nghbr.connect)+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=Ext_col_data)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
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

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###prey ext
y <- cbind(Ext_col_data$extinction_sum_prey, Ext_col_data$extinction_potenital_prey)

mod0<-glm(y~as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)+log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~(nghbr.connect)+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=Ext_col_data)

summary(mod14)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
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

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22

###pred ext
y <- cbind(Ext_col_data$extinction_sum_pred, Ext_col_data$extinction_potenital_pred)

mod0<-glm(y~as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap+log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)+log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~(nghbr.connect)+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod8<-glm(y~(log.network.syn.lap)+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod9<-glm(y~log.number.bottles+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod10<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod11<-glm(y~log.network.syn.lap+log.number.bottles+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod12<-glm(y~log.network.syn.lap+as.factor(productivity)+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles+as.factor(productivity)+nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod14<-glm(y~log.network.syn.lap+log.number.bottles+nghbr.connect+as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=Ext_col_data)

reported.table2 <- bbmle::AICtab(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,nullmod,weights = TRUE, sort = F)
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

r2<-c(pseudoR0,pseudoR1,pseudoR2,pseudoR3,pseudoR4,pseudoR5,pseudoR6,pseudoR7,pseudoR8,pseudoR9,pseudoR10,pseudoR11,pseudoR12,pseudoR13,pseudoR14,pseudoRnullmod)
r22<-as.data.frame(r2, ncol=1)
r22
########################################################################################################################

#Gilarranz, L. J., and J. Bascompte. 2012. 
#https://www.sciencedirect.com/science/article/pii/S0022519311005972



Ext_col_data%>%
  filter(pred.occupancy<1)%>%
  filter(number.bottles>5)%>%
  filter(ext_colon_ratio_pred< 20)%>%
  ggplot(aes(x=ext_colon_ratio_pred, y=pred.occupancy,shape=prey,colour=as.factor(structure)))+
  geom_point()+
  geom_smooth( method="loess")+
  scale_color_viridis_d()+
  ylab("Average Predator Occupancy")+xlab("Predator Extinction to Colonization Ratio")+
  #facet_grid(prey~structure)+
  ylim(0,1)+
  theme_bw()+theme_bw()+ theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_rect(colour = "black"))#+theme(legend.position = "none")

Ext_col_data%>%
  filter(prey.occupancy<1)%>%
  filter(number.bottles>5)%>%
  filter(ext_colon_ratio_prey< 6.5)%>%
  ggplot(aes(x=ext_colon_ratio_prey, y=prey.occupancy, shape=prey,colour=as.factor(structure)))+
  geom_point()+
  geom_smooth(method = "loess")+  scale_color_viridis_d()+
  ylim(0,1)+
  #facet_grid(prey~structure)+
  ylab("Average Prey Occupancy")+xlab("Prey Extinction to Colonization Ratio")+
  theme_bw()+theme_bw()+ theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_rect(colour = "black"))#+theme(legend.position = "none")


Ext_col_data%>%
  filter(nghbr.connect>0)%>%
  ggplot(aes(x=nghbr.connect, y=prey.occupancy))+#, colour=as.factor(connectivity)))+
  geom_point()+
  #geom_smooth(method = "loess")+  scale_color_viridis_d()+
  stat_smooth(method = "lm", formula = y ~ sqrt(x), size = 1)+
  ylim(0,1)+
  #facet_grid(prey~structure)+
  ylab("Average Prey Occupancy")+xlab("Average Nearest Neighboors Connectivity")+
  theme_bw()+theme_bw()+ theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_rect(colour = "black"))#+theme(legend.position = "none")

Ext_col_data%>%
  filter(nghbr.connect>0)%>%
  ggplot(aes(x=nghbr.connect, y=pred.occupancy))+#, colour=as.factor(connectivity)))+
  geom_point()+
  #geom_smooth(method="lm", formula= (y ~ exp(x)), se=FALSE, linetype = 1) +
  #geom_smooth(method = "loess")+  scale_color_viridis_d()+
  #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
  stat_smooth(method = "lm", formula = y ~ sqrt(x), size = 1)+
  ylim(0,1)+
  #facet_grid(prey~structure)+
  ylab("Average Predator Occupancy")+xlab("Average Nearest Neighboors Connectivity")+
  theme_bw()+theme_bw()+ theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_rect(colour = "black"))#+theme(legend.position = "none")


net_Ext_col_data<-Ext_col_data%>%
  filter(number.bottles>1)%>%
  group_by(predator,prey,productivity,network.syn.lap,number.bottles,replicate,structure,media,year)%>%
  summarise(av.ext_colon_ratio_pred=mean(ext_colon_ratio_pred),av.ext_colon_ratio_prey=mean(ext_colon_ratio_prey),
            av.nghbr.connect=mean(nghbr.connect),
            av.connect=mean(connectivity),
            mean.prey.oc=mean(prey.occupancy),mean.pred.oc=mean(pred.occupancy),
            total.vol=sum(volume.L))


net_Ext_col_data%>%
  ggplot(aes(x=av.nghbr.connect, y=mean.prey.oc))+#, colour=as.factor(connectivity)))+
  geom_point()+
  #geom_smooth(method = "loess")+  scale_color_viridis_d()+
  stat_smooth(method = "lm", formula = y ~ sqrt(x), size = 1)+
  ylim(0,1)+
  #facet_grid(prey~structure)+
  ylab("Average Prey Occupancy")+xlab("Average Nearest Neighboors Connectivity")+
  theme_bw()+theme_bw()+ theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_rect(colour = "black"))#+theme(legend.position = "none")

net_Ext_col_data%>%
  ggplot(aes(x=av.nghbr.connect, y=mean.pred.oc)) +#, colour=as.factor(av.connect)))+
  geom_point()+
  #geom_smooth(method="lm", formula= (y ~ exp(x)), se=FALSE, linetype = 1) +
 # geom_smooth(method = "loess")+  
  scale_color_viridis_d()+
  #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
  stat_smooth(method = "glm", formula = y ~ sqrt(x), size = 1)+
  ylim(0,1)+
  #facet_grid(prey~structure)+
  ylab("Average Predator Occupancy")+xlab("Average Nearest Neighboors Connectivity")+
  theme_bw()+theme_bw()+ theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_rect(colour = "black"))#+theme(legend.position = "none")
