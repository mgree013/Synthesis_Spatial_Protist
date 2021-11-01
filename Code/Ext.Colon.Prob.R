library(DataCombine)
library(tidyverse)
library(ggplot2)
library(viridis)

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


##########################################################################################################################################################
Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey, key = "var", value = "value")%>%
  ggplot(aes(x=prey.occupancy, y=value, colour=structure))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  facet_grid(structure~var, scales = "free")

Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey, key = "var", value = "value")%>%
  ggplot(aes(x=pred.occupancy, y=value, colour=structure))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  facet_grid(structure~var, scales = "free")+
  theme_bw()+theme_bw()+ theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

Ext_col_data%>%
  #filter(structure !="isolated")%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey, key = "var", value = "value")%>%
  ggplot(aes(x=pred.occupancy, y=value))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  facet_grid(~var, scales = "free")+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")
Ext_col_data%>%
  #filter(structure !="isolated")%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey, key = "var", value = "value")%>%
  ggplot(aes(x=prey.occupancy, y=value))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  facet_grid(~var, scales = "free")+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")
Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=structure, y=value, fill=structure))+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var, scales = "free")+theme_bw()+ theme(panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(),
                                                      panel.border = element_rect(colour = "black"))+theme(legend.position = "none")


Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=as.factor(connectivity), y=value, fill=var))+
  xlab("Conenctivity")+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var)+theme_bw()+ theme(panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=(nghbr.connect), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  xlab("Nearest Neighboor Connectivity")+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var)+theme_bw()+ theme(panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=(connectivity), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var)theme_bw()+ theme(panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=log(number.bottles), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var)+theme_bw()+ theme(panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

Ext_col_data%>%
  gather(colonization_prob_pred,colonization_prob_prey,extinction_prob_pred,extinction_prob_prey,key = "var", value = "value")%>%
  ggplot(aes(x=log(network.syn.lap+1), y=value, colour=var))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_d()+
  scale_fill_viridis(discrete = TRUE)+
  facet_grid(~var)+theme_bw()+ theme(panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.border = element_rect(colour = "black"))+theme(legend.position = "none")


######
Ext_col_data<-Ext_col_data%>%
  mutate(log.number.bottles=log(number.bottles+1))%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))
Ext_col_data$log.network.syn.lap

#prey colon
y <- cbind(Ext_col_data$colonization_sum_prey, Ext_col_data$colonization_potenital_prey)

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
