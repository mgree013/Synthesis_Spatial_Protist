#Predicted Occupnacy

#1)Local Scale
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
  filter(day < 75)%>%
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

pred_Ext_col_data<-Ext_col_data%>%
  left_join(occupnacy, by="newID")%>%
  filter(pred.pred.oc>0)%>%
  filter(pred.prey.oc>0)%>%
  filter(pred.pred.oc<1)%>%
  filter(pred.prey.oc<1)%>%
  filter(pred.oc>0)%>%
  filter(prey.oc>0)%>%
  filter(pred.oc<1)%>%
  filter(prey.oc<1)%>%
  filter(colonization_prob_pred>0)%>%
  filter(colonization_prob_pred<1)%>%
  filter(colonization_prob_prey>0)%>%
  filter(colonization_prob_prey<1)%>%
  filter(extinction_prob_pred>0)%>%
  filter(extinction_prob_pred<1)%>%
  filter(extinction_prob_pred>0)%>%
  filter(extinction_prob_prey<1)

dog<-betareg(pred.prey.oc~prey.oc, data=pred_Ext_col_data)
dog1<-betareg(pred.prey.oc~1, data=pred_Ext_col_data)
reported.table2<-bbmle::AICtab(dog1,dog,weights = TRUE, sort = F)
reported.table2
performance::r2(dog)
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)

preda<-pred_Ext_col_data%>%
  ggplot(aes(x=prey.oc,y=pred.prey.oc))+ 
  geom_point()+
  ggtitle("c)") +
  #geom_smooth(method = "lm",se=F)+
  annotate("text", x = 0.4, y = .95, label = "R^2 == 0.80", parse = TRUE) +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Observed Occupancy",y="Prey Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")


dog<-betareg(pred.pred.oc~pred.oc, data=pred_Ext_col_data)
dog1<-betareg(pred.prey.oc~1, data=pred_Ext_col_data)
reported.table2<-bbmle::AICtab(dog1,dog,weights=T)
reported.table2
performance::r2(dog)
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)

predb<-pred_Ext_col_data%>%
  ggplot(aes(x=pred.oc,y=pred.pred.oc))+ 
  geom_point()+
  ggtitle("d)") +
  #geom_smooth(method = "lm",se=F)+
  annotate("text", x = 0.4, y = .95, label = "R^2 == 0.91", parse = TRUE) +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Observed Occupancy",y="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")


dog<-betareg(colonization_prob_prey~prey.oc, data=pred_Ext_col_data)
dog1<-betareg(colonization_prob_prey~1, data=pred_Ext_col_data)
reported.table2<-bbmle::AICtab(dog1,dog,weights=T)
reported.table2
performance::r2(dog)
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)

predc<-pred_Ext_col_data%>%
  ggplot(aes(x=prey.oc,y=colonization_prob_prey))+ 
  geom_point()+
  ggtitle("e)") +
  #geom_smooth(method = "lm",se=F)+
  annotate("text", x = 0.4, y = .95, label = "R^2 == 0.34", parse = TRUE) +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Observed Occupancy",y="Prey Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

dog<-betareg(colonization_prob_pred~pred.oc, data=pred_Ext_col_data)
dog1<-betareg(colonization_prob_pred~1, data=pred_Ext_col_data)
reported.table2<-bbmle::AICtab(dog1,dog)
reported.table2
performance::r2(dog)
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)

predd<-pred_Ext_col_data%>%
  ggplot(aes(x=pred.oc,y=colonization_prob_pred))+ 
  geom_point()+
  ggtitle("f)") +
  #geom_smooth(method = "lm",se=F)+
  annotate("text", x = 0.4, y = .95, label = "R^2 == 0.84", parse = TRUE) +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Observed Occupancy",y="Predator Colonization Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")


dog<-betareg(extinction_prob_prey~prey.oc, data=pred_Ext_col_data)
dog1<-betareg(extinction_prob_prey~1, data=pred_Ext_col_data)
reported.table2<-bbmle::AICtab(dog1,dog,weights=T)
reported.table2
performance::r2(dog)
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)

prede<-pred_Ext_col_data%>%
  ggplot(aes(x=prey.oc,y=extinction_prob_prey))+ 
  geom_point()+
  ggtitle("g)") +
  #geom_smooth(method = "lm",se=F)+
  annotate("text", x = 0.4, y = .95, label = "R^2 == 0.58", parse = TRUE) +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Observed Occupancy",y="Prey Extinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

dog<-betareg(extinction_prob_pred~pred.oc, data=pred_Ext_col_data)
dog1<-betareg(extinction_prob_pred~1, data=pred_Ext_col_data)
reported.table2<-bbmle::AICtab(dog1,dog,weights=T)
reported.table2
performance::r2(dog)
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)

predf<-pred_Ext_col_data%>%
  ggplot(aes(x=pred.oc,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("h)") +
  #geom_smooth(method = "lm",se=F)+
  annotate("text", x = 0.4, y = .95, label = "R^2 == 0.55", parse = TRUE) +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Observed Occupancy",y="Predator Extinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")
#plot_grid(preda,predb)

################################################################################################################################################
#2) Network level
pred_network<-newer_pa_datas%>%
  filter(structure !="control")%>%
  rename(lag.pred.oc = `pred.oc-1`,lag.prey.oc = `prey.oc-1`)%>%
  mutate(
    colonization_col_pred=if_else(pred.oc==1 & lag.pred.oc==0 ,1,0),
    non_colonization_col_pred=if_else(pred.oc==0 & lag.pred.oc==0 ,1,0), 
    extinction_col_pred=if_else(pred.oc==0 & lag.pred.oc==1 ,1,0),
    non_extinction_col_pred=if_else(pred.oc==1 & lag.pred.oc==1 ,1,0),
    colonization_col_prey=if_else(prey.oc==1 & lag.prey.oc==0 ,1,0), 
    non_colonization_col_prey=if_else(prey.oc==0 & lag.prey.oc==0 ,1,0), 
    extinction_col_prey=if_else(prey.oc==0 & lag.prey.oc== 1, 1,0),
    non_extinction_col_prey=if_else(prey.oc==1 & lag.prey.oc== 1, 1,0))%>%
  replace(is.na(.), 0)%>%
  group_by(predator,prey,productivity,network.syn.lap,number.bottles,replicate,structure,media,year,lambda_m)%>%
  summarize(
    colonization_sum_pred=sum(colonization_col_pred),non_colonization_sum_pred=sum(non_colonization_col_pred),extinction_sum_pred=sum(extinction_col_pred),non_extinction_sum_pred=sum(non_extinction_col_pred),
    colonization_potenital_pred=sum(colonization_col_pred+non_colonization_col_pred),colonization_potenital_prey=sum(colonization_col_prey+non_colonization_col_prey),
    extinction_potenital_pred=sum(extinction_col_pred+non_extinction_col_pred),extinction_potenital_prey=sum(extinction_col_prey+non_extinction_col_prey),
    colonization_sum_prey=sum(colonization_col_prey),non_colonization_sum_prey=sum(non_colonization_col_prey),extinction_sum_prey=sum(extinction_col_prey),non_extinction_sum_prey=sum(non_extinction_col_prey),
    colonization_prob_pred=colonization_sum_pred/colonization_potenital_pred, extinction_prob_pred=extinction_sum_pred/extinction_potenital_pred,
    colonization_prob_prey=colonization_sum_prey/colonization_potenital_prey, extinction_prob_prey=extinction_sum_prey/extinction_potenital_prey,
    ext_colon_ratio_pred=(extinction_prob_pred/colonization_prob_pred),ext_colon_ratio_prey=(extinction_prob_prey/colonization_prob_prey),
    pred.prey.oc=1-((extinction_prob_prey/colonization_prob_prey)/lambda_m),pred.pred.oc=1-((extinction_prob_pred/colonization_prob_pred)/lambda_m))%>%
  left_join(reg.all, by=c("predator","prey","productivity","network.syn.lap","number.bottles","replicate","structure","media","year"))%>%
  distinct(structure,replicate, .keep_all = T)#%>%
#filter(pred.prey.oc>0 & pred.prey.oc<1)%>%
#filter(pred.pred.oc>0 & pred.pred.oc<1)



#Predicted Occupnacy
preda_net<-pred_network%>%
  filter(pred.prey.oc>0)%>%
  ggplot(aes(x=prey.oc,y=pred.prey.oc))+ 
  geom_point()+
  ggtitle("a)") +
  #geom_smooth(method = "lm",se=F)+
  annotate("text", x = 0.4, y = .95, label = "R^2 == 0.67", parse = TRUE) +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Observed Occupancy",y="Prey Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

predb_net<-pred_network%>%
  filter(pred.pred.oc>0)%>%
  ggplot(aes(x=pred.oc,y=pred.pred.oc))+ 
  geom_point()+
  ggtitle("b)") +
  #geom_smooth(method = "lm",se=F)+
  annotate("text", x = 0.4, y = .95, label = "R^2 == 0.64", parse = TRUE) +
  stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(x="Predator Observed Occupancy",y="Predator Predicted Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

plot_grid(preda_net,predb_net)

Ext_col_data_network_analysis<-pred_network%>%
  #filter(pred.prey.oc>0 & pred.prey.oc<1)%>%
  #filter(prey.oc>0 & prey.oc<1)
  filter(pred.pred.oc>0 & pred.pred.oc<1)
  
dog<-betareg(pred.prey.oc~prey.oc, data=Ext_col_data_network_analysis)
dog1<-betareg(pred.prey.oc~1, data=Ext_col_data_network_analysis)
reported.table2<-bbmle::AICtab(dog1,dog)
reported.table2
performance::r2(dog)
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)

dog<-betareg(pred.pred.oc~pred.oc, data=Ext_col_data_network_analysis)
dog1<-betareg(pred.pred.oc~1, data=Ext_col_data_network_analysis)
reported.table2<-bbmle::AICtab(dog1,dog)
reported.table2
performance::r2(dog)
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)

#Predicted prey and predator occupancy at network level and local  scales
