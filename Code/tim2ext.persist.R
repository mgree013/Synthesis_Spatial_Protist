####

#Total number of extinctions

loc.all%>%
  gather(pred.quasi.ext.ten,pred.quasi.ext.five,pred.quasi.ext.one,pred.ext, key = "var", value = "value") %>% 
  ggplot(aes(x=value,fill=value))+
  geom_bar()+
  scale_fill_viridis_d() +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var)

p1<-loc.all%>%
  ggplot(aes(x=pred.ext,fill=pred.ext))+
  geom_bar(stat="count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  scale_fill_viridis_d() +
  xlab("Number of Local Predator Extinctions")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

p2<-loc.all%>%
  ggplot(aes(x=prey.ext,fill=prey.ext))+
  geom_bar(stat="count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  scale_fill_viridis_d() +
  xlab("Number of Local Prey Extinctions")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

p3<-reg.all%>%
  ggplot(aes(x=reg.pred.ext,fill=reg.pred.ext))+
  geom_bar(stat="count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  scale_fill_viridis_d() +
  xlab("Number of Regional Predator Extinctions")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

p4<-reg.all%>%
  ggplot(aes(x=reg.prey.ext,fill=reg.prey.ext))+
  geom_bar(stat="count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5))+
  scale_fill_viridis_d() +
  xlab("Number of Regional Prey Extinctions")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

plot_grid(p1,p2,p3,p4, nrow=1)

loc.all%>%
  gather(prey.quasi.ext.ten,prey.quasi.ext.five,prey.quasi.ext.one,prey.ext, key = "var", value = "value") %>% 
  ggplot(aes(x=value,fill=value))+
  geom_bar(stat = "identity")+
  scale_fill_viridis_d() +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var)

reg.all%>%
  gather(pred.quasi.ext.ten,pred.quasi.ext.five,pred.quasi.ext.one,reg.pred.ext, key = "var", value = "value") %>% 
  ggplot(aes(x=value,fill=value))+
  geom_bar()+
  scale_fill_viridis_d() +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var)

reg.all%>%
  gather(prey.quasi.ext.ten,prey.quasi.ext.five,prey.quasi.ext.one,reg.prey.ext, key = "var", value = "value") %>% 
  ggplot(aes(x=value,fill=value))+
  geom_bar()+
  scale_fill_viridis_d() +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var)


################################################################################################################################################
#Focus on extinctions
loc.all%>%
  filter(prey.ext=="yes")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prey.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(reg.pred.ext=="yes")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

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
  ggtitle("e)") +
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
  ggtitle("d)") +
  xlab("Nearest Neighboor Connectivity")+ ylab("Prey Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

e6<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=log.number.bottles,y=pred.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ggtitle("f)") +
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
  ggtitle("h)") +
  xlab("Nearest Neighboor Connectivity")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

plot_grid(e1,e2,e3,e4,e5,e6,e7,e8, nrow=2)




loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=nghbr.connect,y=pred.time.high.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

loc.all%>%
  filter(prey.ext=="yes")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prey.nmbr.ext.days))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(pred.ext=="yes")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.nmbr.ext.days))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(pred.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=Sum.Zero.Predator.Densities.Locally))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(prey.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(prey.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(prey.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=day.prey.min))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(pred.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=day.pred.min))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(prey.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prey.minimia))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(pred.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.minimia))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

reg.all.plot%>%
  filter(pred.ext=="yes")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,av.nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

reg.all.plot%>%
  filter(prey.ext=="yes")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,av.nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prey.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

reg.all.plot%>%
  filter(prey.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,av.nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

reg.all.plot%>%
  filter(pred.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,av.nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

reg.all.plot%>%
  #filter(prey.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,av.nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=day.prey.min))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

loc.all%>%
  filter(prey.ext=="no")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=day.prey.min))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

###Trophic
trophic%>%
 # filter(number.bottles !="8")%>%
  ggplot(aes(x=prey.oc,y=pred.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(~prod)

dog<-betareg(pred.persistence~log(prey.den+1), data=trophic)
dog1<-betareg(pred.persistence~1, data=trophic)
reported.table2<-bbmle::AICtab(dog1,dog,weights=T)
reported.table2
performance::r2(dog)

trophic%>%
  #filter(number.bottles !="8")%>%
  ggplot(aes(x=log(pred.den+1),y=log(prey.den+1)))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(predator~prod)

trophic%>%
  #filter(number.bottles !="8")%>%
  ggplot(aes(x=log(pred.den+1),y=prey.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(predator~prod)

trophic%>%
  #filter(number.bottles !="8")%>%
  ggplot(aes(x=log(prey.den+1),y=pred.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(predator~prod)

#Trophic Figs: Fig 8:
trophic<-loc.all%>%filter(pred.persistence>0)%>%filter(pred.persistence<1)%>%
  filter(prey.persistence>0)%>%filter(prey.persistence<1)%>%
  filter(prey.oc>0)%>%filter(prey.oc<1)%>%filter(pred.oc>0)%>%filter(pred.oc<1)

dog<-betareg(pred.persistence~prey.oc, data=trophic)
dog1<-betareg(pred.persistence~1, data=trophic)
reported.table2<-bbmle::AICtab(dog1,dog,weights=T)
reported.table2
performance::r2(dog)
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)

p0<-trophic%>%
  ggplot(aes(x=prey.oc,y=pred.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ggtitle("a)") +
  annotate("text", x = 0.25, y = .95, label = "R^2 == 0.09", parse = TRUE) +
  xlab("Prey Occupancy")+ylab("Predator Persistence")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(prey~predator)

dog<-betareg(prey.persistence~pred.oc, data=trophic)
dog1<-betareg(prey.persistence~1, data=trophic)
reported.table2<-bbmle::AICtab(dog1,dog,weights=T)
reported.table2
performance::r2(dog)

p1<-trophic%>%
  ggplot(aes(x=pred.oc,y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ggtitle("b)") +
  annotate("text", x = 0.15, y = .05, label = "R^2 == 0.16", parse = TRUE) +
  xlab("Predator Occupancy")+ylab("Prey Persistence")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(prey~structure)

dog<-betareg(prey.persistence~pred.persistence, data=trophic)
dog1<-betareg(prey.persistence~1, data=trophic)
reported.table2<-bbmle::AICtab(dog1,dog,weights=T)
reported.table2
performance::r2(dog)

p2<-trophic%>%
  ggplot(aes(x=pred.persistence,y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ggtitle("c)") +
  annotate("text", x = 0.10, y = .05, label = "R^2 == 0.13", parse = TRUE) +
  xlab("Predator Persistence")+ylab("Prey Persistence")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

plot_grid(p0,p1,p2,nrow=1)

reg.all%>%
  #filter(pred.ext=="no")%>%
  ggplot(aes(x=prey.oc,y=pred.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(as.factor(patch.degree)~prod)        

reg.all%>%
  ggplot(aes(x=pred.oc,y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(~prod)                                    
     
################################################################################################################################################
#Proportion of Metacommunity Extinctions


#Focus on Regional Ext, Local responses
ext.prop<-loc.all

loc.all%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prey.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ylab("Proportion of Predator Metacommunites Extinct")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(reg.prey.ext~var,scales="free")          

loc.all%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.oc))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ylab("Proportion of Pred Metacommunites Extinct")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(reg.pred.ext~var,scales="free")     

ext.prop%>%
  ggplot(aes(x=as.factor(productivity),y=prop.prey.met.ext, fill=as.factor(productivity)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ylab("Proportion of Prey Metacommunites Extinct")+xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

ext.prop%>%
  ggplot(aes(x=as.factor(productivity),y=prop.pred.met.ext, fill=as.factor(productivity)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ylab("Proportion of Predator Metacommunites Extinct")+xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

