####

#Total number of extinctions

loc.all%>%
  gather(pred.quasi.ext.ten,pred.quasi.ext.five,pred.quasi.ext.one,pred.ext, key = "var", value = "value") %>% 
  ggplot(aes(x=value,fill=value))+
  geom_bar()+
  scale_fill_viridis_d() +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var)


loc.all%>%
  gather(prey.quasi.ext.ten,prey.quasi.ext.five,prey.quasi.ext.one,prey.ext, key = "var", value = "value") %>% 
  ggplot(aes(x=value,fill=value))+
  geom_bar()+
  scale_fill_viridis_d() +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var)

reg.all.plot<-reg.all%>%
  dplyr::distinct(predator,prey,productivity,network.syn.lap,number.bottles,replicate,structure,media,year,total.vol,av.nghbr.connect,.keep_all=T)

reg.all.plot%>%
  gather(pred.quasi.ext.ten,pred.quasi.ext.five,pred.quasi.ext.one,pred.ext, key = "var", value = "value") %>% 
  ggplot(aes(x=value,fill=value))+
  geom_bar()+
  scale_fill_viridis_d() +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var)

reg.all.plot%>%
  gather(prey.quasi.ext.ten,prey.quasi.ext.five,prey.quasi.ext.one,prey.ext, key = "var", value = "value") %>% 
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
  filter(pred.ext=="yes")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")

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
loc.all%>%
 # filter(number.bottles !="8")%>%
  ggplot(aes(x=log(prey.den+1),y=pred.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~prod)


loc.all%>%
  #filter(number.bottles !="8")%>%
  ggplot(aes(x=log(pred.den+1),y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(~prod)

loc.all%>%
  #filter(number.bottles !="8")%>%
  ggplot(aes(x=prey.oc,y=pred.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Prey Occupancy")+ylab("Predator Persistence")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(prey~structure)
                                             
loc.all%>%
  #filter(number.bottles !="8")%>%
  ggplot(aes(x=pred.oc,y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Occupancy")+ylab("Prey Persistence")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(prey~structure)

loc.all%>%
  #filter(number.bottles !="8")%>%
  ggplot(aes(x=pred.persistence,y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  xlab("Predator Persistence")+ylab("Prey Persistence")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(prey~structure)

reg.all.plot%>%
  #filter(pred.ext=="no")%>%
  ggplot(aes(x=prey.oc,y=pred.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(as.factor(patch.degree)~prod)        

reg.all.plot%>%
  ggplot(aes(x=pred.oc,y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~prod)                                    
     
################################################################################################################################################
#Proportion of Metacommunity Extinctions

ext.prop<-loc.all%>%
  group_by(predator,prey,productivity,log.network.syn.lap,log.number.bottles,replicate,structure,media,year,log.total.vol,nghbr.connect,number.bottles) %>%
  summarize(prop.prey.met.ext=sum(prey.ext.quant)/number.bottles,prop.pred.met.ext=sum(pred.ext.quant)/number.bottles,             
            prey.ext=if_else(prey.den<=0, "yes", "no"),pred.ext=if_else(prey.den<=0, "yes", "no"))%>%
  add_column(reg.all$prey.ext)

  full_join(reg.all,by=c("predator", "prey", "replicate", "structure", "year", "productivity","log.network.syn.lap","log.number.bottles","log.total.vol","number.bottles","media"))
           
  
ext.prop%>%
  #filter(prop.pred.met.ext>0)%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prop.pred.met.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ylab("Proportion of Predator Metacommunites Extinct")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(pred.ext~var,scales="free")          

ext.prop%>%
 # filter(prop.prey.met.ext>0)%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prop.prey.met.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ylab("Proportion of Prey Metacommunites Extinct")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(prey.ext~var,scales="free")     

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

