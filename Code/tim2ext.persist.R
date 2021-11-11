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
  #filter(pred.quasi.ext.five=="yes")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.time.2.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(prey~var,scales="free")

loc.all%>%
  filter(pred.ext=="yes")%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=pred.time.2.ext))+
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
  ggplot(aes(x=value,y=Sum.Zero.Prey.Densities.Locally))+
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

reg.all.plot%>%
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


loc.all%>%
  ggplot(aes(x=log(prey.den+1),y=pred.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(as.factor(patch.degree)~prod)


loc.all%>%
  ggplot(aes(x=log(pred.den+1),y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(as.factor(patch.degree)~prod)
                                             
reg.all.plot%>%
  filter(pred.ext=="no")%>%
  ggplot(aes(x=prey.dens,y=pred.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(as.factor(patch.degree)~prod)        

reg.all.plot%>%
  ggplot(aes(x=pred.dens,y=prey.persistence))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")#+facet_grid(as.factor(patch.degree)~prod)                                    
     
################################################################################################################################################
#Proportion of Metacommunity Extinctions

ext.prop<-loc.all%>%
  group_by(predator,prey,productivity,log.network.syn.lap,log.number.bottles,replicate,structure,media,year,log.total.vol,nghbr.connect,number.bottles) %>%
  summarize(prop.prey.met.ext=sum(prey.ext.quant)/number.bottles,prop.pred.met.ext=sum(pred.ext.quant)/number.bottles)
  
ext.prop%>%
  #filter(number.bottles>1)%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prop.pred.met.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ylab("Proportion of Predator Metacommunites Extinct")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")          

ext.prop%>%
  filter(number.bottles>1)%>%
  gather(log.number.bottles,log.network.syn.lap,log.total.vol,nghbr.connect, key = "var", value = "value")%>%
  ggplot(aes(x=value,y=prop.prey.met.ext))+
  geom_point()+geom_smooth(method = "lm")+
  scale_color_viridis(discrete = TRUE)+
  ylab("Proportion of Prey Metacommunites Extinct")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+facet_grid(~var,scales="free")     

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
