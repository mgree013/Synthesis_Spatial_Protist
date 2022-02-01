library(cowplot)
library(tidyverse)
library(ggplot2)
library(viridis)
library(performance)
library(bbmle)
########################################################################################################################
#Ext, time 2 ext, persistence dynamics


loc.all%>%
  ggplot(aes(x=pred.quasi.ext.ten,fill=pred.quasi.ext.ten))+
  geom_bar()+
  scale_fill_viridis_d() +
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

reg.all.plot<-reg.all%>%
  dplyr::distinct(predator,prey,productivity,network.syn.lap,number.bottles,replicate,structure,media,year,total.vol,av.nghbr.connect,.keep_all=T)

reg.all.plot%>%
  ggplot(aes(x=pred.quasi.ext.one,fill=pred.quasi.ext.one))+
  geom_bar()+
  scale_fill_viridis_d() +
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

#local
loc.all.glm<-loc.all%>%
  filter(nghbr.connect>0)#%>%
  #filter(pred.ext=="no")

p1<-loc.all.glm%>%
  ggplot(aes(x=log.number.bottles, y=pred.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Metacommunity Size")+ ylab("Predator Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p2<-loc.all.glm%>%
  ggplot(aes(x=log.total.vol, y=pred.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Total Volume")+ ylab("Predator Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p3<-loc.all.glm%>%
  ggplot(aes(x=nghbr.connect, y=pred.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Nearest Neighboor Connectivity")+ ylab("Predator Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p4<-loc.all.glm%>%
  ggplot(aes(x=log.network.syn.lap, y=pred.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Network Synchrony")+ylab("Predator Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")


p5<-loc.all.glm%>%
  ggplot(aes(x =as.factor(prod) , y = pred.persistence, fill=as.factor(prod))) +
  geom_boxplot()+
  xlab("Productivity (g)")+ ylab("Predator Persistence")+
  scale_fill_viridis(discrete=T)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

plot_grid(p1,p3,p4,p5)

p1<-loc.all.glm%>%
  ggplot(aes(x=log.number.bottles, y=prey.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Metacommunity Size")+ ylab("Prey Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p2<-loc.all.glm%>%
  ggplot(aes(x=log.total.vol, y=prey.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Total Volume")+ ylab("Prey Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p3<-loc.all.glm%>%
  ggplot(aes(x=nghbr.connect, y=prey.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Nearest Neighboor Connectivity")+ylab("Prey Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p4<-loc.all.glm%>%
  ggplot(aes(x=log.network.syn.lap, y=prey.persistence))+
  geom_point() +
  xlab("Network Asynchrony")+ ylab("Prey Persistence")+
  geom_smooth(method='lm')+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")


p5<-loc.all.glm%>%
  ggplot(aes(x =as.factor(prod) , y = prey.persistence, fill=as.factor(prod))) +
  geom_boxplot()+
  xlab("Productivity (g)")+ ylab("Prey Persistence")+
  scale_fill_viridis(discrete=T)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

plot_grid(p1,p3,p4,p5)

p1<-loc.all.glm%>%
  ggplot(aes(x=log.number.bottles, y=pred.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Metacommunity Size")+ ylab("Day of Predator Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p2<-loc.all.glm%>%
  ggplot(aes(x=log.total.vol, y=pred.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Total Volume")+ ylab("Day of Predator Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p3<-loc.all.glm%>%
  ggplot(aes(x=nghbr.connect, y=pred.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Nearest Neighboor Connectivity")+ ylab("Day of Predator Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p4<-loc.all.glm%>%
  ggplot(aes(x=log.network.syn.lap, y=pred.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Network Synchrony")+ ylab("Day of Predator Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")


p5<-loc.all.glm%>%
  ggplot(aes(x =as.factor(prod) , y = pred.time.2.ext, fill=as.factor(prod))) +
  geom_boxplot()+
  xlab("Productivity (g)")+ ylab("Day of Predator Extinction")+
  scale_fill_viridis(discrete=T)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

plot_grid(p1,p3,p4,p5)

p1<-loc.all.glm%>%
  ggplot(aes(x=log.number.bottles, y=prey.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Metacommunity Size")+ ylab("Day of Prey Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p2<-loc.all.glm%>%
  ggplot(aes(x=log.total.vol, y=prey.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Total Volume")+ ylab("Day of Prey Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p3<-loc.all.glm%>%
  ggplot(aes(x=nghbr.connect, y=prey.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Nearest Neighboor Connectivity")+ ylab("Day of Prey Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

p4<-loc.all.glm%>%
  ggplot(aes(x=log.network.syn.lap, y=prey.time.2.ext))+
  geom_point() +
  xlab("Network Asynchrony")+ ylab("Day of Prey Extinction")+
  geom_smooth(method='lm')+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")


p5<-loc.all.glm%>%
  ggplot(aes(x =as.factor(prod) , y = prey.time.2.ext, fill=as.factor(prod))) +
  geom_boxplot()+
  xlab("Productivity (g)")+ ylab("Day of Prey Extinction")+
  scale_fill_viridis(discrete=T)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")

plot_grid(p1,p3,p4,p5)

######################################################################
#Regional
reg.all.glm<-reg.all%>%
  filter(av.nghbr.connect>0)

p1<-reg.all.glm%>%
  ggplot(aes(x=log.number.bottles, y=pred.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Metacommunity Size")+ ylab("Predator Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)

p2<-reg.all.glm%>%
  ggplot(aes(x=log.total.vol, y=pred.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Total Volume")+ ylab("Predator Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)

p3<-reg.all.glm%>%
  ggplot(aes(x=av.nghbr.connect, y=pred.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Neearest Neighboor Connectivity")+ ylab("Predator Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)

p4<-reg.all.glm%>%
  ggplot(aes(x=log.network.syn.lap, y=pred.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Network Asynchrony")+ ylab("Predator Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)


p5<-reg.all.glm%>%
  ggplot(aes(x =as.factor(prod) , y = pred.persistence, fill=as.factor(prod))) +
  geom_boxplot()+
  xlab("Productivity (g)")+ ylab("Predator Persistence")+
  scale_fill_viridis(discrete=T)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)

plot_grid(p1,p3,p4,p5)

p1<-reg.all.glm%>%
  ggplot(aes(x=log.number.bottles, y=prey.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Metacommunity Size")+ ylab("Prey Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)

p2<-reg.all.glm%>%
  ggplot(aes(x=log.total.vol, y=prey.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Total Volume")+ ylab("Prey Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)

p3<-reg.all.glm%>%
  ggplot(aes(x=av.nghbr.connect, y=prey.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Nearest Neighboor Connectivity")+ ylab("Prey Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)

p4<-reg.all.glm%>%
  ggplot(aes(x=log.network.syn.lap, y=prey.persistence))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Network Asynchrony")+ ylab("Prey Persistence")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)


p5<-reg.all.glm%>%
  ggplot(aes(x =as.factor(prod) , y = prey.persistence, fill=as.factor(prod))) +
  geom_boxplot()+xlab("Productivity (g)")+ylab("Prey Persistence")+
  scale_fill_viridis(discrete=T)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)

plot_grid(p1,p3,p4,p5)


p1<-reg.all.glm%>%
  ggplot(aes(x=log.number.bottles, y=pred.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Metacommunity Size")+ylab("Day of Predator Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)

p2<-reg.all.glm%>%
  ggplot(aes(x=log.total.vol, y=pred.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Total Volume")+ ylab("Day of Predator Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)

p3<-reg.all.glm%>%
  ggplot(aes(x=av.nghbr.connect, y=pred.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Neearest Neighboor Connectivity")+ ylab("Day of Predator Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)

p4<-reg.all.glm%>%
  ggplot(aes(x=log.network.syn.lap, y=pred.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Network Asynchrony")+ylab("Day of Predator Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)


p5<-reg.all.glm%>%
  ggplot(aes(x =as.factor(prod) , y = pred.time.2.ext, fill=as.factor(prod))) +
  geom_boxplot()+
  xlab("Productivity (g)")+ ylab("Day of Predator Extinction")+
  scale_fill_viridis(discrete=T)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~pred.ext)

plot_grid(p1,p3,p4,p5)

p1<-reg.all.glm%>%
  ggplot(aes(x=log.number.bottles, y=prey.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Metacommunity Size")+ ylab("Day of Prey Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)

p2<-reg.all.glm%>%
  ggplot(aes(x=log.total.vol, y=prey.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Total Volume")+ ylab("Day of Prey Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)

p3<-reg.all.glm%>%
  ggplot(aes(x=av.nghbr.connect, y=prey.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Nearest Neighboor Connectivity")+ ylab("Day of Prey Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)

p4<-reg.all.glm%>%
  ggplot(aes(x=log.network.syn.lap, y=prey.time.2.ext))+
  geom_point() +
  geom_smooth(method='lm')+
  xlab("Network Asynchrony")+ ylab("Day of Prey Extinction")+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)


p5<-reg.all.glm%>%
  ggplot(aes(x =as.factor(prod) , y = prey.time.2.ext, fill=as.factor(prod))) +
  geom_boxplot()+xlab("Productivity (g)")+ylab("Day of Prey Extinction")+
  scale_fill_viridis(discrete=T)+
  theme_bw()+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(colour = "black"))+theme(legend.position = "none")+facet_grid(~prey.ext)

plot_grid(p1,p3,p4,p5)
########################################################################################################################


#1)Ext-COlon
########################################################################################################################
Ext_col_data<-Ext_col_data%>%
  mutate(log.number.bottles=log(number.bottles+1))%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))%>%
  filter(bottle.number>1)%>%
  filter(nghbr.connect>0)%>%
  left_join(loc.all)
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
        panel.border = element_blank(),panel.background = element_blank())+facet_grid(~prey.ext)

b<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=extinction_prob_prey))+ 
  geom_point()+
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+facet_grid(~prey.ext)#+ theme(legend.position = "none")

c<-Ext_col_data%>%
  ggplot(aes(x=log.network.syn.lap,y=extinction_prob_prey))+ 
  geom_point()+
  ggtitle("c)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Prey Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+facet_grid(~prey.ext)#+ theme(legend.position = "none")

d<-Ext_col_data%>%
  ggplot(aes(x=log.number.bottles,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+facet_grid(~pred.ext)#+ theme(legend.position = "none")

e<-Ext_col_data%>%
  ggplot(aes(x=nghbr.connect,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("h)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+facet_grid(~pred.ext)#+ theme(legend.position = "none")

f<-Ext_col_data%>%
  ggplot(aes(x=log.network.syn.lap,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("g)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Predator Exctinction Probability")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+facet_grid(~pred.ext)#+ theme(legend.position = "none")

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
#MODELS
#prey colon
y <- cbind(Ext_col_data$colonization_sum_pred, Ext_col_data$colonization_potenital_pred)

mod0<-glm(y~as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~(nghbr.connect)*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod8<-glm(y~(log.network.syn.lap)*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod9<-glm(y~log.number.bottles*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod10<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod11<-glm(y~log.network.syn.lap*log.number.bottles*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod12<-glm(y~log.network.syn.lap*as.factor(productivity)*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles*as.factor(productivity)*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod14<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect*productivity,family=binomial(link = "logit"),data=Ext_col_data)
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
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~(nghbr.connect)*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod8<-glm(y~(log.network.syn.lap)*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod9<-glm(y~log.number.bottles*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod10<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod11<-glm(y~log.network.syn.lap*log.number.bottles*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod12<-glm(y~log.network.syn.lap*as.factor(productivity)*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles*as.factor(productivity)*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod14<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect*productivity,family=binomial(link = "logit"),data=Ext_col_data)
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
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~(nghbr.connect)*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod8<-glm(y~(log.network.syn.lap)*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod9<-glm(y~log.number.bottles*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod10<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod11<-glm(y~log.network.syn.lap*log.number.bottles*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod12<-glm(y~log.network.syn.lap*as.factor(productivity)*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles*as.factor(productivity)*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod14<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect*productivity,family=binomial(link = "logit"),data=Ext_col_data)
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

###pred ext
y <- cbind(Ext_col_data$extinction_sum_pred, Ext_col_data$extinction_potenital_pred)

mod0<-glm(y~as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=Ext_col_data)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=Ext_col_data)
mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=Ext_col_data)
mod7<-glm(y~(nghbr.connect)*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod8<-glm(y~(log.network.syn.lap)*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod9<-glm(y~log.number.bottles*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod10<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod11<-glm(y~log.network.syn.lap*log.number.bottles*as.factor(productivity),family=binomial(link = "logit"),data=Ext_col_data)
mod12<-glm(y~log.network.syn.lap*as.factor(productivity)*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod13<-glm(y~log.number.bottles*as.factor(productivity)*nghbr.connect,family=binomial(link = "logit"),data=Ext_col_data)
mod14<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect*productivity,family=binomial(link = "logit"),data=Ext_col_data)
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


########################################################################
#Plots
a<-occupnacy%>%
  ggplot(aes(x=log.number.bottles,y=prey.oc))+ 
  geom_point()+
  ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

b<-occupnacy%>%
  ggplot(aes(x=log.network.syn.lap,y=prey.oc))+ 
  geom_point()+
  ggtitle("c)") +
  #geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

c<-occupnacy%>%
  ggplot(aes(x=nghbr.connect,y=prey.oc))+ 
  geom_point()+
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

d<-occupnacy%>%
  ggplot(aes(x=log.number.bottles,y=pred.oc))+ 
  geom_point()+
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

e<-occupnacy%>%
  ggplot(aes(x=log.network.syn.lap,y=pred.oc))+ 
  geom_point()+
  ggtitle("g)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-occupnacy%>%
  ggplot(aes(x=nghbr.connect,y=pred.oc))+ 
  geom_point()+
  ggtitle("h)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

op1<-occupnacy%>%
  filter(connectivity.x>0)%>%
  ggplot(aes(x=as.factor(productivity),y=pred.oc,fill=as.factor(productivity)))+ 
  geom_boxplot()+
  ggtitle("e)") +
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

plot_grid(op1,op2,nrow=1)

plot_grid(op2,a,b,c,op1,d,e,f,nrow=2)
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
        panel.border = element_blank(),panel.background = element_blank())#+facet_wrap(number.bottles~structure.x, scales="free") #+facet_grid(media~predator)

################################################################################################################################################
#Prey
y <- cbind(occupnacy$prey.occupany, occupnacy$n)

mod1<-glm(y~log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod2<-glm(y~(nghbr.connect),family=binomial(link = "logit"),data=occupnacy)
mod3<-glm(y~log.network.syn.lap,family=binomial(link = "logit"),data=occupnacy)
mod4<-glm(y~as.factor(productivity),family=binomial(link = "logit"),data=occupnacy)

mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=occupnacy)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod7<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=occupnacy)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=occupnacy)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,nullmod,weights = TRUE, sort = T)

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
mod4<-glm(y~as.factor(productivity),family=binomial(link = "logit"),data=occupnacy)

mod4<-glm(y~log.network.syn.lap*log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod5<-glm(y~log.network.syn.lap*nghbr.connect,family=binomial(link = "logit"),data=occupnacy)
mod6<-glm(y~(nghbr.connect)*log.number.bottles,family=binomial(link = "logit"),data=occupnacy)
mod7<-glm(y~log.network.syn.lap*log.number.bottles*nghbr.connect,family=binomial(link = "logit"),data=occupnacy)
nullmod<-glm(y~1,family=binomial(link = "logit"),data=occupnacy)
reported.table2 <- bbmle::AICtab(mod1,mod2,mod3,mod4,nullmod,weights = TRUE, sort = F)

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
pred_Ext_col_data<-Ext_col_data%>%
  left_join(occupnacy)%>%
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
  
#Predicted Occupnacy
dog<-betareg(pred.prey.oc~prey.oc, data=pred_Ext_col_data)
dog1<-betareg(pred.prey.oc~1, data=pred_Ext_col_data)
reported.table2<-bbmle::AICtab(dog1,dog)
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
reported.table2<-bbmle::AICtab(dog1,dog)
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
        panel.border = element_blank(),panel.background = element_blank())+facet_grid(~as.factor(Pred.attack.rate))#+ theme(legend.position = "none")


pred_Ext_col_data%>%
  ggplot(aes(x=pred.oc,y=extinction_prob_pred))+ 
  geom_point()+
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+facet_grid(~prey)

dog<-betareg(colonization_prob_prey~prey.oc, data=pred_Ext_col_data)
dog1<-betareg(colonization_prob_prey~1, data=pred_Ext_col_data)
reported.table2<-bbmle::AICtab(dog1,dog)
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
reported.table2<-bbmle::AICtab(dog1,dog)
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
reported.table2<-bbmle::AICtab(dog1,dog)
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

plot_grid(preda_net,predb_net,preda,predb,predc,predd,prede,predf,nrow=4)

#Network level
Ext_col_data_network<-newer_pa_datas%>%
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
  ggtitle("e)") +
  scale_fill_viridis(discrete = TRUE)+
  labs(x="Productivity (g)",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

a<-Ext_col_data_network%>%
  ggplot(aes(x=log.number.bottles,y=prey.oc))+ 
  geom_point()+
  ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

b<-Ext_col_data_network%>%
  ggplot(aes(x=log.network.syn.lap,y=prey.oc))+ 
  geom_point()+
  ggtitle("c)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Netwokr Synchrony",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

c<-Ext_col_data_network%>%
  ggplot(aes(x=av.nghbr.connect,y=prey.oc))+ 
  geom_point()+
  ggtitle("d)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Prey Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

d<-Ext_col_data_network%>%
  ggplot(aes(x=log.number.bottles,y=pred.oc))+ 
  geom_point()+
  ggtitle("f)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Metacommunity Size",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

e<-Ext_col_data_network%>%
  ggplot(aes(x=log.network.syn.lap,y=pred.oc))+ 
  geom_point()+
  ggtitle("g)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Network Synchrony",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

f<-Ext_col_data_network%>%
  ggplot(aes(x=av.nghbr.connect,y=pred.oc))+ 
  geom_point()+
  ggtitle("h)") +
  geom_smooth(method = "lm",se=F)+
  scale_color_viridis_d()+
  labs(x="Nearest Neighboor Connectivity",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+ theme(legend.position = "none")

plot_grid(a,b,c,d,e,f,nrow=2)
plot_grid(prey.a,a,b,c,pred.a,d,e,f,nrow=2)

#Predicted Occupnacy
preda_net<-Ext_col_data_network%>%
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

predb_net<-Ext_col_data_network%>%
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

Ext_col_data_network_analysis<-Ext_col_data_network%>%
  #filter(pred.prey.oc>0 & pred.prey.oc<1)%>%
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



################################################################################################################################################
#Trophic Dynamics
Ext_col_data%>%
  filter(extinction_sum_pred<2)%>%
  ggplot(aes(y=prey.occupancy,x=pred.occupancy))+
  geom_point()+
  #ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  #stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(y="Prey Density",x="Predator Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")

trophic<-loc.all%>%
  mutate(trophic=if_else(Sum.Zero.Prey.Densities.Locally>Sum.Zero.Predator.Densities.Locally,"prey","pred"))

trophic%>%
  filter(pred.den>0)%>%
  filter(nghbr.connect>0)%>%
  ggplot(aes(y=log(prey.den+1),x=log(pred.den+1)))+
  geom_point()+
  #ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  #stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  annotate("text", x = 4, y = 2.5, label = "R^2 == 0.39", parse = TRUE) +
  labs(y="Prey Density",x="Predator Density")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+ theme(legend.position = "none")#+
  #facet_grid(as.factor(prod)~pred.ext)

trophic.glm<-trophic%>%
  filter(pred.den>0)%>%
  filter(nghbr.connect>0)

dog<-glm(log(prey.den+1)~log(pred.den+1),family=gaussian(link = "identity"), data=trophic.glm)
dog1<-glm(log(prey.den+1)~1,family=gaussian(link = "identity"), data=trophic.glm)
reported.table2<-bbmle::AICtab(dog1,dog)
reported.table2
pseudoR1 <- ((dog$null.deviance-dog$deviance)/dog$null.deviance)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)

r2(dog)
performance::r2(dog1)

trophic%>%
  filter(pred.den>0)%>%
  filter(nghbr.connect>0)%>%
  ggplot(aes(x=prey.oc,y=pred.oc))+
  geom_point()+
  #ggtitle("b)") +
  geom_smooth(method = "lm",se=F)+
  #stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_color_viridis_d()+
  labs(x="Prey Occupancy",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+#+ theme(legend.position = "none")
  facet_grid(as.factor(prod)~prey)

trophic%>%
  filter(pred.den>0)%>%
  filter(nghbr.connect>0)%>%
  ggplot(aes(x=as.factor(prod),y=log(pred.den+1),fill=as.factor(prod)))+
  geom_boxplot()+
  #ggtitle("b)") +
  #stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_fill_viridis_d()+
  labs(x="Prey Occupancy",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+#+ theme(legend.position = "none")
facet_grid(as~trophic)

loc.all%>%
  filter(pred.den>0)%>%
  filter(nghbr.connect>0)%>%
  ggplot(aes(x=as.factor(prod),y=log(prey.den+1),fill=as.factor(prod)))+
  geom_boxplot()+
  #ggtitle("b)") +
  #stat_smooth(method = glm, method.args = list(family = binomial(link = "logit")),se=F)+
  scale_fill_viridis_d()+
  labs(x="Prey Occupancy",y="Predator Occupancy")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())#+#+ theme(legend.position = "none")
#facet_grid(~structure)
