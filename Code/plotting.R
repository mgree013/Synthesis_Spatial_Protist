#plotting take 2: Interactions
#Prod=0.56,0.76,1.28
#prey=colpidium,paramecium,tetra
#predator=Euplotes,Didinium

#Eup-Tetra-Low
loc.all%>%
  filter(prod=="0.56")%>%
  filter(predator=="Euplotes")%>%
  filter(prey=="tetra")%>%
  ggplot(aes(x=log.number.bottles,y=pred.time.2.ext))+
  geom_point()+#geom_smooth(method = "lm")+
  stat_smooth(method="glm", method.args=list(family="poisson"), se=FALSE) +
  scale_color_viridis(discrete = TRUE)+
  #ggtitle("f)") +
  ggtitle("g)") +
  xlab("Metacommunity Size")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

#Eup-tetra-high
loc.all%>%
  filter(prod=="1.28")%>%
  filter(predator=="Euplotes")%>%
  filter(prey=="tetra")%>%
  ggplot(aes(x=log.number.bottles,y=pred.time.2.ext))+
  geom_point()+#geom_smooth(method = "lm")+
  stat_smooth(method="glm", method.args=list(family="poisson"), se=FALSE) +
  scale_color_viridis(discrete = TRUE)+
  ggtitle("g)") +
  xlab("Metacommunity Size")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

#colp-Did
loc.all%>%
  filter(prod=="0.76")%>%
  filter(predator=="Didinium")%>%
  filter(prey=="colpidium")%>%
  ggplot(aes(x=log.number.bottles,y=pred.time.2.ext))+
  geom_point()+#geom_smooth(method = "lm")+
  stat_smooth(method="glm", method.args=list(family="poisson"), se=FALSE) +
  scale_color_viridis(discrete = TRUE)+
  #ggtitle("f)") +
  ggtitle("g)") +
  xlab("Metacommunity Size")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

#Did-Para low
loc.all%>%
  filter(prod=="0.56")%>%
  filter(predator=="Didinium")%>%
  filter(prey=="paramecium")%>%
  ggplot(aes(x=log.number.bottles,y=pred.time.2.ext))+
  geom_point()+#geom_smooth(method = "lm")+
  stat_smooth(method="glm", method.args=list(family="poisson"), se=FALSE) +
  scale_color_viridis(discrete = TRUE)+
  #ggtitle("f)") +
  ggtitle("g)") +
  xlab("Metacommunity Size")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

#Did-Para low
loc.all%>%
  filter(prod=="1.28")%>%
  filter(predator=="Didinium")%>%
  filter(prey=="paramecium")%>%
  ggplot(aes(x=nghbr.connect,y=pred.time.2.ext))+
  geom_point()+#geom_smooth(method = "lm")+
  stat_smooth(method="glm", method.args=list(family="poisson"), se=FALSE) +
  scale_color_viridis(discrete = TRUE)+
  #ggtitle("f)") +
  ggtitle("g)") +
  xlab("Metacommunity Size")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

