#plotting predicted values from best fit models~
##################################################################################################################################

#Problem: It seems like its plotting same slope for all predictors. Need to switch it for specific slope of predictors
#Problem 2: Level of productivity (.76) one is cuasing issues in predict. 

#Part 1: Time to Extinction

#1A)Prey Time to ext
y<-loc.all$prey.time.2.ext
mod14<-glm(y~log.number.bottles+nghbr.connect+as.factor(predator),family=poisson(link="log"),data=loc.all)
summary(mod14)
set.seed(16)
new_data <- data.frame(productivity = factor(rep(c("0.56","0.76","1.28"), each = 400)),
                       predator=factor(rep(c("Euplotes","Didinium"), each = 600)),
                       log.number.bottles  = rep(seq(1.098612, 3.258097, length.out = 1200), 3),
                       nghbr.connect  = rep(seq(1, 8, length.out = 1200), 3))

new_data$prey.time.2.ext <- predict.glm(mod14, newdata = new_data, type = "response")
#new_data <- predict(mod14, newdata = new_data, type = "terms")


prey.meta<-ggplot(data = loc.all, 
       aes(x = log.number.bottles, y = prey.time.2.ext)) +
  geom_point() +
  stat_smooth(data=new_data,method = glm,method.args = list(family = poisson(link = "log")))+
  ggtitle("c)") +
  xlab("Metacommunity Size")+ ylab("Prey Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

prey.con<-ggplot(data = loc.all, 
       aes(x = nghbr.connect, y = prey.time.2.ext)) +
  geom_point() +
  stat_smooth(data=new_data,method = glm,method.args = list(family = poisson(link = "log")))+
  ggtitle("d)") +
  xlab("Connectivity")+ ylab("Prey Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

pred.prey<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=prey.time.2.ext, fill=as.factor(predator)))+
  geom_point()+
  geom_boxplot(data=new_data)+
  scale_fill_viridis(discrete=T)+
  ggtitle("b)") +
  ylab("Prey Time to Extinction")+
  xlab("Predator Idendity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=prey.time.2.ext, fill=as.factor(predator)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ggtitle("b)") +
  ylab("Prey Time to Extinction")+
  xlab("Predator Idendity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

prod.prey<-loc.all%>%
  ggplot(aes(x=as.factor(productivity),y=prey.time.2.ext, fill=as.factor(productivity)))+
  geom_point()+
  geom_boxplot(data=new_data)+
  scale_fill_viridis(discrete=T)+
  ggtitle("a)") +
  ylab("Prey Time to Extinction")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

loc.all%>%
  ggplot(aes(x=as.factor(productivity),y=prey.time.2.ext, fill=as.factor(productivity)))+
  geom_point()+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ggtitle("a)") +
  ylab("Prey Time to Extinction")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

############################################
#1B)Predator Time to ext
y<-loc.all$pred.time.2.ext
mod14<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=poisson(link="log"),data=loc.all)
summary(mod14)

new_data <- data.frame(productivity = factor(rep(c("0.56","0.76","1.28"), each = 400)),
                       predator=factor(rep(c("Euplotes","Didinium"), each = 600)),
                       log.number.bottles  = rep(seq(1.098612, 3.258097, length.out = 1200), 3),
                       nghbr.connect  = rep(seq(1, 8, length.out = 1200), 3))

new_data$pred.time.2.ext <- predict(mod14, newdata = new_data, type = "response")
 

pred.meta<-ggplot(data = loc.all, 
       aes(x = log.number.bottles, y = pred.time.2.ext)) +
  geom_point() +
  stat_smooth(data=new_data,method = glm,method.args = list(family = poisson(link = "log")))+
  ggtitle("g)") +
  xlab("Metacommunity Size")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

pred.con<-ggplot(data = loc.all, 
       aes(x = nghbr.connect, y = pred.time.2.ext)) +
  geom_point() +
  #geom_smooth(method="lm")+
  stat_smooth(data=new_data,method = glm,method.args = list(family = poisson(link = "log")))+
  ggtitle("h)") +
  xlab("Connectivity")+ ylab("Predator Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

pred.pred<-loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=pred.time.2.ext, fill=as.factor(predator)))+
  geom_point()+
  geom_boxplot(data=new_data)+
  scale_fill_viridis(discrete=T)+
  ggtitle("f)") +
  ylab("Predator Time to Extinction")+
  xlab("Predator Idendity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

loc.all%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=pred.time.2.ext, fill=as.factor(predator)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ggtitle("b)") +
  ylab("Predator Time to Extinction")+
  xlab("Predator Idendity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

prod.pred<-loc.all%>%
  ggplot(aes(x=as.factor(productivity),y=pred.time.2.ext, fill=as.factor(productivity)))+
  geom_point()+
  geom_boxplot(data=new_data)+
  scale_fill_viridis(discrete=T)+
  ggtitle("e)") +
  ylab("Predator Time to Extinction")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

loc.all%>%
  ggplot(aes(x=as.factor(productivity),y=pred.time.2.ext, fill=as.factor(productivity)))+
  geom_point()+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ggtitle("a)") +
  ylab("Predator Time to Extinction")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

plot_grid(prod.prey,pred.prey,prey.meta,prey.con,prod.pred,pred.pred,pred.meta,pred.con,nrow=2)



####
#Part 2: Local Occupancy

#2A)Prey Occupancy
y <- cbind(occupnacy$prey.occupany, occupnacy$n)
mod14<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)

new_data <- data.frame(productivity = factor(rep(c("0.56","0.76","1.28"), each = 400)),
                       predator=factor(rep(c("Euplotes","Didinium"), each = 600)),
                       log.number.bottles  = rep(seq(1.098612, 3.258097, length.out = 1200), 3),
                       nghbr.connect  = rep(seq(1, 8, length.out = 1200), 3))

new_data$prey.oc <- predict(mod14, newdata = new_data, type = "response")

prey.meta<-ggplot(data = occupnacy, 
                  aes(x = log.number.bottles, y = prey.oc)) +
  geom_point() +
  stat_smooth(data=new_data,method = glm,method.args = list(family = binomial(link = "logit")))+
  ggtitle("c)") +
  xlab("Metacommunity Size")+   ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

prey.con<-ggplot(data = occupnacy, 
                 aes(x = nghbr.connect, y = prey.oc)) +
  geom_point() +
  stat_smooth(data=new_data,method = glm,method.args = list(family = binomial(link = "logit")))+
  ggtitle("d)") +
  xlab("Connectivity")+   ylab("Prey Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

pred.prey<-occupnacy%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=prey.oc, fill=as.factor(predator)))+
  geom_point()+
  geom_boxplot(data=new_data)+
  scale_fill_viridis(discrete=T)+
  ggtitle("b)") +
  ylab("Prey Occupancy")+
  xlab("Predator Idendity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

occupnacy%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=prey.oc, fill=as.factor(predator)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ggtitle("b)") +
  ylab("Prey Occupancy")+
  xlab("Predator Idendity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

prod.prey<-occupnacy%>%
  ggplot(aes(x=as.factor(productivity),y=prey.oc, fill=as.factor(productivity)))+
  geom_point()+
  geom_boxplot(data=new_data)+
  scale_fill_viridis(discrete=T)+
  ggtitle("a)") +
  ylab("Prey Occupancy")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

occupnacy%>%
  ggplot(aes(x=as.factor(productivity),y=prey.oc, fill=as.factor(productivity)))+
  geom_point()+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ggtitle("a)") +
  ylab("Prey Occupancy")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

############################################
#2B)Predator Occupancy
y <- cbind(occupnacy$pred.occupany, occupnacy$n)
mod14<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=binomial(link="logit"),data=occupnacy)
summary(mod14)

new_data <- data.frame(productivity = factor(rep(c("0.56","0.76","1.28"), each = 80000)),
                       predator=factor(rep(c("Euplotes","Didinium"), each = 120000)),
                       log.number.bottles  = rep(seq(1.098612, 3.258097, length.out = 2400), 100),
                       nghbr.connect  = rep(seq(1, 8, length.out = 2400), 100))

new_data$pred.oc <- predict(mod14, newdata = new_data, type = "response")

new_data%>%ggplot( aes(x = nghbr.connect, y = pred.oc)) +
  geom_point() +
  geom_smooth(method="lm", colour="black")
  
pred.meta<-ggplot(data = occupnacy, 
                  aes(x = log.number.bottles, y = pred.oc)) +
  geom_point() +
  stat_smooth(data=new_data,method = glm,method.args = list(family = binomial(link = "logit")))+
  ggtitle("g)") +
  xlab("Metacommunity Size")+ ylab("Predator Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

pred.con<-ggplot(data = occupnacy, 
                 aes(x = nghbr.connect, y = pred.oc)) +
  geom_point() +
  geom_smooth(method="lm", colour="black")+
  stat_smooth(data=new_data,method = glm,method.args = list(family = binomial(link = "logit")))+
  ggtitle("h)") +
  xlab("Connectivity")+ ylab("Predator Occupancy")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

pred.pred<-occupnacy%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=pred.oc, fill=as.factor(predator)))+
  geom_point()+
  geom_boxplot(data=new_data)+
  scale_fill_viridis(discrete=T)+
  ggtitle("f)") +
  ylab("Predator Occupancy")+
  xlab("Predator Idendity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

occupnacy%>%
  filter(number.bottles>1)%>%
  ggplot(aes(x=as.factor(predator),y=pred.oc, fill=as.factor(predator)))+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ggtitle("b)") +
  ylab("Predator Occupancy")+
  xlab("Predator Idendity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")+ theme(axis.text.x = element_text(face = "italic"))

prod.pred<-occupnacy%>%
  ggplot(aes(x=as.factor(productivity),y=pred.oc, fill=as.factor(productivity)))+
  geom_point()+
  geom_boxplot(data=new_data)+
  scale_fill_viridis(discrete=T)+
  ggtitle("e)") +
  ylab("Predator Occupancy")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

occupnacy%>%
  ggplot(aes(x=as.factor(productivity),y=pred.oc, fill=as.factor(productivity)))+
  geom_point()+
  geom_boxplot()+
  scale_fill_viridis(discrete=T)+
  ggtitle("a)") +
  ylab("Predator Occupancy")+
  xlab("Productivity")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))+
  theme(legend.position = "none")

plot_grid(prod.prey,pred.prey,prey.meta,prey.con,prod.pred,pred.pred,pred.meta,pred.con,nrow=2)











#Prey.ext
library(jtools)
y<-loc.all$prey.time.2.ext
mod14<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=poisson(link="log"),data=loc.all)
mod14<-glm(y~log.number.bottles+nghbr.connect,family=poisson(link="log"),data=loc.all)

summary(mod14)

prepplot <- as.data.frame(matrix(ncol = 3, nrow = 10000))
colnames(prepplot) <- c("log.number.bottles", "nghbr.connect", "est.y")

range(loc.all$log.number.bottles)
range(loc.all$nghbr.connect)

prepplot$log.number.bottles <- rep(seq(1.098612, 3.258097, length.out = 100), 100)
prepplot <- prepplot[order(prepplot$log.number.bottles),]
prepplot$nghbr.connect <- rep(seq(1,8, length.out = 100), 100)
prepplot$est.y<-predict(mod14,prepplot, type="response")

ggplot(prepplot, aes(nghbr.connect, log.number.bottles, fill = est.y)) + 
  geom_tile() +
  ggtitle("A)") +
  xlab("Connectivity") + ylab("Metacommunity Size") +
  scale_fill_viridis_c()+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))+
  labs(fill = "Est. Prey Time to Ext.")
















######OLD Stuff not working
## generate prediction frame
y<-loc.all$prey.time.2.ext
lm.mod<-lm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),data=loc.all)
summary(lm.mod)

prepplot <- as.data.frame(matrix(ncol = 5, nrow = 10000))
colnames(prepplot) <- c("productivity", "log.number.bottles", "nghbr.connect","predator", "est.prey.ext")

range(loc.all$nghbr.connect)
range(loc.all$log.number.bottles)


prepplot$log.number.bottles <- rep(seq(1.098612, 3.258097, length.out = 100), 100)
prepplot <- prepplot[order(prepplot$log.number.bottles),]
prepplot$nghbr.connect <- rep(seq(1,8, length.out = 100), 100)
prepplot$est.prey.ext<-predict(lm.mod,prepplot, type="response")


loc.all%>%
  ggplot(aes(x=log.number.bottles,y=prey.time.2.ext, colour=interaction(as.factor(productivity),as.factor(predator))))+
  geom_point()+#geom_smooth(method = "lm")+
  #stat_smooth(method="glm", method.args=list(family="poisson"), se=FALSE) +
  scale_color_viridis(discrete = TRUE, name="Productivity and Predator")+
  ggtitle("c)") +
  xlab("Metacommunity Size")+ ylab("Prey Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

loc.all%>%
  ggplot(aes(x=nghbr.connect,y=prey.time.2.ext, colour=interaction(as.factor(productivity),as.factor(predator))))+
  geom_point()+#geom_smooth(method = "lm")+
  stat_smooth(method="glm", method.args=list(family="poisson"), se=FALSE) +
  scale_color_viridis(discrete = TRUE, name="Productivity and Predator")+
  ggtitle("c)") +
  xlab("Connectivity")+ ylab("Prey Time to Extinction")+
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black"))

##################################################################################################################################
#plotting take 2: Interactions
#Prod=0.56,0.76,1.28
#prey=colpidium,paramecium,tetra
#predator=Euplotes,Didinium

#Eup-Tetra-Low
eup.tetra.low<-loc.all%>%
  filter(prod=="0.56")%>%
  filter(predator=="Euplotes")%>%
  filter(prey=="tetra")

#Eup-tetra-high
eup.tetra.high<-loc.all%>%
  filter(prod=="1.28")%>%
  filter(predator=="Euplotes")%>%
  filter(prey=="tetra")

#colp-Did
did.colp.med<-loc.all%>%
  filter(prod=="0.76")%>%
  filter(predator=="Didinium")%>%
  filter(prey=="colpidium")

#Did-Para low
did.par.low<-loc.all%>%
  filter(prod=="0.56")%>%
  filter(predator=="Didinium")%>%
  filter(prey=="paramecium")

#Did-Para0high
did.par.high<-loc.all%>%
  filter(prod=="1.28")%>%
  filter(predator=="Didinium")%>%
  filter(prey=="paramecium")

########################################################################################################################
#Exmaple Stack Exchange
lm.mod <- lm(mpg ~ wt*hp, data = mtcars)
summary(lm.mod)

prepplot <- as.data.frame(matrix(ncol = 3, nrow = 10000))
colnames(prepplot) <- c("hp", "wt", "est.mpg")

prepplot$hp <- rep(seq(52,335, length.out = 100), 100)
prepplot <- prepplot[order(prepplot$hp),]
prepplot$wt <- rep(seq(1.513,5.424, length.out = 100), 100)
prepplot$est.mpg <- 49.80842 - 8.21662*prepplot$wt - 0.12010*prepplot$hp + 
  0.02785*prepplot$wt*prepplot$hp
prepplot$est.mpg<-predict(lm.mod,prepplot, type="response")

ggplot(prepplot, aes(wt, hp, fill = est.mpg)) + 
  geom_tile() +
  ggtitle("A)") +
  xlab("Weight (1000 lbs.)") + ylab("Horsepower") +
  scale_fill_gradientn(colours = c("#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
################################################################################################################################################################################################

lm.mod <- lm(prey.time.2.ext ~ log.number.bottles+nghbr.connect, data = did.colp.med)
summary(lm.mod)

prepplot <- as.data.frame(matrix(ncol = 3, nrow = 10000))
colnames(prepplot) <- c("nghbr.connect","log.number.bottles",  "est.prey.ext")

range(did.colp.med$meta.size)
range(did.colp.med$nghbr.connect)

prepplot$nghbr.connect <- rep(seq(1,8, length.out = 100), 100)
prepplot <- prepplot[order(prepplot$nghbr.connect),]
prepplot$log.number.bottles <- rep(seq(2,25, length.out = 100), 100)
prepplot$est.prey.ext<-predict(lm.mod,prepplot, type="response")

ggplot(prepplot, aes(log.number.bottles, nghbr.connect, fill = est.prey.ext)) + 
  geom_tile() +
  ggtitle("A)") +
  xlab("meta.size") + ylab("Connectivity") +
  scale_fill_gradientn(colours = c("#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))


######
range(did.colp.med$meta.size)
range(did.colp.med$nghbr.connect)


range(eup.tetra.high$meta.size)
range(eup.tetra.high$nghbr.connect)

range(eup.tetra.low$meta.size)
range(eup.tetra.low$nghbr.connect)

range(did.par.high$meta.size)
range(did.par.high$nghbr.connect)

range(did.par.low$meta.size)
range(did.par.low$nghbr.connect)

# fit model with 3-way-interaction
library(sjPlot)
fit <- glm(prey.time.2.ext ~ as.factor(pred.prey.prod)+log.number.bottles+nghbr.connect,family=poisson(link="log"), data = loc.all)

# select only levels 30, 50 and 70 from continuous variable Barthel-Index
tab_model((fit))
get_model_data(fit, type = "pred",terms = c("nghbr.connect" ,"predator [Didinium, Euplotes]","log.number.bottles", "productivity [0.56,0.76,1.28]"))
plot_model(fit, type = "pred",terms = c("nghbr.connect" ,"log.number.bottles","pred.prey.prod [Didinium_colpidium_0.76, Didinium_paramecium_0.56, Didinium_paramecium_1.28,Euplotes_tetra_0.56,Euplotes_tetra_1.28 ]"))




