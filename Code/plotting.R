#Take 3
##################################################################################################################################
#Prey.ext
library(jtools)
y<-loc.all$prey.time.2.ext
mod14<-glm(y~as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator),family=poisson(link="log"),data=loc.all)
mod14<-glm(y~log.number.bottles+nghbr.connect+as.factor(productivity),family=poisson(link="log"),data=loc.all)

summary(mod14)
effect_plot(mod14, pred = log.number.bottles)
effect_plot(mod14, pred = nghbr.connect)


#define new observation
newdata = data.frame(disp=200, hp= 100)
#use model to predict value of am
predict(model, newdata, type="response")
prediction<-predict(mod14)




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

lm.mod <- lm(prey.time.2.ext ~ log.number.bottles+nghbr.connect, data = eup.tetra.high)
summary(lm.mod)

prepplot <- as.data.frame(matrix(ncol = 3, nrow = 10000))
colnames(prepplot) <- c("meta.size", "connect", "est.prey.ext")

range(eup.tetra.high$meta.size)
range(eup.tetra.high$nghbr.connect)


prepplot$connect <- rep(seq(1.666667, 4.000000, length.out = 100), 100)
prepplot <- prepplot[order(prepplot$connect),]
prepplot$meta.size <- rep(seq(15,15, length.out = 100), 100)
prepplot$est.prey.ext <- 49.80842 - 8.21662*prepplot$meta.size - 0.12010*prepplot$connect + 
  0.02785*prepplot$meta.size*prepplot$connect
prepplot$est.mpg<-predict(lm.mod,prepplot, type="response")

ggplot(prepplot, aes(meta.size, connect, fill = est.prey.ext)) + 
  geom_tile() +
  ggtitle("A)") +
  xlab("meta.size") + ylab("Connectivity") +
  scale_fill_gradientn(colours = c("#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

# fit model with 3-way-interaction
library(sjPlot)
fit <- glm(prey.time.2.ext ~ as.factor(pred.prey.prod)+log.number.bottles+nghbr.connect,family=poisson(link="log"), data = loc.all)

# select only levels 30, 50 and 70 from continuous variable Barthel-Index
tab_model((fit))
get_model_data(fit, type = "pred",terms = c("nghbr.connect" ,"predator [Didinium, Euplotes]","log.number.bottles", "productivity [0.56,0.76,1.28]"))
plot_model(fit, type = "pred",terms = c("nghbr.connect" ,"log.number.bottles","pred.prey.prod [Didinium_colpidium_0.76, Didinium_paramecium_0.56, Didinium_paramecium_1.28,Euplotes_tetra_0.56,Euplotes_tetra_1.28 ]"))




