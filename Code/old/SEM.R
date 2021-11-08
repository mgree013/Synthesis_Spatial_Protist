##### Lavaan modeling
library(lavaan)
library(semPlot)

smod1 = ' Pred ~ Spatial  
          Pred=~pred.persistence+pred.amp
          Spatial =~ connect.per  +total.vol +network.syn.lap
         '

#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=loc.all)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)

#Check scale vars
pred.persistence<-loc.all$pred.persistence
prey.den<-loc.all$pred.den
synchrony<-loc.all$network.syn.lap
meta.size<-loc.all$meta.size
connectivity<-loc.all$patch.degree
media<-loc.all$media

summary(loc.all$pred.persistence)
summary(loc.all$prey.persistence)
summary(loc.all$network.syn.lap)
summary(loc.all$meta.size)
summary(loc.all$patch.degree)
summary(loc.all$total.vol)
summary(loc.all$cv.pred)
summary(loc.all$cv.prey)
summary(loc.all$pred.den)
summary(loc.all$prey.den)
summary(loc.all$tubelength)

network.syn.lap<-loc.all$network.syn.lap/10
summary(network.syn.lap)
cv.pred<-loc.all$cv.pred/100
summary(cv.pred)
cv.prey<-loc.all$cv.prey/100
summary(cv.prey)
pred.den<-loc.all$pred.den/100
summary(pred.den)
prey.den<-loc.all$prey.den/1000
summary(prey.den)
patch.degree<-loc.all$patch.degree
meta.size<-loc.all$meta.size/10
summary(meta.size)
total.vol<-loc.all$total.vol
prey.persistence<-loc.all$prey.persistence/10
summary(prey.persistence)
pred.persistence<-loc.all$pred.persistence/10
summary(pred.persistence)
prod<-loc.all$prod
tube.length<-loc.all$tubelength/10
summary(tube.length)
prey.size<-loc.all$prey.size/100
summary(prey.size)
pred.size<-loc.all$pred.size/100
summary(pred.size)
pred.attack<-loc.all$pred.attack
summary(pred.attack)
prey.k.cap<-loc.all$prey.k.cap/1000
summary(prey.k.cap)
connectivity<-loc.all$connect.per
summary(connectivity)
new.frame<-data.frame(prod,pred.persistence,prey.persistence,cv.pred,cv.prey,pred.den,prey.den,patch.degree,meta.size,network.syn.lap,total.vol,tube.length,prey.k.cap,prey.size,pred.size,pred.attack,connectivity)
smod1 = ' pred.persistence ~Spatial+prod+prey.persistence
          prey.persistence~Spatial+prod
          Spatial=~patch.degree  +total.vol +network.syn.lap+meta.size'

smod1 = ' pred.persistence ~Spatial+prod+prey.persistence
          prey.persistence~Spatial+prod
          Spatial=~lam1*patch.degree  +lam1*total.vol +lam1*network.syn.lap+lam1*meta.size'

smod1 = ' pred.cv ~Spatial+prod+prey.cv
          prey.cv~Spatial+prod
          Spatial=~patch.degree  +total.vol +network.syn.lap+meta.size'
#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=new.frame)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)


smod1 = ' pred.amp ~Spatial  +prey.persistence +prod
          prey.amp~Spatial +prod
          Spatial =~ patch.degree  +total.vol +network.syn.lap+meta.size+tube.length
         '

#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=new.frame)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)

smod1 = ' pred.persistence~prey.persistence +Spatial
          Spatial~prey.persistence
          Spatial =~ patch.degree  +meta.size +network.syn.lap
         '

#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=new.frame)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)


#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation=2)

#Composite
model.prey.traits<-lm(prey.persistence~prey.growth+prey.k.cap,data = loc.all)
summary(model.prey.traits)

model_prey <- summary(model.prey.traits)$coefficients[2, 1]

model_prey2 <- summary(model.prey.traits)$coefficients[3, 1]

composite <- model_prey * loc.all$prey.growth + model_prey2 * loc.all$prey.k.cap

summary(lm(prey.persistence ~ composite, data = data.frame(loc.all, composite)))


smod1 = ' pred.persistence ~prey.persistence + Pred.Traits+Prey.Traits
          Pred.Traits=~pred.size+pred.attack
          Prey.Traits=~prey.size+prey.k.cap'

smod1 = ' pred.persistence ~prey.persistence +total.vol+connectivity+network.syn.lap
          prey.persistence~ prod+total.vol+connectivity+network.syn.lap'

smod1 = ' prey.persistence ~pred.persistence +total.vol+connect.per+network.syn.lap
          pred.persistence~ meta.size+connect.per+network.syn.lap
          prod~prey.persistence'
#Spatial =~ patch.degree  +total.vol +network.syn.lap+meta.size
#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=new.frame)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = TRUE,  fade=FALSE, rotation=2)

############
Tpca = prcomp(new.frame[,c(8:12)], scale.=TRUE)
biplot(Tpca)

summary(Tpca)
## let's see what loads where
Tpca$rotation

## let's pull out the first three axes
PC1 = Tpca$x[,1] # Al, BS, Ca, ECEC, FE, K, MG, Mn, NO3 min, P, Ph
PC2 = Tpca$x[,2] # NH4, Ntot
PC3 = Tpca$x[,3] # NO3, Nmin
## Na loads on PC 4


##########

smod1 = ' pred.persistence ~ Spatial + Prey  
          Spatial =~ connect.per + meta.size +total.vol +network.syn.lap
          Prey ~ prey.den + prey'

smod1 = '   pred.persistence ~ Spatial + prey.persistence  
            prey.persistence~Spatial
           Spatial =~ connect.per + meta.size  +network.syn.lap
            '

smod1 = '   pred.persistence~prey.persistence
            pred.persistence ~ Spatial 
            prey.persistence ~ Spatial 
            Spatial =~ connect.per  +total.vol +network.syn.lap
            '

smod1 = ' cv.pred ~ meta.size +connect.per+ prey.den  +network.syn.lap
          '
smod1 = ' Pred ~ Spatial + Prey 
          Pred=~pred.persistence+cv.pred
          Prey=~prey.den +pred.persistence
          Spatial =~ connect.per  +total.vol +network.syn.lap'

smod1 = ' Pred ~ Spatial  
          Pred=~pred.persistence+cv.pred
          Spatial =~ connect.per  +total.vol +network.syn.lap
          Pred~ predator + Pred
          Spatial~ predator + Spatial'


#####
#Ext-Colon Data
Ext_col_dataz<-Ext_col_data%>%
  mutate(log.network.syn.lap=log(network.syn.lap+1))

smod1 = ' extinction_prob_prey ~ nghbr.connect  +number.bottles +log.network.syn.lap
         '

#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=Ext_col_dataz)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)

smod1 = ' colonization_prob_pred ~ nghbr.connect  +number.bottles +log.network.syn.lap
         '

#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=Ext_col_dataz)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)

smod1 = ' 
          colonization_prob_pred~extinction_prob_prey +colonization_prob_prey +extinction_prob_pred
         '

#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=Ext_col_dataz)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)

#######OCcupancy
#Ext-Colon Data


smod1 = ' prey.oc ~ nghbr.connect  +log.number.bottles +log.network.syn.lap+pred.oc+prey.y+media
          pred.oc ~ nghbr.connect  +log.number.bottles +log.network.syn.lap+predator+media
         '

#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=occupnacy)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)

#2
smod1 = ' prey.oc ~ productivity
          pred.oc ~ prey.oc '

smod1 = ' prey.oc ~ log.network.syn.lap+log.number.bottles+nghbr.connect+productivity
          pred.oc ~ prey.oc+nghbr.connect+log.network.syn.lap+log.number.bottles'

smod1 = ' prey.oc ~ Spatial + pred.oc + media+predator +prey.y
          pred.oc ~ prey.oc + Spatial +predator +prey.y
          Spatial =~ nghbr.connect  +log.number.bottles +log.network.syn.lap'

smod1 = 'prey.oc ~ log.number.bottles+nghbr.connect+productivity+Prey.disp.rate
         pred.oc ~ prey.oc+nghbr.connect+log.number.bottles +Pred.attack.rate'

occupnacy$Prey.disp.rate<-occupnacy$Prey.disp.rate/100
#prey.traits =~Prey.size+Prey.K.cap+Prey.growth.rate+Prey.disp.rate
filt.occupancy<-occupnacy%>%
  dplyr::select(c(prey.oc,pred.oc,productivity,nghbr.connect,log.number.bottles,log.network.syn.lap))
apply(filt.occupancy, 2, na.rm=TRUE, sd)
#space<-'pred.persistence =~connect.per + meta.size +total.vol '
smod1.fit <- sem(smod1,data=occupnacy)
summary(smod1.fit,standardized=TRUE,rsq=T)
fitMeasures(smod1.fit)
modindices(smod1.fit)

#quick plot of path analysis
semPaths(smod1.fit, what='std', layout = "tree3", intercepts = FALSE, residuals = FALSE, 
         edge.label.cex=1.25, curvePivot = FALSE,  fade=FALSE, rotation = 2)
