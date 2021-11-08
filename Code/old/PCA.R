library(ggbiplot)
library(vegan)
#############################################################################################################################
#Spatial PCA

Tpca = prcomp(loc.all[,c(4,14:18)], scale.=TRUE)
biplot(Tpca)

Tpca
summary(Tpca)
ggbiplot(Tpca)
ggbiplot(Tpca,labels=rownames(loc.all$structure), groups=loc.all$structure, ellipse=F)
## let's see what loads where
Tpca$rotation

## let's pull out the first three axes
Spatial_PC1 = scores(Tpca, choices=c(1), display=c("sites")) # patch.degree,patch.deg2,tubelength,nghbr.connect,
Spatial_PC2 = scores(Tpca, choices=c(2), display=c("sites")) # network.syn.lap,number.bottles
Spatial_PC3 = scores(Tpca, choices=c(3), display=c("sites")) # 

Tpca$x
spatial_pc_scores <- data.frame(Tpca$x[,1:3])
colnames(spatial_pc_scores)<-c("Spatial_PC1","Spatial_PC2","Spatial_PC3")
rownames(spatial_pc_scores)<-rownames(loc.all)
spatial_pc_scores<-spatial_pc_scores%>%
  rownames_to_column(var = "Site")

loc.all<-loc.all%>%
  rownames_to_column(var = "Site")

loc.all_spa<-left_join(loc.all,spatial_pc_scores,by="Site")
#############################################################################################################################
#Traits PCA

Tpca = prcomp(loc.all[,c(19:24)], scale.=TRUE)
biplot(Tpca)
summary(Tpca)

ggbiplot(Tpca)
ggbiplot(Tpca,labels=rownames(loc.all$prey), groups=loc.all$prey, ellipse=F)
## let's see what loads where
Tpca$rotation

## let's pull out the first three axes
Spatial_PC1 = scores(Tpca, choices=c(1), display=c("sites")) # patch.degree,patch.deg2,tubelength,nghbr.connect,
Spatial_PC2 = scores(Tpca, choices=c(2), display=c("sites")) # network.syn.lap,number.bottles
#Spatial_PC3 = scores(Tpca, choices=c(3), display=c("sites")) # 

Tpca$x
trait_pc_scores <- data.frame(Tpca$x[,1:2])
colnames(trait_pc_scores)<-c("Trait_PC1","Trait_PC2")
rownames(trait_pc_scores)<-rownames(loc.all)
trait_pc_scores<-trait_pc_scores%>%
  rownames_to_column(var = "Site")

#loc.all<-loc.all%>%rownames_to_column(var = "Site")

loc.all_spa_trait<-left_join(loc.all_spa,trait_pc_scores,by="Site")
#############################################################################################################################
#Predator Response PCA

Tpca = prcomp(loc.all[,c(19:24)], scale.=TRUE)
biplot(Tpca)
summary(Tpca)

ggbiplot(Tpca)
ggbiplot(Tpca,labels=rownames(loc.all$prey), groups=loc.all$prey, ellipse=F)
## let's see what loads where
Tpca$rotation

## let's pull out the first three axes
Spatial_PC1 = scores(Tpca, choices=c(1), display=c("sites")) # patch.degree,patch.deg2,tubelength,nghbr.connect,
Spatial_PC2 = scores(Tpca, choices=c(2), display=c("sites")) # network.syn.lap,number.bottles
#Spatial_PC3 = scores(Tpca, choices=c(3), display=c("sites")) # 

Tpca$x
trait_pc_scores <- data.frame(Tpca$x[,1:2])
colnames(trait_pc_scores)<-c("Trait_PC1","Trait_PC2")
rownames(trait_pc_scores)<-rownames(loc.all)
trait_pc_scores<-trait_pc_scores%>%
  rownames_to_column(var = "Site")

#loc.all<-loc.all%>%rownames_to_column(var = "Site")

loc.all_spa_trait<-left_join(loc.all_spa,trait_pc_scores,by="Site")


#############################################################################################################################

loc.all_spa%>%
  gather(day.prey.min,day.pred.min,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den,prey.persistence,pred.persistence,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,cv.prey,cv.pred, key = "var", value = "value") %>% 
  ggplot(aes(x =(Spatial_PC1) , y = value)) +
  geom_point() +
  geom_smooth(method='glm')+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

loc.all_spa%>%
  gather(day.prey.min,day.pred.min,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den,prey.persistence,pred.persistence,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,cv.prey,cv.pred, key = "var", value = "value") %>% 
  ggplot(aes(x =(Spatial_PC2) , y = value)) +
  geom_point() +
  geom_smooth(method='glm')+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

loc.all_spa_trait%>%
  gather(day.prey.min,day.pred.min,prey.amp,pred.amp,day.prey.max,day.pred.max,prey.den,pred.den,prey.persistence,pred.persistence,Sum.Zero.Prey.Densities.Locally,Sum.Zero.Predator.Densities.Locally,cv.prey,cv.pred, key = "var", value = "value") %>% 
  ggplot(aes(x =as.factor(predator) , y = value, fill=as.factor(predator))) +
  geom_boxplot()+
  facet_wrap(~ var, scales = "free") +
  theme_bw()
