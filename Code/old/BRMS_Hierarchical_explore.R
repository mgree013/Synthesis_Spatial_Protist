install.packages("brms")
library(brms)
library(tidyverse)

##### S model #######
S_m_alpha_4stu <- brm(S ~ Treatment + (Treatment | Study) +
                        (1 | site.id / Block),
                      family = 'poisson', #Poisson because S are integer values at alpha-scale
                      data = alpha_data_4stu,
                      cores = 4, chains = 4,
                      iter = 4000,
                      prior = c(
                        prior('normal(0,1)', class = 'sd'),
                        prior('normal(0,1)', class = 'b')),
                      control = list(adapt_delta = 0.99, max_treedepth = 12))
fixef(S_m_alpha_4stu)

#
y<-loc.all$prey.time.2.ext
mod14<-glm(y~,family=poisson(link="log"),data=loc.all)
###Time to Ext prey analysis ######

S_m_alpha_4stu <- brm(prey.time.2.ext ~ as.factor(productivity)+log.number.bottles+nghbr.connect+as.factor(predator) +
                        (newID | year) + (1 | structure / replicate),
                      family = 'poisson', #Poisson because S are integer values at alpha-scale
                      data = loc.all,
                      cores = 4, chains = 4,
                      iter = 4000,
                      prior = c(
                        prior('normal(0,1)', class = 'sd'),
                        prior('normal(0,1)', class = 'b')),
                      control = list(adapt_delta = 0.99, max_treedepth = 12))
fixef(S_m_alpha_4stu)
summary(S_m_alpha_4stu)
