#install.packages("polycor")
#install.packages("corrr")
library(haven)
library(Hmisc)
library(polycor)
library(tidyverse)
library(corrr)
library(nlme)
data <- read_sav("strategijeBazaRaw.sav")
describe(data)
dataItems <- subset(data, select = c(su1:su25, su26, su27:su64, studij, prosjek_fakultet))
dataCrit <- subset(data, select = c(studij, prosjek_fakultet))
hetcor(dataItems, pairwise.complete.obs)
is.data.frame(dataItems)
cor(dataItems)
is.numeric(dataItems$su1)
is.numeric(dataItems$prosjek_fakultet)
cors2 <- sapply(dataItems, cor, y=dataItems$prosjek_fakultet, c=("pairwise.complete.obs"))
cors2
#broj NAs
sum(!complete.cases(dataItems))
corel <- rcorr(dataItems)
cor(dataItems$su1,dataItems$prosjek_fakultet)
dataItems <- as_tibble(dataItems)
dataItems
corelations <- correlate(dataItems)
c <- corelations %>% focus(prosjek_fakultet)
c
print(c)
fashion(c)
fashion(c %>% filter(prosjek_fakultet > .2))
model <- lm(prosjek_fakultet ~ ., dataItems)
summary(model)
mixmod1 <- lme(prosjek_fakultet ~ 1, dataItems, random = ~ 1|studij, na.action="na.omit")
summary(mixmod1)

VarCorr(mixmod1)
varests <- as.numeric(VarCorr(mixmod1)[1:2])
# vector of variance estimates
ICC <- varests[1]/sum(varests) # computes ICC
ICC
colSums(is.na(dataItems))
fashion(colSums)


#github
#https://aberdeenstudygroup.github.io/studyGroup/lessons/SG-T1-GitHubVersionControl/VersionControl/