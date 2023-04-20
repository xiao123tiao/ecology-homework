install.packages("ade4")
install.packages("vegan")
install.packages("gclus")
install.packages("FD")
install.packages("factoextra")
install.packages("caret")
install.packages("cluster")
library(ade4)
library(tidyverse)
library(caret)
library(dplyr)
library(vegan)
library(gclus)
library(cluster)


#task1

#Read in the doubs into your R and delete the site 8 which has no fishes
data(doubs,package = "ade4") 
fish <-doubs$fish
fish <- doubs$fish %>% mutate(fish,tfs=rowSums(doubs$fish))
total_fish<-subset(fish,tfs!=0)
#Find the most species and how many species the place has
spe<-doubs$fish 
summary(spe)
#delete no fishes site
spe<-spe[-8,]
env<-doubs$env
env<-env[-8,]
#env<-doubs$env
seq(along=total_fish[,28])[total_fish[,28]==max(total_fish[,28])]
seq(along=total_fish[,28])[total_fish[,28]==min(total_fish[,28])]
max<-max(total_fish$tfs)
min<-min(total_fish$tfs)
#Which species is most widespread (i.e. found in most locations)?
mw_species=data.frame()
for (i in 1:27){
  null_0 <-sum(total_fish[,i]==0)
  mw_species=rbind(mw_species,cbind(colnames(total_fish[i]),null_0))
}
head(mw_species)
mw_species
seq(along=mw_species[,2])[mw_species[,2]==min(mw_species[,2])]
min_species<-min(mw_species$null_0)


#task2

#Select a suitable association measure of species
spe.t<-t(spe)
spe.t.chi<-decostand(spe.t,"chi.square")
spe.t.D<-dist(spe.t.chi)
#Calculate ward & single minimum variance clustering
spe.t.chi.single<-hclust(spe.t.D,method = "single")
plot(spe.t.chi.single)
spe.t.chi.ward <- hclust(spe.t.D, method="ward.D2")
plot(spe.t.chi.ward)
source("coldiss.R")
coldiss(spe.t.D,byrank=FALSE,diag=FALSE)
#Q mode aggregated the locations
spe.norm<-decostand(spe,"normalize")
#Calculate the string distance matrix between quadrat
spe.ch<-vegdist(spe.norm,"euc")
#Calculate the Chord distance clustering
spe.dc<- vegdist(spe.norm)
spe.hel<-decostand(spe,"hel")
#Hellinger matrix
spe.dh<-vegdist(spe.hel)
#Calculate single & ward minimum variance clustering
spe.ch.single<-hclust(spe.ch,method = "single")
plot(spe.ch.single)
spe.ch.ward <- hclust(spe.ch, method="ward.D2")
plot(spe.ch.ward)


#task3

#choose the right analytic method
print(decorana(t(spe)))
#Axis length is 3.0~4.0, so RDA analysis is selected
RDA<-rda(spe,env,scale=T)
#Extract data
spe_rda<-data.frame(RDA$CCA$u[,1:2],rownames(env))
colnames(spe_rda)=c("RDA1","RDA2","samples")
#Extraction species score
spe_rda_score<-data.frame(RDA$CCA$v[,1:2])
#Calculate the axis label data (= axis eigenvalues /sum(all axis eigenvalues)
RDA1=round(RDA$CCA$eig[1]/sum(RDA$CCA$eig)*100,2)
RDA2=round(RDA$CCA$eig[2]/sum(RDA$CCA$eig)*100,2)
plot(RDA)
#Species data Hellinger pre-transformation）
spe_hel <- decostand(spe, method = 'hellinger')
#Use all environmental data
rda_tb <- rda(spe_hel~., env, scale = FALSE)
#Drawing
plot(rda_tb)

