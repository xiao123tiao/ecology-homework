#Homework_1: Exploring doubs dataset with Tidyverse
#1st task
install.packages("ade4")      #install package ade4
install.packages("tidyverse") #install package tidyverse
library(ade4)                 #Loading library of ade4
library(tidyverse)            #Loading library of tidyverse
data(doubs,package="ade4")    #extract data from ade4 packet doubs "doubs"
str(doubs)                    #checking what the data looks like 
class(doubs)                  #checking the class of the data
#2nd task
env<-doubs$env                #extract env from doubs
site<-row.names(env)          
env_dataframe<-data.frame(site,env)#Turning the row names into a column called site
env_tb<-as_tibble(env_dataframe) #convert the data frame to a tibble，named it env_tb
class(env_tb)                 ##checking the class of the data.
env_tb
#3rd task
dfs<-env_tb$dfs
env_tb[dfs>1000,] 
#Extract and remain the data of the dfs with more than 1000 km.           
env_final<-env_tb[env_tb$dfs>1000,] %>% 
#Select these columns:site, dfs, slo, flo, pH, nit, oxy for further analysis.
select(site, dfs, slo, flo, pH, nit, oxy)%>% 
#Rename some columns as follows: dfs to distsour, slo to slope, flo to flowrate, nit to nitrogen, oxy to oxygen.
rename(distsour = dfs, slope =slo , flowrate =flo ,  nitrogen =nit , oxygen =oxy )%>% 
#Arrange the data first by slope in ascending order, and then by pH in descending order.
arrange(slope,desc(pH))

