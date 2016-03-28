setwd("D:\\kaggle\\2016_presidential_election\\new\\2016_presidential_election")


results<-read.csv("primary_results.csv")
county.demo<-read.csv("county_facts.csv")


library(magrittr)
library(dplyr)

head(results)

#Filter Votes of Republicans and Democrats into twi frames
unique(results$party)
Republican.votes<-results %>%
  filter(party=="Republican")
Democrats.votes<-results %>%
  filter(party=="Democrat")

library(ggplot2)
#calculate the votes percentage in each state for every Republican Candidate


#Define Democrats Winner of a county
countyWinner<-Democrats.votes %>%
  group_by(state,fips) %>%
  summarize(winner = candidate[which.max(fraction_votes)],
            fraction_votes = max(fraction_votes),
            votes = max(votes))
head(countyWinner)
countyWinner$winner<-factor(countyWinner$winner)

library(sqldf)

#Find out the number of countys won by the candidates in each state 
stateCount<-sqldf('select state,winner,count(winner) as countyswon from countyWinner group by state,winner') 

# Visualize the resiult
ggplot(data=stateCount, aes(x=state, y=countyswon, fill=winner)) +
  geom_bar(colour="black", stat="identity")+ggtitle("Hillary Clinton Vs Bernie Sanders in Primarys")


#Define state winner as the winner of maximum countys
statewinners<-sqldf('select state,winner,max(countyswon) as countyswon from (select state,winner,count(winner) as countyswon from countyWinner group by state,winner)  group by state') 

library(data.table)

#Select the demographic featured from the demographic dataset for analysis
race.demographics<-subset(county.demo,select=
                            c("fips","area_name","AGE775214","SEX255214","RHI125214","RHI225214","RHI325214","RHI425214","RHI725214","POP645213","POP815213","EDU685213","VET605213","HSG445213","INC110213","PVY020213","POP060210"))
setnames(race.demographics, old=c("AGE775214","SEX255214","RHI125214","RHI225214","RHI325214","RHI425214","RHI725214","POP645213","POP815213","EDU685213","VET605213","HSG445213","INC110213","PVY020213","POP060210"), new=c("Old_Citizens","Female_Percent","Whites_Percent","African_American_Percent","American_Indian_Percent","Asians_Percent","Hispanic_Latino_Percent","Foreign_born_Percent","Non_English_speaking_Percent","Collegieate_Percent","Veterans","House_Ownership_Rate","Median_Household_Income","Poverty_Level_Percent","Population_Density"))

#Join the County winners with county demographics
Democrats.votes<-inner_join(countyWinner,race.demographics, by="fips")

#Filter only the countys won by Hillary Clinton and Bernie Sanders
Democrats.votes %<>%
  filter(winner=="Hillary Clinton" | winner=="Bernie Sanders")
nrow(Democrats.votes)

#remove unnecessary fields like fraction of votes and number of votes
new<-subset(Democrats.votes,select=c(winner,Old_Citizens,Female_Percent,Whites_Percent,African_American_Percent,American_Indian_Percent,Asians_Percent,Hispanic_Latino_Percent,Foreign_born_Percent,Non_English_speaking_Percent,Collegieate_Percent,Veterans,House_Ownership_Rate,Median_Household_Income,Poverty_Level_Percent,Population_Density))
head(new)


#Feature Hashing of winner variable for cluster analysis
new$winner<-factor(new$winner)
dummy<-model.matrix(~winner-1,data=new)
joined = cbind(new[match(rownames(dummy), rownames(new)),], dummy)
new<-subset(joined, select=-c(winner))

#Feature hashed colum names
names(new)

#find out the number of clusters in the k means alogrithm from SSE plot by elbow method
wss <- (nrow(new)-1)*sum(apply(new,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(new, 
                                     centers=i,iter.max=1000,algorithm="MacQueen")$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


#Create the clusters
k<-kmeans(new, centers=4,iter.max=1000,algorithm="MacQueen")
k$size
k$centers
k$withinss
summary(k)

aggregate(new,by=list(cluster=k$cluster), FUN=mean)

library("fpc")
#plot the cluster
plotcluster(new, k$cluster)

#projection of linear discriminants
dp = discrproj(new, k$cluster)

new$clustname<-k$cluster
new$clustname<-factor(new$clustname)
head(new)

#Visualize the cluster
ggplot(new, aes(dp$proj[,1], dp$proj[,2], color=factor(k$cluster)))+geom_point(pch=19,size=8)+ggtitle("Cluster Visualization")


#Visualize the Hillary Clinton's win against Bernie Samders in each CLuster
ggplot(new, aes(dp$proj[,1], dp$proj[,2], color=factor(new$`winnerHillary Clinton`)))+geom_point(pch=19,size=8)+facet_grid(clustname~.)+ggtitle("Countys won by Hillary Clinton against Bernie Sanders in each Cluster")





