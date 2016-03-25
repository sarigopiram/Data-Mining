setwd("D:\\kaggle\\2016_presidential_election")

results<-read.csv("primary_results.csv")
county.demo<-read.csv("county_facts.csv")


library(magrittr)
library(dplyr)

head(results)

unique(results$party)

Republican.votes<-results %>%
  filter(party=="Republican")
Democrats.votes<-results %>%
  filter(party=="Democrat")

library(ggplot2)
#calculate the votes percentage in each state for every Republican Candidate

AvgFraction.votes<-Republican.votes %>%
  group_by(candidate,state) %>%
  summarize(AvgVotesPercentage=mean(fraction_votes)*100)



ggplot(AvgFraction.votes, aes(x=factor(state), y=AvgVotesPercentage, colour=candidate, group=candidate)) + geom_line(size=1.5) + geom_point(size=4, shape=21, fill="white")
  

countyWinner<-Democrats.votes %>%
  group_by(state,fips) %>%
  summarize(winner = candidate[which.max(fraction_votes)],
            fraction_votes = max(fraction_votes),
            votes = max(votes))

head(countyWinner)
countyWinner$winner<-factor(countyWinner$winner)
library(sqldf)

ggplot(countyWinner, aes(x=factor(state), y=fraction_votes, colour=winner, group=winner)) + geom_line(size=1.5) + geom_point(size=4, shape=21, fill="white")

ggplot(data=countyWinner, aes(c(state,fips), fill=winner)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black")


DemAvgFraction.votes<-Democrats.votes %>%
  group_by(candidate,state) %>%
  summarize(AvgVotesPercentage=mean(fraction_votes)*100)
ggplot(DemAvgFraction.votes, aes(x=factor(state), y=AvgVotesPercentage, colour=candidate, group=candidate)) + geom_line(size=1.5) + geom_point(size=4, shape=21, fill="white")


head(county.demo)

race.demographics<-subset(county.demo,select=
                            c("fips","area_name","RHI125214","RHI225214","RHI325214","RHI425214","RHI525214","RHI725214"))



Democrats.votes<-inner_join(Democrats.votes,race.demographics, by="fips")

library(GGally)
nrow(Democrats.votes)
Democrats.votes %<>%
  filter(candidate=="Hillary Clinton" | candidate=="Bernie Sanders")

Democrats.votes$candidate<-factor(Democrats.votes$candidate)
ggparcoord(data = Democrats.votes,                 
           # Which columns to use in the plot
           columns = 10:14,                 
           # Which column to use for coloring data
           groupColumn = 6,                 
           # Allows order of vertical bars to be modified
           order = "anyClass",                
           # Do not show points
           showPoints = FALSE,                
           # Turn on alpha blending for dense plots
           alphaLines = 0.6,                
           # Turn off box shading range
           shadeBox = NULL,                
           # Will normalize each column's values to [0, 1]
           scale = "uniminmax" # try "std" also
)

countyWinner<-countyWinner %>%
  group_by(state) %>%
  summarize(winner = winner[which.max(fraction_votes)],
            fraction_votes = max(fraction_votes),
            votes = max(votes))




trump.records<- results %>%
  filter(candidate=="Donald Trump")

clinton.records<-results %>%
  filter(candidate=="Hillary Clinton")

trump.records<-inner_join(trump.records,county.demo, by="fips")
clinton.records<-inner_join(clinton.records,county.demo, by="fips")

head(clinton.records)

