setwd('//Users//sarthakkumarsahu//Documents//Data Analytics//pubg kaggle//pubg data')

library(ggplot2)
library(dplyr)
library(gridExtra)

#reading the file
pubg <- read.csv('train_V2.csv',header = TRUE)

colnames(pubg)

pubg_mst <- pubg 

#As per the data given we need to convert the below values to NA

pubg_mst$killPoints <- ifelse(pubg_mst$rankPoints!=-1 & pubg_mst$killPoints==0,NA,pubg_mst$killPoints)

pubg_mst$winPoints <- ifelse(pubg_mst$winPoints!=-1 & pubg_mst$winPoints==0,NA,pubg_mst$winPoints)

pubg_mst$rankPoints <- replace(pubg_mst$rankPoints, pubg_mst$rankPoints==-1,NA)

pubg_mst$newRank <- ifelse(is.na(pubg_mst$killPoints),pubg_mst$rankPoints,(pubg_mst$killPoints+pubg_mst$winPoints)/2)

check_na <- function(x) {
  round((sum(is.na(x))/nrow(pubg_mst))*100,2) 
}

sapply(pubg_mst, function(x) check_na(x))

summary(pubg_mst)

#Removing killPoints, winPoints and rankPoints

pubg_mst <- subset(pubg_mst, select = -c(killPoints,winPoints,rankPoints))

colnames(pubg_mst)

View(head(pubg_mst$matchDuration))

unique(pubg_mst$matchType)

is.data.frame(pubg_mst)

#Visualization of matchType frequency
ggplot(pubg_mst, aes(matchType)) + stat_count()

colnames(pubg_mst)
summary(pubg_mst)

unique(pubg_mst$assists)

#Visualization of assists
ggplot(pubg_mst, aes(assists)) + geom_histogram()

#Visualization of match duration
ggplot(pubg_mst,aes(matchDuration)) + geom_histogram()

#Match type analysis of all kills
mct_allKills <- pubg_mst %>% group_by(matchType) %>%  summarise(mean_kills = mean(kills), mean_road = mean(roadKills),
                                                                mean_team = mean(teamKills),mean_head = mean(headshotKills))



k1 <- ggplot(mct_allKills, aes(x = matchType,y = mean_kills)) +
  geom_bar(stat = "identity", fill="tomato3") + ylab ("Mean Kills")

k2 <- ggplot(mct_allKills, aes(x = matchType,y = mean_road)) +
  geom_bar(stat = "identity", fill="tomato3") + ylab ("Mean Road Kills")

k3 <- ggplot(mct_allKills, aes(x = matchType,y = mean_team)) +
  geom_bar(stat = "identity", fill="tomato3") + ylab ("Mean Team Kills")

k4 <- ggplot(mct_allKills, aes(x = matchType,y = mean_head)) +
  geom_bar(stat = "identity", fill="tomato3") + ylab("Mean Head Shot Kills")

grid.arrange(k1,k2,k3,k4)

pubg_mst


