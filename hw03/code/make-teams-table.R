# ===================================================================
# Title: Ranking NBA Team
# Description:
#   This script prepares data to be used
# in the ranking analysis
# Input(s): data file 'nba2017-roster.csv','nba2017-stats.csv'
# Output(s): data file 'hw03-josia-yuan.Rmd'
# Author: Josia Yuan
# Date: 10-06-2017
# ===================================================================

library(readr)
library(dplyr)
library(ggplot2)
#Add new Variables
ros<-read.csv('../data/nba2017-roster.csv')
sta<-read.csv('../data/nba2017-stats.csv')
sta<-mutate(sta,missed_fg=field_goals_atts-field_goals_made)
sta<-mutate(sta,missed_ft=points1_atts-points1_made)
sta<-mutate(sta,points=3*points3_made + 2*points2_made + points1_made)
sta<-mutate(sta,rebounds=off_rebounds+def_rebounds)
sta<-mutate(sta,efficiency=(points+rebounds+assists+steals+blocks-missed_fg-missed_ft-turnovers)/games_played)
sink(file='../output/efficiency-summary.txt')
summary(sta$efficiency)
sink()

#Merging data
dat<-merge(sta,ros)
dat$team<-as.character(dat$team)

#Creating nba2017-teams.csv
team_data<-dat %>% group_by(team) %>% summarise(experience=round(mean(experience),digits=2),
                                                salary=round(sum(salary/1000000),digits=2),
                                                points3=sum(points3_made),
                                                points2=sum(points2_made),
                                                free_throws=sum(points1_made),
                                                points=sum(3*points3_made+2*points2_made+points1_made),
                                                off_rebounds=sum(off_rebounds),
                                                def_rebounds=sum(def_rebounds),
                                                assists=sum(assists),
                                                steals=sum(steals),
                                                blocks=sum(blocks),
                                                turnovers=sum(turnovers),
                                                fouls=sum(fouls),
                                                efficiency=sum(efficiency))
summary(team_data)
class(dat$team)
sink(file = '../output/teams-summary.txt')
summary(team_data)
sink()
write.csv(team_data,file = '../data/nba2017-teams.csv')

#Graphics
stars(team_data[,-1],labels=team_data$team)
pdf(file='../images/teams_star_plot.pdf')
stars(team_data[,-1],labels=team_data$team)
dev.off()

ggplot(data=team_data,aes(x=experience,y=salary))+geom_point()+geom_label(aes(label=team_data$team))
pdf(file='../images/experience_salary.pdf')
ggplot(data=team_data,aes(x=experience,y=salary))+geom_point()+geom_label(aes(label=team_data$team))
dev.off()
 