# ===================================================================
# Title:Data Preparation for HW04
# Description:
#   This script prepares data to be used
# in hw04
# Input(s): data file 'rawscores.csv'
# Output(s): data file 'hw04-josia-yuan.Rmd'
# Author: Josia Yuan
# Date: 11-11-2017
# ===================================================================

#load packages
library(readr)
library(dplyr)
source("functions.R")
#read rawscores.csv
raw<-read.csv('../data/rawdata/rawscores.csv',stringsAsFactors = FALSE)
head(raw)
sink(file='../output/summary-rawscores.txt')
str(raw)
sink()
for(i in 1:16){
  sink('../output/summary-rawscores.txt',append = TRUE)
  print(summary_stats(raw[,i]))
  print(print_stats(raw[,i]))
  sink()
}


#replace NA with 0
for (i in 1:nrow(raw)) { 
  for (j in 1:ncol(raw)) {
    if (is.na(raw[i,j]) == TRUE) raw[i,j] <- 0
  }
}
raw <- raw


#clean data
#rescale Quiz
raw$QZ1 <- rescale100(raw$QZ1,xmin = 0,xmax = 12)
raw$QZ2 <- rescale100(raw$QZ2,xmin = 0,xmax = 18)
raw$QZ3 <- rescale100(raw$QZ3,xmin = 0,xmax = 20)
raw$QZ4 <- rescale100(raw$QZ4,xmin = 0,xmax = 20)

#rescale to add variable
raw <- mutate(raw,Test1=rescale100(raw$EX1,xmin=0,xmax = 80))
raw <- mutate(raw,Test2=rescale100(raw$EX2,xmin=0,xmax = 90))

#Add Homework
hw <- raw[1:334,1:9]
colnames(hw) <- NULL
hw <- as.matrix(hw)
Homework <- c()
for(i in 1:334){
  Homework <- c(Homework,score_homework(hw[i,]))
}
raw <- mutate(raw, Homework=Homework)

#Add Quiz
qiz <- raw[1:334,11:14]
colnames(qiz) <- NULL
qiz <- as.matrix(qiz)
Quiz <- c()
for(i in 1:334){
  Quiz <- c(Quiz,score_quiz(qiz[i,]))
}
raw <- mutate(raw, Quiz=Quiz)

#Add Lab
Att <- raw[1:334,10]
colnames(Att) <- NULL
Att <- as.matrix(Att)
Lab <- c()
for(i in 1:334){
  Lab <- c(Lab,score_lab(Att[i,]))
}
raw <- mutate(raw, Lab=Lab)

#Add Overall
head(raw)
Overall <- Lab*0.1+0.3*Homework+0.15*Quiz+0.2*raw$Test1+0.25*raw$Test2
raw <- mutate(raw, Overall=Overall)
head(raw)

#Add Grade
raw <- mutate(raw,Grade=c(1:334))
raw$Grade[raw$Overall<50&raw$Overall>=0]<-'F'
raw$Grade[raw$Overall<60&raw$Overall>=50]<-'D'
raw$Grade[raw$Overall<70&raw$Overall>=60]<-'C-'
raw$Grade[raw$Overall<77.5&raw$Overall>=70]<-'C'
raw$Grade[raw$Overall<79.5&raw$Overall>=77.5]<-'C+'
raw$Grade[raw$Overall<82&raw$Overall>=79.5]<-'B-'
raw$Grade[raw$Overall<86&raw$Overall>=82]<-'B'
raw$Grade[raw$Overall<88&raw$Overall>=86]<-'B+'
raw$Grade[raw$Overall<90&raw$Overall>=88]<-'A-'
raw$Grade[raw$Overall<95&raw$Overall>=90]<-'A'
raw$Grade[raw$Overall<=100&raw$Overall>=95]<-'A+'
head(raw)

#Export structure
sink(file='../output/summary-cleanscores.txt')
str(raw)
sink()

#Export summary
needed_data <- raw[,17:22]
filenames<-c("Test1-stats.txt",
             "Test2-stats.txt",
             "Homework-stats.txt",
             "Quiz-stats.txt",
             "Lab-stats.txt",
             "Overall-stats.txt")
for(i in 1:6){
  sink(file = paste("../output/",filenames[i]))
  print(summary_stats(needed_data[,i]))
  print(print_stats(needed_data[,i]))
  sink()
}

#Export clean data frame
write.csv(raw,file='../data/cleandata/cleanscores.csv')