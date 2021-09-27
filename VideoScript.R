VideoData<-read.csv("~/Desktop/VC Thesis/VideoData.csv")
str(VideoData)
library(tidyverse)
VideoData<-na.omit(VideoData)

PredSum<- VideoData %>%
  group_by(Treatment, Predator) %>%
  summarize(NumberStrike = length(which(Attempt == "Strike")), NumberChase = length(which(Attempt == "Chase")), Miss = length(which(Success == "N")), Success = length(which(Success == "Y")))
PredSum

VideoData$Code<-paste(VideoData$Family,VideoData$Treatment, VideoData$Predator, sep="-")

CodeSummary<-VideoData %>%
  group_by(Code) %>%
  summarize(NumberStrike = length(which(Attempt == "Strike")))
CodeSummary
