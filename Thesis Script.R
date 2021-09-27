#Photos<-read.csv("~/Desktop/PhotoData.csv")
#Labels<-read.csv("~/Desktop/Annie's Senior Thesis expt labels.csv")

#str(Photos)

#str(Labels)

#Photos$Family<-Labels$Family[match(Photos$Number, Labels$Number)]
#str(Photos)

#Photos$RearingTreatment<-Labels$RearingTreatment[match(Photos$Number, Labels$Number)]

#Photos$Predator<-Labels$Predator[match(Photos$Number, Labels$Number)]
#?write.csv
###write.csv(Photos, file="MatchedPhoto.csv")
Data<-read.csv("~/Desktop/MatchedPhoto.csv")
Data<-na.omit(Data)

library(dplyr)
library(ggplot2)
library(plotrix)
library(tidyr)
library(car)
library(multcomp)
library(lme4)
library(gridExtra)  
 

qplot(x=RearingTreatment, y=TailSpotArea, data=Data)

#qplot(x=RearingTreatment, y=TailSpotArea, data=Photos, fill=Predator)
##### grouping code with predator 
###temp<-aggregate(cbind(TailSpotArea,TailMuscleHeight, TotalLength)~Predator*RearingTreatment*Family, data=Photos, mean)
###temp$Code<-paste(temp$Family,temp$RearingTreatment,temp$Predator, sep="-")

###grouping code with just family and treatment
temp<-aggregate(cbind(TotalLength, BodyLength, 
                      TailLength, MaxTailDepth,TailMuscleHeight, 
                      TailSpotArea, Hue, Chroma, Brightness, HeadWidth, TailMuscleWidth
                      )~RearingTreatment*Family, data=Data, mean)

temp$Code<-paste(temp$Family,temp$RearingTreatment, sep="-")

temp

##Grouping by familly, treatment, predator for survival 
temp2<-aggregate(cbind(TotalLength, BodyLength, 
                      TailLength, MaxTailDepth,TailMuscleHeight, 
                      TailSpotArea, Chroma, Brightness, Hue, HeadWidth, TailMuscleWidth
)~RearingTreatment*Family*Predator, data=Data, mean)

temp2$Code<-paste(temp2$Family,temp2$RearingTreatment, temp2$Predator, sep="-")

temp2

##summarizing means
Summary<-temp2 %>%
group_by(Code) %>%
summarize(TotalLengthMean = mean(TotalLength), BodyLengthMean = mean(BodyLength), 
          TailDepthMean = mean(MaxTailDepth), MuscleHeightMean = mean(TailMuscleHeight),
          TailSpotAreaMean = mean(TailSpotArea), ChromaMean = mean(Chroma), BrightnessMean = mean(Brightness), 
          HueMean = mean(Hue), HeadWidthMean = mean(HeadWidth), MuscleWidthMean = mean(TailMuscleWidth))

Summary

#means by rearing treatment
Summary2<-temp2 %>%
  group_by(RearingTreatment) %>%
  summarize(TotalLengthMean = mean(TotalLength), TLSD = sd(TotalLength), 
            BodyLengthMean = mean(BodyLength), BLSD = sd(BodyLength),
            TailLengthMean = mean(TailLength), TLSD = sd(TailLength),
            TailDepthMean = mean(MaxTailDepth), TDSD = sd(MaxTailDepth),
            MuscleHeightMean = mean(TailMuscleHeight), MHSD = sd(TailMuscleHeight),
            TailSpotAreaMean = mean(TailSpotArea), TSASD = sd(TailSpotArea),
            ChromaMean = mean(Chroma), CHRSD = sd(Chroma),
            HueMean = mean(Hue), HUESD = sd(Hue),
            HeadWidthMean = mean(HeadWidth), HWSD = sd(HeadWidth),
            MuscleWidthMean = mean(TailMuscleWidth), MWSD = sd(TailMuscleWidth))

Summary2

##SUMMARIZING MEANS AND MAKING PLOTS OF ALL THE TRAITS
#Should I use raw or relative traits??

NewSummary<-read.csv("~/Desktop/new.csv")
str(NewSummary)
?col.names
colnames(NewSummary)<-c("Treatment", "Total Length", "Body Length", "Tail Length", "Tail Depth", "Tail Muscle Height",
                           "Tail Spot Area", "Chroma", "Brightness", "Hue", "Head Width", "Tail Muscle Width")

Summary.long<-gather(NewSummary, key="Variable", value="Measurement", "Total Length", 
                   "Body Length", "Tail Length", "Tail Depth", "Tail Muscle Height", 
                   "Tail Spot Area", "Chroma", "Brightness", "Hue", "Head Width", "Tail Muscle Width")

##how to delete titles or x labels, they say the same thing
#ggplot(aes(x=Variable, y=Measurement, fill=Treatment), data=Summary.long)+ 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = c("D" = "red","C" = "grey50", "F" = "blue"))+
  labs(x="", y="")

  
  
#code from justin  CHANGE THESE PLOTS TO RELATIVE MEASURES
  
NewSummary<-read.csv("~/Desktop/new.csv")
  
str(NewSummary)
?ylim
Summary.mean<-group_by(RearingTreatment) %>%
  summarise(surv.mean = mean(Surv3), surv.sd = sd(Surv3), surv.n = length(Surv3)) %>%
  mutate(surv.se = surv.sd/sqrt(surv.n))


  NewSummary$RearingTreatment<-c("Control","Dragonfly","Fish")
  NewSummary$RearingTreatment<-factor(NewSummary$RearingTreatment, levels=c("Dragonfly","Control","Fish"))
  plotCols<-c("Dragonfly" = "red","Control" = "grey50", "Fish" = "blue")
TL<-ggplot(aes(x=RearingTreatment, y=TotalLengthMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=surv.mean-surv.se, ymax=surv.mean+surv.se), position=position_dodge(0.9), width=0.4)+
    coord_cartesian(ylim = c(10,15)) +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="mm") +
    ggtitle("Total Length") + 
    theme(legend.position="none") 
TL
  
BL<-ggplot(aes(x=RearingTreatment, y=BodyLengthMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="") +
    ggtitle("Body Length") +
    theme(legend.position="none")
BL 

  TaL<-ggplot(aes(x=RearingTreatment, y=TailLengthMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="") +
    ggtitle("Tail Length") +
    theme(legend.position="none")
  TaL
  
  TD<-ggplot(aes(x=RearingTreatment, y=TailDepthMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="mm") +
    ggtitle("Maximum Tail Depth") +
    theme(legend.position="none")
  TD
  TMH<-ggplot(aes(x=RearingTreatment, y=MuscleHeightMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="") +
    ggtitle("Tail Muscle Height") +
    theme(legend.position="none")
  TMH
 
  TSA<-ggplot(aes(x=RearingTreatment, y=TailSpotAreaMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="") +
    ggtitle("Tail Spot Area") +
    theme(legend.position="none")
  TSA
  
  CHR<-ggplot(aes(x=RearingTreatment, y=ChromaMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="") +
    ggtitle("Chroma") +
    theme(legend.position="none")
  CHR
  
  BRI<-ggplot(aes(x=RearingTreatment, y=BrightnessMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="") +
    ggtitle("Brightness") +
    theme(legend.position="none")
  BRI
  
  HUE<-ggplot(aes(x=RearingTreatment, y=HueMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="") +
    ggtitle("Hue") +
    theme(legend.position="none")
  HUE
  
  HWM<-ggplot(aes(x=RearingTreatment, y=HeadWidthMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="mm") +
    ggtitle("Maximum Head Width") +
    theme(legend.position="none")
  HWM
  
  MWM<-ggplot(aes(x=RearingTreatment, y=MuscleWidthMean, fill=RearingTreatment), data=NewSummary)+ 
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = plotCols)+
    labs(x="", y="") +
    ggtitle("Tail Muscle Width") +
    theme(legend.position="none")
  MWM
  
grid.arrange(TL, BL, TaL, TD, TMH, TSA, HWM, MWM, CHR, HUE, nrow=2, ncol=5) 

  
  
  
qplot(x=Code, y=TotalLengthMean, data=Summary)
qplot(x=Code, y=TailSpotAreaMean, data=Summary)
qplot(x=Code, y=BrightnessMean, data=Summary)
qplot(x=Code, y=ChromaMean, data=Summary)



qplot(x=RearingTreatment, y=TotalLength, data=Data)
qplot(x=RearingTreatment, y=BodyLength, data=Data)
qplot(x=RearingTreatment, y=TailLength, data=Data)
qplot(x=RearingTreatment, y=TailMuscleHeight, data=Data)
qplot(x=RearingTreatment, y=Chroma, data=Data)
qplot(x=RearingTreatment, y=Brightness, data=Data)
qplot(x=RearingTreatment, y=Red, data=Data)
qplot(x=RearingTreatment, y=Green, data=Data)
qplot(x=RearingTreatment, y=Blue, data=Data)
qplot(x=RearingTreatment, y=TailMuscleWidth, data=Data)
qplot(x=RearingTreatment, y=MaxTailDepth, data=Data)

##Data<-droplevels(na.omit(Data))

##mixed effects body length
#lmm1<-lmer(BodyLength~RearingTreatment+(1|Family), data=Data)
#lmm2<-lmer(BodyLength~1+(1|Family), data=Data)
#anova(lmm1, lmm2)
#qqnorm(resid(lmm1))
#ph1<-glht(lmm1, linfct=mcp(RearingTreatment="Tukey"))
#summary(ph1)

##body length and treatment

#lmm3<-lmer(MaxTailDepth~BodyLength*RearingTreatment+(1|Family), data=Data)
#lmm4<-lmer(MaxTailDepth~BodyLength+RearingTreatment+(1|Family), data=Data)
#lmm5<-lmer(MaxTailDepth~BodyLength+(1|Family), data=Data)
#lmm6<-lmer(MaxTailDepth~RearingTreatment+(1|Family), data=Data)
#anova(lmm3, lmm4) #significance of interaction btwn body length and treatment
#anova(lmm4, lmm6) #effect of body length
#anova(lmm4, lmm5) #effect of treatment


##how to change color?????
qplot(BodyLength, TailMuscleHeight, data=Data, col=RearingTreatment) +geom_smooth(method=lm)
qplot(BodyLength, TailLength, data=Data, col=RearingTreatment) + geom_smooth(method=lm)
qplot(BodyLength, TotalLength, data=Data, col=RearingTreatment) + geom_smooth(method=lm)
qplot(BodyLength, TailMuscleWidth, data=Data, col=RearingTreatment) + geom_smooth(method=lm)
qplot(TailSpotArea, Chroma, data=Data, col=RearingTreatment) + geom_smooth(method=lm)
qplot(TailSpotArea, Brightness, data=Data, col=RearingTreatment, xlab="Tail Spot Area") + geom_smooth(method=lm)

#here i got the color to change but not the line color!!!!!
ggplot(data=Data, aes(x=BodyLength, y=TailMuscleHeight, fill=RearingTreatment)) +
  geom_smooth(method = lm, color="grey50") +
  scale_fill_manual("Rearing Treatment", values = c("D" = "red","C" = "grey50", "F" = "blue"), 
  labels=c("Control", "Dragonfly", "Fish")) +
  labs(y="Tail Muscle Height", x="Body Length")


#New plots
#qplot(as.factor(Family), RelArea, data=Data, geom="boxplot", fill=RearingTreatment)
#qplot(RearingTreatment,RelTailLength, geom="boxplot", data=Data)
#qplot(RearingTreatment, RelMaxTailDepth, data=Data)
Data$Survival2<-Data$Survival
Data$Survival2<-as.factor(ifelse(Data$Survival2=="TAIL" |Data$Survival2=="Y", "Y", "N"))
Data$Survival3<-as.numeric(Data$Survival2)-1
str(Data)
Data

sum(is.na(Data$Survival))

#Significant effect of predator, and of interaction (within predator treatments, the rearing env matters)
glmer1<-glmer(Survival2~RearingTreatment*Predator+(1|Family), family="binomial", data=Data)
summary(glmer1)
Anova(glmer1)

#within fish treatments: Significant effect of rearing treatment. Significant difference between D and C,
#F and D, but not F and C
glmer2<-glmer(Survival2~RearingTreatment+(1|Family), family="binomial", data=Data[Data$Predator=="F",])
Anova(glmer2)
phfish<-glht(glmer2, linfct=mcp(RearingTreatment="Tukey"))
summary(phfish)

#not significant effect of rearing treatment in dragonfly predation, but there is a trend
#no trends between D - C and F - C but a close to significant trend between F - D
temp<-droplevels(Data[Data$RearingTreatment!="C",])

glmer3<-glmer(Survival2~RearingTreatment+(1|Family), family="binomial", data=temp[temp$Predator=="D",])
Anova(glmer3)
phdragonfly<-glht(glmer3, linfct=mcp(RearingTreatment="Tukey"))
summary(phdragonfly)

glmer3<-glmer(Survival2~RearingTreatment+(1|Family), family="binomial", data=Data[Data$Predator=="D",])
Anova(glmer3)
phdragonfly<-glht(glmer3, linfct=mcp(RearingTreatment="Tukey"))
summary(phdragonfly)


#what are these. logit transformation.takes proportion and transforms it into normal scale. strong family effects (random effects)
#these effects are not captured in the raw values. These numbers reflect what the model is doing. These values show the true proportion of survival when accounting for family effects.
#plogis(2.5251)#control predator D
#plogis(2.5251+0.1777)#dragonfly predator D
#plogis(2.5251-0.7822)#fish predator D
#plogis(2.5251-2.5373)#control predator F
#plogis(2.5251-2.5373-1.1376)#dragonfly predator F
#plogis(2.5251-2.5373+1.0385)#fish predator F

#survival percentages
aggregate(Survival3~RearingTreatment*Predator, Data, mean)


#qplot(x=RelMaxTailDepth, data=temp, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator)
#qplot(x=RelArea, data=temp, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator)
#qplot(x=RelTailMuscle, data=temp, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator)
#qplot(x=RelTailLength, data=temp, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator)

#Survival plots of different traits
qplot(x=MaxTailDepth, data=Data, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator)
qplot(x=TailSpotArea, data=Data, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator)
qplot(x=MaxTailDepth, data=Data, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator)
qplot(x=TailMuscleHeight, data=Data, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator)
qplot(x=TailLength, data=Data, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator)

#not enough that got eaten for dragonflies. No patterns in the fish data. 
#use tail muscle height as response variable
#lmm1<-lmer(TailMuscleHeight~Survival2+(1|Family), data=Data[Data$Predator=="F",])
#summary(lmm1)
#Anova(lmm1)
#aggregate(TailMuscleHeight~Survival2, data=Data[Data$Predator=="F",], mean)
#qplot(x=Survival2, y=TailMuscleHeight, geom="boxplot", data=Data[Data$Predator=="F",])

#lmm1<-lmer(MaxTailDepth~Survival2*RearingTreatment+(1|Family), data=Data[Data$Predator=="F",])
#summary(lmm1)
#Anova(lmm1)
#qplot(x=Survival2, y=MaxTailDepth, geom="boxplot", data=Data[Data$Predator=="F",])

#glm1<-glmer(Survival2~TailSpotArea*RearingTreatment+(1|Family), family="binomial", data=Data[Data$Predator=="F",])
#summary(glm1)
#Anova(glm1)
#qplot(x=TailSpotArea, y=Survival3, col=RearingTreatment, data=Data[Data$Predator=="F",]) + 
  geom_smooth(method=glm, method.args=list(family = "binomial"))

glm1<-glmer(Survival2~TailSpotArea+(1|Family), family="binomial", data=Data[Data$Predator=="F" & Data$RearingTreatment=="D",])
glm2<-glmer(Survival2~sqrt(TailSpotArea)+(1|Family), family="binomial", data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
glm3<-glmer(Survival2~sqrt(TailSpotArea)+(1|Family), family="binomial", data=Data[Data$Predator=="F" & Data$RearingTreatment=="F",])
Anova(glm1);Anova(glm2);Anova(glm3)


 #tailspotarea in controls
glm2<-glmer(Survival2~sqrt(TailSpotArea)*PC1+(1|Family), family="binomial", data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm2)
Anova(glm2)

?hist

controls<-subset(Data, RearingTreatment == "C")
controls

hist(controls$TailSpotArea)

hist(sqrt(controls$TailSpotArea))

#max tail depth in controls
hist(controls$MaxTailDepth)

glm3<-glmer(Survival2~MaxTailDepth*PC1+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm3)
Anova(glm3)

#tail muscle height in controls
hist(controls$TailMuscleHeight)
glm4<-glmer(Survival2~TailMuscleHeight*PC1+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm4)
Anova(glm4)

#total length in controls
hist(controls$TotalLength)
glm5<-glmer(Survival2~TotalLength*PC1+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm5)
Anova(glm5)

#hue controls
hist(sqrt(controls$Hue))

glm6<-glmer(Survival2~sqrt(Hue)*TailSpotArea+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm6)
Anova(glm6)

#chroma controls Getting warnings for this one when I dont use sqrt
hist(controls$Chroma)
glm7<-glmer(Survival2~sqrt(Chroma)*TailSpotArea+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm7)
Anova(glm7)

#brightness controls NOT NORMAL!!!!
hist(controls$Brightness)
hist(sqrt(controls$Brightness))

glm8<-glmer(Survival2~sqrt(Brightness)*TailSpotArea+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm8)
Anova(glm8)

#head width controls
hist(controls$HeadWidth)
glm9<-glmer(Survival2~HeadWidth*PC1+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm9)
Anova(glm9)
 #tail muscle width controls
hist(controls$TailMuscleWidth)
glm10<-glmer(Survival2~TailMuscleWidth*PC1+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm10)
Anova(glm10)





############STOP###################
##PCA

PCA1<-
  prcomp(~TotalLength+BodyLength+TailLength+MaxTailDepth+TailMuscleHeight+HeadWidth+TailMuscleWidth
         , data=Data, scale=T)
Data$PC1<--PCA1$x[,1]
#tail spot area
lmm1<-lmer(TailSpotArea~PC1*RearingTreatment+(1|Family), data=Data)
lmm1
ph1<-glht(lmm1, linfct=mcp(RearingTreatment="Tukey"))
summary(ph1)

pcsum<-Data %>%
  group_by(RearingTreatment) %>%
  summarize(MeanPC = mean(PC1))

pcsum

#total length
lmm2<-lmer(TotalLength~PC1*RearingTreatment+(1|Family), data=Data)
ph2<-glht(lmm2, linfct=mcp(RearingTreatment="Tukey"))
summary(ph2)

#body length
lmm3<-lmer(BodyLength~PC1*RearingTreatment+(1|Family), data=Data)
ph3<-glht(lmm3, linfct=mcp(RearingTreatment="Tukey"))
summary(ph3)

#tail depth
lmm4<-lmer(MaxTailDepth~PC1*RearingTreatment+(1|Family), data=Data)
ph4<-glht(lmm4, linfct=mcp(RearingTreatment="Tukey"))
summary(ph4)

#tail muscle height
lmm5<-lmer(TailMuscleHeight~PC1*RearingTreatment+(1|Family), data=Data)
ph5<-glht(lmm5, linfct=mcp(RearingTreatment="Tukey"))
summary(ph5)

#chroma use tail spot area as covariate
lmm6<-lmer(Chroma~RearingTreatment+(1|Family), data=Data)
ph6<-glht(lmm6, linfct=mcp(RearingTreatment="Tukey"))
summary(ph6)

#brightness use tail spot area as covariate
lmm7<-lmer(Brightness~RearingTreatment+(1|Family), data=Data)
ph7<-glht(lmm7, linfct=mcp(RearingTreatment="Tukey"))
summary(ph7)

#hue
lmm8<-lmer(Hue~RearingTreatment+(1|Family), data=Data)
ph8<-glht(lmm8, linfct=mcp(RearingTreatment="Tukey"))
summary(ph8)

#head width
lmm9<-lmer(HeadWidth~PC1*RearingTreatment+(1|Family), data=Data)
ph9<-glht(lmm9, linfct=mcp(RearingTreatment="Tukey"))
summary(ph9)

#muscle width
lmm10<-lmer(TailMuscleWidth~PC1*RearingTreatment+(1|Family), data=Data)
ph10<-glht(lmm10, linfct=mcp(RearingTreatment="Tukey"))
summary(ph10)




