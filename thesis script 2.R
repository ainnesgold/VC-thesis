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
library(micompr)


Data$RelTotalLength<-Data$TotalLength/Data$BodyLength
Data$RelTailLength<-Data$TailLength/Data$BodyLength
Data$RelMaxTailDepth<-Data$MaxTailDepth/Data$BodyLength
Data$RelTailMuscleHeight<-Data$TailMuscleHeight/Data$BodyLength
Data$RelTailSpotArea<-Data$TailSpotArea/Data$BodyLength
Data$RelHeadWidth<-Data$HeadWidth/Data$BodyLength
Data$RelTailMuscleWidth<-Data$TailMuscleWidth/Data$BodyLength

##summarize relative means
###RelMeans<-Data %>%
  group_by(RearingTreatment) %>%
  summarize(TotalLengthMean = mean(TotalLength), BodyLengthMean = mean(BodyLength), 
            RelTailLengthMean = mean(RelTailLength), 
            RelTailDepthMean = mean(RelMaxTailDepth), RelMuscleHeightMean = mean(RelTailMuscleHeight),
            RelTailSpotAreaMean = mean(RelTailSpotArea), ChromaMean = mean(Chroma), 
            HueMean = mean(Hue), RelHeadWidthMean = mean(RelHeadWidth), RelMuscleWidthMean = mean(RelTailMuscleWidth))
RelMeans<-Data %>%
  group_by(RearingTreatment) %>%
  summarize(TotalLengthMean = mean(TotalLength), TLSE = std.error(TotalLength), 
            BodyLengthMean = mean(BodyLength), BLSE = std.error(BodyLength),
            RelTailLengthMean = mean(RelTailLength), TaLSE = std.error(RelTailLength),
            RelTailDepthMean = mean(RelMaxTailDepth), TDSE = std.error(RelMaxTailDepth),
            RelMuscleHeightMean = mean(RelTailMuscleHeight), MHSE = std.error(RelTailMuscleHeight),
            RelTailSpotAreaMean = mean(RelTailSpotArea), TSASE = std.error(RelTailSpotArea),
            ChromaMean = mean(Chroma), CHRSE = std.error(Chroma),
            HueMean = mean(Hue), HUESE = std.error(Hue),
            RelHeadWidthMean = mean(RelHeadWidth), HWSE = std.error(RelHeadWidth),
            RelMuscleWidthMean = mean(RelTailMuscleWidth), MWSE = std.error(RelTailMuscleWidth))

#change to relative traits
#coord_cartesian(ylim = c(10,15)) +
RelMeans$RearingTreatment<-c("Control","Dragonfly","Fish")
RelMeans$RearingTreatment<-factor(RelMeans$RearingTreatment, levels=c("Dragonfly","Control","Fish"))

TL<-ggplot(aes(x=RearingTreatment, y=TotalLengthMean, fill=RearingTreatment), data=RelMeans)+ 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=TotalLengthMean-TLSE, ymax=TotalLengthMean+TLSE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(12,15)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="mm") +
  ggtitle("Total Length") + 
  theme(legend.position="none") 
TL

BL<-ggplot(aes(x=RearingTreatment, y=BodyLengthMean, fill=RearingTreatment), data=RelMeans)+ 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=BodyLengthMean-BLSE, ymax=BodyLengthMean+BLSE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(4,5.5)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="mm") +
  ggtitle("Body Length") +
  theme(legend.position="none")
BL 

RTaL<-ggplot(aes(x=RearingTreatment, y=RelTailLengthMean, fill=RearingTreatment), data=RelMeans) + 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=RelTailLengthMean-TaLSE, ymax=RelTailLengthMean+TaLSE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(1.75,1.85)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="") +
  ggtitle("Relative Tail Length") +
  theme(legend.position="none")
RTaL

RTD<-ggplot(aes(x=RearingTreatment, y=RelTailDepthMean, fill=RearingTreatment), data=RelMeans)+ 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=RelTailDepthMean-TDSE, ymax=RelTailDepthMean+TDSE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(0.5,0.7)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="") +
  ggtitle("Relative Maximum Tail Depth") +
  theme(legend.position="none")
RTD

RTMH<-ggplot(aes(x=RearingTreatment, y=RelMuscleHeightMean, fill=RearingTreatment), data=RelMeans)+ 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=RelMuscleHeightMean-MHSE, ymax=RelMuscleHeightMean+MHSE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(0.2,0.26)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="") +
  ggtitle("Relative Tail Muscle Height") +
  theme(legend.position="none")
RTMH

RTSA<-ggplot(aes(x=RearingTreatment, y=RelTailSpotAreaMean, fill=RearingTreatment), data=RelMeans)+ 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=RelTailSpotAreaMean-TSASE, ymax=RelTailSpotAreaMean+TSASE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(0,1.2)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="") +
  ggtitle("Relative Tail Spot Area") +
  theme(legend.position="none")
RTSA

CHR<-ggplot(aes(x=RearingTreatment, y=ChromaMean, fill=RearingTreatment), data=RelMeans)+ 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=ChromaMean-CHRSE, ymax=ChromaMean+CHRSE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(60,150)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="") +
  ggtitle("Chroma") +
  theme(legend.position="none")
CHR


HUE<-ggplot(aes(x=RearingTreatment, y=HueMean, fill=RearingTreatment), data=RelMeans)+ 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=HueMean-HUESE, ymax=HueMean+HUESE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(17,21)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="") +
  ggtitle("Hue") +
  theme(legend.position="none")
HUE

RHWM<-ggplot(aes(x=RearingTreatment, y=RelHeadWidthMean, fill=RearingTreatment), data=RelMeans)+ 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=RelHeadWidthMean-HWSE, ymax=RelHeadWidthMean+HWSE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(0.5,0.6)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="") +
  ggtitle("Relative Maximum Head Width") +
  theme(legend.position="none")
RHWM

RMWM<-ggplot(aes(x=RearingTreatment, y=RelMuscleWidthMean, fill=RearingTreatment), data=RelMeans)+ 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=RelMuscleWidthMean-MWSE, ymax=RelMuscleWidthMean+MWSE), 
                position=position_dodge(0.9), width=0.4)+
  coord_cartesian(ylim = c(0.18,0.2)) +
  scale_fill_manual(values = plotCols)+
  labs(x="", y="") +
  ggtitle("Relative Tail Muscle Width") +
  theme(legend.position="none")
RMWM

#all mean plots together
grid.arrange(TL, BL, RTaL, RTD, RTMH, RTSA, RHWM, RMWM, CHR, HUE,  nrow=2, ncol=5) 




#LINEAR MIXED EFFECT MODELS SHOWING EFFECT OF TREATMENT ON TRAITS
#tail spot area
lmm1<-lmer(RelTailSpotArea~RearingTreatment+(1|Family), data=Data)
lmm1
ph1<-glht(lmm1, linfct=mcp(RearingTreatment="Tukey"))
summary(ph1)
cld(ph1)


#total length
lmm2<-lmer(TotalLength~RearingTreatment+(1|Family), data=Data)
ph2<-glht(lmm2, linfct=mcp(RearingTreatment="Tukey"))
summary(ph2)
cld(ph2)

#body length
lmm3<-lmer(BodyLength~RearingTreatment+(1|Family), data=Data)
ph3<-glht(lmm3, linfct=mcp(RearingTreatment="Tukey"))
summary(ph3)
cld(ph3)

lmmtail<-lmer(RelTailLength~RearingTreatment+(1|Family), data=Data)
phtail<-glht(lmmtail, linfct=mcp(RearingTreatment="Tukey"))
summary(phtail)
cld(phtail)

#rel tail depth
lmm4<-lmer(RelMaxTailDepth~RearingTreatment+(1|Family), data=Data)
ph4<-glht(lmm4, linfct=mcp(RearingTreatment="Tukey"))
summary(ph4)
cld(ph4)
#rel tail muscle height
lmm5<-lmer(RelTailMuscleHeight~RearingTreatment+(1|Family), data=Data)
ph5<-glht(lmm5, linfct=mcp(RearingTreatment="Tukey"))
summary(ph5)
cld(ph5)
#chroma use tail spot area as covariate WARNING
lmm6<-lmer(Chroma~RearingTreatment*sqrt(TailSpotArea)+(1|Family), data=Data)
ph6<-glht(lmm6, linfct=mcp(RearingTreatment="Tukey"))
summary(ph6)
qplot(x=sqrt(TailSpotArea),y=Chroma, col=RearingTreatment, data=Data)+geom_smooth(method="lm")

lmm6<-lmer(Chroma~RearingTreatment+(1|Family), data=Data)
ph6<-glht(lmm6, linfct=mcp(RearingTreatment="Tukey"))
summary(ph6)
qplot(x=RearingTreatment,y=Chroma, data=Data, geom="boxplot")
cld(ph6)


#brightness use tail spot area as covariate
#lmm7<-lmer(Brightness~RearingTreatment*TailSpotArea+(1|Family), data=Data)
#ph7<-glht(lmm7, linfct=mcp(RearingTreatment="Tukey"))
#summary(ph7)

#hue ????? WARNING
lmm8<-lmer(Hue~RearingTreatment+(1|Family), data=Data)
Anova(lmm8)
ph8<-glht(lmm8, linfct=mcp(RearingTreatment="Tukey"))
summary(ph8)
cld(ph8)

#head width
lmm9<-lmer(RelHeadWidth~RearingTreatment+(1|Family), data=Data)
ph9<-glht(lmm9, linfct=mcp(RearingTreatment="Tukey"))
summary(ph9)
cld(ph9)
#muscle width
lmm10<-lmer(RelTailMuscleWidth~RearingTreatment+(1|Family), data=Data)
ph10<-glht(lmm10, linfct=mcp(RearingTreatment="Tukey"))
summary(ph10)
cld(ph10)

##GLMs FOR CONTROLS IN FISH TREATMENT ONLY

binom.test(c(19,8))


glm2<-glmer(Survival~sqrt(RelTailSpotArea)+(1|Family), 
            family="binomial", data=Data[Data$Predator=="D",])
summary(glm2)
Anova(glm2)

#subset data for just N and TAIL
# not working Data$Survival<-factor(Data$Survival, levels=c("No", "Tail Damage", "Yes"))
qplot(x=Survival, y=sqrt(TailSpotArea), data=Data, geom="boxplot", fill=Survival)


Data$Survival2<-as.factor(ifelse(Data$Survival2=="TAIL" |Data$Survival2=="Y", "Y", "N"))
Data$Survival3<-as.numeric(Data$Survival2)-1

#tailspotarea in controls
controls<-subset(Data, RearingTreatment == "C")
hist(controls$RelTailSpotArea)
hist(sqrt(controls$RelTailSpotArea))

glm2<-glmer(Survival2~sqrt(RelTailSpotArea)+(1|Family), family="binomial", data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm2)
Anova(glm2)

qplot(x=sqrt(RelTailSpotArea), data=Data, geom="density", fill=RearingTreatment, alpha=0.5)


#max tail depth in controls
hist(controls$RelMaxTailDepth)

glm3<-glmer(Survival2~RelMaxTailDepth+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm3)
Anova(glm3)

#tail muscle height in controls
hist(controls$RelTailMuscleHeight)
glm4<-glmer(Survival2~RelTailMuscleHeight+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm4)
Anova(glm4)

#total length in controls
hist(sqrt(controls$RelTotalLength))
glm5<-glmer(Survival2~sqrt(RelTotalLength)+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm5)
Anova(glm5)

#hue controls
hist(sqrt(controls$Hue))

glm6<-glmer(Survival2~sqrt(Hue)+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm6)
Anova(glm6)

#chroma controls Getting warnings for this one when I dont use sqrt
hist(controls$Chroma)
glm7<-glmer(Survival2~sqrt(Chroma)+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm7)
Anova(glm7)

#brightness controls NOT NORMAL!!!!
hist(controls$Brightness)
hist(sqrt(controls$Brightness))

glm8<-glmer(Survival2~sqrt(Brightness)+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm8)
Anova(glm8)

#head width controls
hist(sqrt(controls$RelHeadWidth))
glm9<-glmer(Survival2~sqrt(RelHeadWidth)+(1|Family), family="binomial", 
            data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm9)
Anova(glm9)
#tail muscle width controls
hist(controls$RelTailMuscleWidth)
glm10<-glmer(Survival2~RelTailMuscleWidth+(1|Family), family="binomial", 
             data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm10)
Anova(glm10)


#body length
hist(controls$BodyLength)
glm10<-glmer(Survival2~BodyLength+(1|Family), family="binomial", 
             data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm10)
Anova(glm10)
#rel tail length
hist(controls$RelTailLength)
glm10<-glmer(Survival2~RelTailLength+(1|Family), family="binomial", 
             data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",])
summary(glm10)
Anova(glm10)


##Survival density graphs of relative traits
Data$Survival<-as.factor(ifelse(Data$Survival2=="TAIL" |Data$Survival2=="Y", "Y", "N"))
Data$Survival3<-as.numeric(Data$Survival2)-1

##get rid of 0.5 legend

QTL<-qplot(x=TotalLength, data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",],
           fill=Survival, geom="density", 
      alpha=0.5, xlab="Total Length", ylab="Density", show.legend=F)
QTL

QBL<-qplot(x=BodyLength, data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",], 
           fill=Survival, geom="density", alpha=0.5,
           xlab="Body Length", ylab="", show.legend=F)
QBL
QRTL<-qplot(x=RelTailLength, data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",], 
            fill=Survival, geom="density", alpha=0.5,
            xlab="Relative Tail Length", ylab="", show.legend=F)
QRTL

QRTMH<-qplot(x=RelTailMuscleHeight, data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",], 
             fill=Survival, geom="density", alpha=0.5,
             xlab="Relative Tail Muscle Height", ylab="", show.legend=F)
QRTMH
#all treatments
plot1<-qplot(x=RelMaxTailDepth, data=Data, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator,
      xlab="Relative Maximum Tail Depth", ylab="", show.legend=F)

QRMTD<-qplot(x=RelMaxTailDepth, data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",], 
             fill=Survival, geom="density", alpha=0.5,
             xlab="Relative Maximum Tail Depth", ylab="", show.legend=F)
QRMTD
#rel tail spot area in all treatments
plot2<-qplot(x=RelTailSpotArea, data=Data, fill=Survival2, geom="density", alpha=0.5, facets = .~Predator,
      xlab="Relative Tail Spot Area", ylab="Density", show.legend=F)

grid.arrange(plot2, plot1, nrow=1, ncol=2)

#just controls
QRTSA<-qplot(x=RelTailSpotArea, data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",], 
             fill=Survival, geom="density", alpha=0.5,
             xlab="Relative Tail Spot Area", ylab="Density", show.legend=F)
QRTSA
QRHW<-qplot(x=RelHeadWidth, data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",], 
            fill=Survival, geom="density", alpha=0.5,
            xlab="Relative Head Width", ylab="", show.legend=F)
QRHW
QRTMW<-qplot(x=RelTailMuscleWidth, data=Data[Data$Predator=="F" &
            Data$RearingTreatment=="C",], fill=Survival, geom="density", alpha=0.5,
             xlab="Relative Tail Muscle Width", ylab="", show.legend=F)
QRTMW

QCHR<-qplot(x=Chroma, data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",],
            fill=Survival, geom="density", alpha=0.5,
            xlab="Chroma", ylab="", show.legend=F)
QCHR
QHUE<-qplot(x=Hue, data=Data[Data$Predator=="F" & Data$RearingTreatment=="C",], 
            fill=Survival, geom="density", alpha=0.5,
            xlab="Hue", show.legend=F)
QHUE


grid.arrange(QTL, QBL, QRTL, QRTMH, QRMTD, QRTSA, QRHW, QRTMW, QCHR, QHUE, nrow=2, ncol=5)

Data$RearingTreatment<-factor(Data$RearingTreatment, levels=c("D", "C", "F"))
                                
##create family means figures
FamilyMeans<-aggregate(cbind(TotalLength, BodyLength, RelTailLength, RelMaxTailDepth, RelTailMuscleHeight,
                             RelTailSpotArea, Hue, Chroma, RelHeadWidth, RelTailMuscleWidth, RelTailSpotArea, 
                             RelTailLength, RelMaxTailDepth, 
                             RelTailMuscleHeight)~Family*RearingTreatment, mean, data=Data)
#chroma

Data.sum<-aggregate(cbind(TotalLength, BodyLength,RelTailLength, RelMaxTailDepth, 
                          RelTailMuscleHeight, RelHeadWidth, RelTailMuscleWidth,
                          RelTailSpotArea, Chroma, Hue)~RearingTreatment*Family, data=Data, mean)
Data.sum$Family<-as.factor(Data.sum$Family)

#write.csv(Data.sum, file="Datasum.csv")
Datasum<-read.csv("~/Desktop/Datasum.csv")
Datasum$RearingTreatment<-factor(Datasum$RearingTreatment, levels=c("Dragonfly", "Control", "Fish"))

Datasum$Family<-as.factor(Datasum$Family)



familyBL<-ggplot(data=Datasum, aes(x = RearingTreatment, y = BodyLength, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Body Length (mm)", x="Rearing Treatment")
familyBL

familyTL<-ggplot(data=Datasum, aes(x = RearingTreatment, y = TotalLength, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Total Length (mm)", x="Rearing Treatment")
familyTL

familyTaL<-ggplot(data=Datasum, aes(x = RearingTreatment, y = RelTailLength, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Relative Tail Length", x="Rearing Treatment")
familyTaL

familyTD<-ggplot(data=Datasum, aes(x = RearingTreatment, y = RelMaxTailDepth, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Relative Tail Depth", x="Rearing Treatment")
familyTD

familyTMH<-ggplot(data=Datasum, aes(x = RearingTreatment, y = RelTailMuscleHeight, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Relative Tail Muscle Height", x="Rearing Treatment")
familyTMH

familyHW<-ggplot(data=Datasum, aes(x = RearingTreatment, y = RelHeadWidth, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Relative Head Width", x="Rearing Treatment")
familyHW

familyTMW<-ggplot(data=Datasum, aes(x = RearingTreatment, y = RelTailMuscleWidth, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Relative Tail Muscle Width", x="Rearing Treatment")
familyTMW

familyTSA<-ggplot(data=Datasum, aes(x = RearingTreatment, y = RelTailSpotArea, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Relative Tail Spot Area", x="Rearing Treatment")
familyTSA

familyCHR<-ggplot(data=Datasum, aes(x = RearingTreatment, y = Chroma, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Chroma", x="Rearing Treatment")
familyTMH

familyHUE<-ggplot(data=Datasum, aes(x = RearingTreatment, y = Hue, group=Family, col= Family))+
  geom_line()+
  theme(legend.position="none")+
  labs(y="Hue", x="Rearing Treatment")
familyHUE

grid.arrange(familyTL, familyBL, familyTaL, familyTMH, familyTD, 
             familyTSA, familyHW, familyTMW, familyCHR, familyHUE, nrow=5, ncol=2)

















##########STOP







FamilyMeans

plot(Chroma~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,], 
     type="l", ylim=c(min(FamilyMeans$Chroma), max(FamilyMeans$Chroma)), xaxt="n", las=1, 
     main="Chroma", ylab="", xlab="Rearing Treatment")
axis(side=1, at=1:3, labels=c("Dragonfly", "Control", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(Chroma~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}

##plot each family total length
plot(TotalLength~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,], 
     type="l", ylim=c(min(FamilyMeans$TotalLength), max(FamilyMeans$TotalLength)), 
     xaxt="n", las=1, main="Total Length", ylab="mm", xlab="Rearing Treatment")
axis(side=1, at=1:3, labels=c("Control", "Dragonfly", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(TotalLength~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}

##plot each family body length
plot(BodyLength~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,],
     type="l", ylim=c(min(FamilyMeans$BodyLength), max(FamilyMeans$BodyLength)), 
     xaxt="n", las=1, main="Body Length", ylab="mm", xlab="Treatment")
axis(side=1, at=1:3, labels=c("Control", "Dragonfly", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(BodyLength~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}

##plot each family tail length
plot(RelTailLength~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,], 
     type="l", ylim=c(min(FamilyMeans$RelTailLength), max(FamilyMeans$RelTailLength)), 
     xaxt="n", las=1, main="Relative Tail Length", ylab="", xlab="Rearing Treatment")
axis(side=1, at=1:3, labels=c("Control", "Dragonfly", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(RelTailLength~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}

##plot each family MaxTailDepth
plot(RelMaxTailDepth~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,], 
     type="l", ylim=c(min(FamilyMeans$RelMaxTailDepth), max(FamilyMeans$RelMaxTailDepth)), 
     xaxt="n", las=1, main="Relative Maximum Tail Depth", ylab="", xlab="Rearing Treatment")
axis(side=1, at=1:3, labels=c("Control", "Dragonfly", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(RelMaxTailDepth~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}

##plot each family TailMuscleHeight
plot(RelTailMuscleHeight~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,], 
     type="l", ylim=c(min(FamilyMeans$RelTailMuscleHeight), max(FamilyMeans$RelTailMuscleHeight)), 
     xaxt="n", las=1, main="Relative Tail Muscle Height", ylab="", xlab="Rearing Treatment")
axis(side=1, at=1:3, labels=c("Control", "Dragonfly", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(RelTailMuscleHeight~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}

##plot each family TailSpotArea
par(mai=c(1,1,0.8,0.4))
plot(RelTailSpotArea~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,], 
     type="l", ylim=c(min(FamilyMeans$RelTailSpotArea), max(FamilyMeans$RelTailSpotArea)), 
     xaxt="n", las=1, main="Relative Tail Spot Area", 
     ylab="", xlab="Rearing Treatment")
axis(side=1, at=1:3, labels=c("Control", "Dragonfly", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(RelTailSpotArea~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}

#ylab=expression("" ~ mm^{2}),

##plot each family Hue
plot(Hue~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,], type="l", 
     ylim=c(min(FamilyMeans$Hue), max(FamilyMeans$Hue)), xaxt="n", las=1, 
     main="Hue", ylab="", xlab="Rearing Treatment")
axis(side=1, at=1:3, labels=c("Control", "Dragonfly", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(Hue~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}


##plot each family Head width
plot(RelHeadWidth~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,], 
     type="l", ylim=c(min(FamilyMeans$RelHeadWidth), max(FamilyMeans$RelHeadWidth)), xaxt="n",
     las=1, main="Relative Head Width", ylab="", xlab="Rearing Treatment")
axis(side=1, at=1:3, labels=c("Control", "Dragonfly", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(RelHeadWidth~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}

##plot each family Tail Muscle Width
plot(RelTailMuscleWidth~as.numeric(RearingTreatment), data=FamilyMeans[FamilyMeans$Family==1,], 
     type="l", ylim=c(min(FamilyMeans$RelTailMuscleWidth), max(FamilyMeans$RelTailMuscleWidth)),
     xaxt="n", las=1, main="Relative Tail Muscle Width", ylab="", xlab="Rearing Treatment")
axis(side=1, at=1:3, labels=c("Control", "Dragonfly", "Fish"))
for(i in unique(FamilyMeans$Family)){lines(RelTailMuscleWidth~as.numeric(RearingTreatment), 
                                           data=FamilyMeans[FamilyMeans$Family==i,], col=i)}


