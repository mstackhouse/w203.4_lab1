library(car)
library(ggplot2)

CancerData <- read.csv(file="D:/Wei's Workspace/Berkeley Master/W203/Lecture 2/Live Session/Lab_1_Spring_2018/Lab_1 (1)/Cancer_EDA/cancer.csv", header=TRUE, sep=",")

summary(CancerData$PctPrivateCoverage)
summary(CancerData$PctEmpPrivCoverage)
summary(CancerData$PctPublicCoverage)

scatterplotMatrix( ~ PctPrivateCoverage + PctEmpPrivCoverage + PctPublicCoverage + deathRate, data = CancerData, diagonal = "histogram")

hist(CancerData$PctPrivateCoverage,breaks = seq(20,95,by=5), main = "PctPrivateCoverage Hist")

hist(CancerData$PctEmpPrivCoverage,breaks = seq(10,75,by=5), main = "PctEmpPrivCoverage Hist")

hist(CancerData$PctPublicCoverage,breaks = seq(10,70,by=5), main = "PctPublicCoverage Hist")

qplot(CancerData$PctPublicCoverage, geom="histogram", binwidth = 2, fill=I("white"), col=I("blue"), main = "PctPublicCoverage Hist") 

cor(CancerData$deathRate, CancerData$PctPrivateCoverage)
cor(CancerData$deathRate, CancerData$PctEmpPrivCoverage)
cor(CancerData$deathRate, CancerData$PctPublicCoverage)

plot(jitter(CancerData$deathRate, factor = 2), jitter(CancerData$PctPrivateCoverage, factor = 2))
abline(lm(CancerData$deathRate ~ CancerData$PctPrivateCoverage))

plot(jitter(CancerData$deathRate, factor = 2), jitter(CancerData$PctEmpPrivCoverage, factor = 2))
abline(lm(CancerData$deathRate ~ CancerData$PctEmpPrivCoverage))

plot(jitter(CancerData$deathRate, factor = 2), jitter(CancerData$PctPublicCoverage, factor = 2))
abline(lm(CancerData$deathRate ~ CancerData$PctPublicCoverage))