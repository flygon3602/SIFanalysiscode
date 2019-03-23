setwd("/Users/PiaBhanu/Desktop")
library(gplots)
library(car)
data=read.csv("likert2.csv")
datapre=subset(data,data$SIacondition == "Pre SI-a")
meanpre=mean(datapre$SIascore)
##defining datapre,its log,its mean
datapost=subset(data,data$SIacondition == "Post SI-a")
meanpost=mean(datapost$SIascore)
##defining datapost,its log,its mean
sdpre=sd(datapre$SIascore)
sdpost=sd(datapost$SIascore)
##defining standard deviations
sepre=sdpre/sqrt(length(datapre))
sepost=sdpost/sqrt(length(datapost))
##standard errors
t.test(datapre$SIascore, datapost$SIascore)
##t-test
v=length(datapre)+length(datapost)-2
tcrit=qt(.975, v)
##t-crit
clpre=tcrit*sepre
clpost=tcrit*sepost
clpreu=mean(datapre$SIascore)+clpre
clprel=mean(datapre$SIascore)-clpre
clpostu=mean(datapost$SIascore)+clpost
clpostl=mean(datapost$SIascore)-clpost
plotCI(1:2,c(meanpre,meanpost),uiw=c(clpre,clpost),lty=1,xlim=c(0.0,3.0),xaxt="n",xlab="",ylab="Mean Scores",main="95% CL Mean AV Scores")
##plotting <3
datapre$logpre=log(datapre$SIascore)
datapost$logpost=log(datapost$SIascore)
qqnorm(datapre$SIascore)
shapiro.test(datapre$logpre)
##paired t test
t.test(datapre$SIascore, datapost$SIascore, paired=TRUE)
leveneTest(data$SIascore~data$SIacondition)
##checks if variances are the same (can be t-tested)
stripchart(SIascore~School, data=datapre, vertical=TRUE, method="jitter")
schoolmean=tapply(datapre$SIascore, datapre$School, mean)
anov1=aov(SIascore~School, data=datapre)
summary(anov1)
##pairwise comparison after anova
pairwise=TukeyHSD(anov)
pairwisefinal=pairwise$School
pairwisefinal=as.data.frame(pairwisefinal)
pairwisefinal$'p adj'
#pulling out significant comparisons
pairwisesig=subset(pairwisefinal,pairwisefinal$'p adj'<.05)
##plots data by school
data$Stratio[data$School=="Hartford"]=17.73
data$Stratio[data$School=="Navarette"]=18
data$Stratio[data$School=="Hull"]=18.50
data$Stratio[data$School=="Patterson"]=20
data$Stratio[data$School=="Weinberg"]=19.1
data$Stratio[data$School=="CTA"]=20.52
data$Stratio[data$School=="Bologna"]=15.19
data$Stratio[data$School=="Auxier"]=17.67

data$lunch[data$School=="Hartford"]=662
data$lunch[data$School=="Navarette"]=171
data$lunch[data$School=="Hull"]=188
data$lunch[data$School=="Patterson"]=151
data$lunch[data$School=="Weinberg"]=151
data$lunch[data$School=="CTA"]=104
data$lunch[data$School=="Bologna"]=489
data$lunch[data$School=="Auxier"]=177

data$total[data$School=="Hartford"]=665
data$total[data$School=="Navarette"]=720
data$total[data$School=="Hull"]=740
data$total[data$School=="Patterson"]=1000
data$total[data$School=="Weinberg"]=808
data$total[data$School=="CTA"]=667
data$total[data$School=="Bologna"]=615
data$total[data$School=="Auxier"]=866

data$lunchratio=data$lunch/data$total

anov2=aov(SIascore~SIacondition*lunchratio, data=data)
##flunch=c(662, 151, 104, 489, 188, 177, 171, 141)
##school=c("Hartford", "Navarette", "Hull", "Patterson","Weinberg", "CTA", "Bologna","Auxier")
##data$Flunch[data$School==school]=flunch
