setwd("C:/Users/USER/Dropbox")

#data management
dat <- read.csv("data mentah.csv")
str(dat)
summary(dat)

library(tidyr)
be <- gather(dat[,3:8])
att <- gather(dat[,9:17])
emo <- gather(dat[,18:22])

be[,1] <- as.factor(be[,1])
be[,2] <- as.factor(be[,2])

att[,1] <- as.factor(att[,1])
att[,2] <- as.factor(att[,2])

emo[,1] <- as.factor(emo[,1])
emo[,2] <- as.factor(emo[,2])

summary(be)
summary(att)
summary(emo)

addmargins(table(be$value))
addmargins(prop.table(table(be$value)))*100

addmargins(table(att$value))
addmargins(prop.table(table(att$value)))*100

addmargins(table(emo$value))
addmargins(prop.table(table(emo$value)))*100

#write.csv(be,"be.csv")
#write.csv(att,"att.csv")
#write.csv(emo,"emo.csv")

# data fix
be <- read.csv("be.csv")
str(be)
summary(be)
be$be <- ifelse(be$be=="SA",1,ifelse(be$be=="A",2,ifelse(be$be=="N",3,ifelse(be$be=="D",4,ifelse(be$be=="SD",5,6)))))
b <- prop.table(table(be$sex, be$be))*100
xs <- barplot(b,axisnames = F,
        beside=T, col=c("red","blue"), ylim=c(0,100), ylab="percentage", main="BEHAVIOR")
axis(side=1,at=c(2,5,8,11),lab=c("strongly agree","agree", "neutral","disagree"))
legend("topright", fill=c("red","blue"), legend=c("female","male"))

att <- read.csv("att.csv")
str(att)
summary(att)
att$att <- ifelse(att$att=="SA",1,ifelse(att$att=="A",2,ifelse(att$att=="N",3,ifelse(att$att=="D",4,ifelse(att$att=="SD",5,6)))))
barplot(prop.table(table(att$sex, att$att))*100,beside=T, col=c("red","blue"), ylim=c(0,100))
barplot(prop.table(table(att$sex, att$att))*100,axisnames = F,
        beside=T, col=c("red","blue"), ylim=c(0,100), ylab="percentage", main="ATTITUDE")
axis(side=1,at=c(2,5,8,11,14),lab=c("strongly agree","agree", "neutral","disagree", "strongly disagree"))
legend("topright", fill=c("red","blue"), legend=c("female","male"))

emo <- read.csv("emo.csv")
str(emo)
summary(emo)
emo$emo <- ifelse(emo$emo=="SA",1,ifelse(emo$emo=="A",2,ifelse(emo$emo=="N",3,ifelse(emo$emo=="D",4,ifelse(emo$emo=="SD",5,6)))))
barplot(table(emo$sex, emo$emo),beside=T)
barplot(prop.table(table(emo$sex, emo$emo))*100,axisnames = F,
        beside=T, col=c("red","blue"), ylim=c(0,100), ylab="percentage", main="EMOTION")
axis(side=1,at=c(2,5,8,11,14),lab=c("strongly agree","agree", "neutral","disagree", "strongly disagree"))
legend("topright", fill=c("red","blue"), legend=c("female","male"))

