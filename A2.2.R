library(dplyr)
library(ggplot2)
rates = data.matrix(read.table("~/Downloads/InterestRates.txt", header=FALSE))
rates
rates1 <- read.table("~/Downloads/InterestRates.txt")
rates1
r <- t(head(rates))
r
datas <-subset(r,r[,1:5])
datas
datas.frame = data.frame(datas)
datas.frame
colnames(datas.frame) <- c("D1", "D2", "D3", "D4", "D5")
length(datas.frame$D1)
rframe = datas.frame$D1

YC = ggplot(data= datas.frame ) + aes( x = c(1:51)) + 
  geom_line(aes(y = D1), color = "darkred") + 
  geom_line(aes(y = D2), color="blue") + 
  geom_line(aes(y = D3), color="green") +
  geom_line(aes(y = D4), color="yellow")+
  geom_line(aes(y = D5), color="purple") +
  xlab("Maturity")
YC

datas1$V22
datas1 <- as.data.frame(rates)
datas1$V1
DC = ggplot(data= datas1 ) + aes( x = c(1:1264)) + 
  geom_line(aes(y = V1), color = "darkred") + xlab("Yield overnight") + geom_line(aes(y = V5), color = "darkgreen")
DC

"Yield overnight over time is the opposite values to the other randomly chosen yield over time. 
In the day where overnight yield significantly decreases, 
the randomly chosen maturity remarkably increases 
and it can be noticeable thru the whole graph."


diffr <- sapply(1:length(rates1)-1, function(i) rates1[,i+1] - rates1[,i])
diffr1 <- as.data.frame(diffr[2:50])
colnames(diffr1) <- paste0("V",2:50)
diffr1$V3
cor(diffr1)


CC = ggplot(data= diffr ) + aes( x = c(1:1264)) + 
  geom_line(aes(y = diffr[3]), color = "darkred") + xlab("One year yield") 
CC

CC1 = ggplot(data= diffr ) + aes( x = c(1:1264)) + 
CC1

CC2 = ggplot(data= diffr ) + aes( x = c(1:1264)) + 
  geom_line(aes(y = diffr[22]), color = "darkblue") + xlab("Ten year yield") 
CC2



CCC = ggplot(data= diffr) + aes( x = c(1:1264)) + geom_line(aes(y = V11), color = "darkgreen") + geom_line(aes(y = V22), color = "darkblue") +  geom_line(aes(y = V3), color = "darkred") + xlab("Days measure")
CCC
pairs(~ + FIVEY + TENY + RNDOM, data=SBS[1:1264,], pch=20, col="blue")

