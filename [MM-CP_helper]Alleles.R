library(ggplot2)
require(data.table) 
#install.packages("viridis")  # Install
library("viridis")
library(tidyr)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# --- data input

data<-read.table("MMCP.csv", header=F, sep=",")
#data<-read.table("MMCP_mat.csv", header=F, sep=",")

g <- subset(data, V7 == "all")
#g <- subset(g, V1 <= 3)
g <- subset(g, V3 != "Sex")
g <- subset(g, V3 != "Sex_Karyotype")
g <- subset(g, V3 == "CP")
#g <- subset(g, V3 == "Cas9_helper")

g$V7 <- NULL
colnames(g) <- c("Iterations","Generation","Trait","Allele1","Allele2","Number")
g$Chroms <- 2 * g$Number

# ---------------- Drive -------------------
gD <- g
#gD$Generation <- gD$Generation - 1
gD$Allele1 <- ifelse(gD$Allele1 == "Transgene", 1, 0)
gD$Allele2 <- ifelse(gD$Allele2 == "Transgene", 1, 0)

numbXG <- gD
numbXG$Transgenic <- ifelse((numbXG$Allele1 + numbXG$Allele2) > 0,"Non-transgenic","Transgenic")

gD$DriveChroms <- (gD$Allele1 + gD$Allele2) * gD$Number
gD <- setDT(gD)[, Fraction := DriveChroms / sum(Chroms), by=list(Generation, Iterations, Trait)]
gDf <- aggregate(Fraction ~ Iterations + Generation + Trait, data=gD, FUN=sum)

p3 <- ggplot(numbXG, aes(x=Generation, y=Number/50,color=Transgenic)) +
  stat_summary(aes(group=Transgenic), fun = "sum" ,geom = 'line', size=1, alpha=1) +
  scale_x_continuous(breaks=0:10) +
  theme_bw()
plot(p3)

p4 <- ggplot(gDf, aes(x=Generation, y=Fraction)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  scale_x_continuous(breaks=0:10) + 
  theme_bw()
plot(p4)

p5 <- ggplot(gDf, aes(x=Generation, y=Fraction)) +
  geom_smooth(method = "loess") +
  ylim(0,1) +
  scale_x_continuous(breaks=0:10) + 
  theme_bw()
plot(p5)

p6 <- ggplot(gDf, aes(x=Generation, y=Fraction)) +
  geom_jitter() +
  ylim(0,1) +
  geom_hline(yintercept=0.3) +
  scale_x_continuous(breaks=0:10) +
  theme_bw()
plot(p6)


