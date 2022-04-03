library(ggplot2)
require(data.table)
library(tidyr)
library(dplyr)

setwd(paste(file.path(path.expand('~'),'Desktop'),"/model", sep = ""))
data<-read.table("modeloutput.csv", header=F, sep=",")

#---------------------- BY SEX -------------------------------

gn <- subset(data, V3 == "Sex")
gnx_keeps <- c("V1", "V2","V4","V6")
gnxtot <- gn[gnx_keeps]
colnames(gnxtot) <- c("Iterations","Generation","Sex","Number")
gnxtott <- data.table(gnxtot)

# pnx <- ggplot(data=gnxtott, aes(x=Generation, y=Number, group=Iterations)) +
#   geom_line() +
#   ylim(0,500) + scale_x_continuous(breaks=seq(0,20,1)) +
#   facet_wrap(~ Sex)
# print(pnx)
# ggsave(filename=paste(type,"_sexes.png",sep=""), plot=pnx, device="png", path=getwd(),dpi=500, height=12, units="cm")

pnxm <- ggplot(data=gnxtott, aes(x=Generation, y=Number)) +
  geom_point(size=0.01) +
  geom_smooth() +
  ylim(1,500) + scale_x_continuous(breaks=seq(0,20,1)) +
  facet_wrap(~ Sex)
print(pnxm)

#---------------------- BY KARYOTYPE -------------------------------

k <- subset(data, V3 == "Sex_Karyotype")
k_keeps <- c("V1", "V2","V4","V6")
ktot <- k[k_keeps]
colnames(ktot) <- c("Iterations","Generation","Karyotype","Number")
ktott <- data.table(ktot)

# kp <- ggplot(data=ktott, aes(x=Generation, y=Number, group=Iterations)) +
#   geom_line() +
#   ylim(0,500) + scale_x_continuous(breaks=seq(0,20,1)) +
#   facet_wrap(~ Karyotype)
# print(kp)
# ggsave(filename=paste(type,"_sexes.png",sep=""), plot=pnx, device="png", path=getwd(),dpi=500, height=12, units="cm")

kpxm <- ggplot(data=ktott, aes(x=Generation, y=Number)) +
  geom_point(size=0.01) +
  geom_smooth() +
  ylim(1,500) + scale_x_continuous(breaks=seq(0,20,1)) +
  facet_wrap(~ Karyotype)
print(kpxm)

#---------------------- BY STERILE MALES  -------------------------------

sit <- subset(data, V3 == "SterileMales")
sit_keeps <- c("V1", "V2","V4","V6")
sittot <- sit[sit_keeps]
colnames(sittot) <- c("Iterations","Generation","SterileMales","Number")
sittott <- data.table(sittot)


sitpxm <- ggplot(data=sittott, aes(x=Generation, y=Number)) +
  geom_point(size=0.01) +
  geom_smooth() +
  scale_x_continuous(breaks=seq(0,20,1)) 
print(sitpxm)

#---------------------- BY EGGS -------------------------------

e <- subset(data, V3 == "Eggs")
e_keeps <- c("V1", "V2","V4","V6")
etot <- e[e_keeps]
colnames(etot) <- c("Iterations","Generation","Eggs","Number")
etott <- data.table(etot)


epxm <- ggplot(data=etott, aes(x=Generation, y=Number)) +
  geom_point(size=0.01) +
  geom_smooth() +
  scale_x_continuous(breaks=seq(0,20,1)) 
print(epxm)

#---------------------- ALL -------------------------------

gn_keeps <- c("V1", "V2","V6")
gntot <- gn[gn_keeps]
colnames(gntot) <- c("Iterations","Generation","Number")
gntott <- data.table(gntot)
gntott <- gntott[ , .(Population = sum(Number)), by = .(Generation,Iterations)]

pn <- ggplot(data=gntott, aes(x=Generation, y=Population, group=Iterations)) +
  geom_line() +
  geom_point() + ylim(0,500) + xlim(5,20)
print(pn)

ggsave(filename=paste(type,"_population.png",sep=""), plot=pn, device="png", path=getwd(),dpi=500, height=12, units="cm")


gc()
