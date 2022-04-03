library(ggplot2)
require(data.table)
library(tidyr)
library(dplyr)

setwd(paste(file.path(path.expand('~'),'Desktop'),"/model", sep = ""))

data<-read.table("modeloutput.csv", header=F, sep=",")
samplesize = 47

gn <- subset(data, V3 == "Sex")
gn_keeps <- c("V1", "V2","V6")
gntot <- gn[gn_keeps]
colnames(gntot) <- c("Iterations","Generation","Number")
gntott <- data.table(gntot)
gntott <- gntott[ , .(Population = sum(Number)), by = .(Generation,Iterations)]

pn <- ggplot(data=gntott, aes(x=Generation, y=Population, group=Iterations)) +
  geom_line(aes(color=Iterations)) +
  geom_point() + ylim(0,1000) + scale_x_continuous(breaks=seq(0,10,1))
print(pn)

g <- subset(data, V3 != "Sex")
g <- subset(g, V7 == "sample")
g$V7 <- NULL
colnames(g) <- c("Iterations","Generation","Trait","Allele1","Allele2","Number")

gz <- subset(g, Trait == "ZPG")
gz$DriveOrNot <- ifelse(gz$Allele2 == "Transgene", "ZPG", "unmarked")
gz_keeps <- c("Iterations", "Generation","DriveOrNot" ,"Number")
gz <- gz[gz_keeps]
gzt <- data.table(gz) 
gzT <- gzt[ , .(Totalcount = sum(Number)), by = .(DriveOrNot, Generation,Iterations)]

# p0 <- ggplot(data=gzT, aes(x=Generation, y=Totalcount, group=DriveOrNot)) +
#   geom_line(aes(color=DriveOrNot)) +
#   geom_point() +
#   facet_grid(cols = vars(gzT$Iterations)) +
#   ggtitle("ZPG drive")
# 
# print(p0)

g1 <- subset(g, Trait == "Aper1")
g1$DriveOrNot <- ifelse(g1$Allele2 == "Transgene", "Aper1", "unmarked")
g1_keeps <- c("Iterations", "Generation","DriveOrNot" ,"Number")
g1 <- g1[g1_keeps]
g1t <- data.table(g1) 
g1T <- g1t[ , .(Totalcount = sum(Number)), by = .(DriveOrNot, Generation,Iterations)]

# p1 <- ggplot(data=g1T, aes(x=Generation, y=Totalcount, group=DriveOrNot)) +
#   geom_line(aes(color=DriveOrNot)) +
#   geom_point() +
#   facet_grid(cols = vars(g1T$Iterations)) +
#   ggtitle("Aper1 drive")
# 
# print(p1)

g2 <- subset(g, Trait == "AP2")
g2$DriveOrNot <- ifelse(g2$Allele2 == "Transgene", "AP2", "unmarked")
g2_keeps <- c("Iterations", "Generation","DriveOrNot" ,"Number")
g2 <- g2[g2_keeps]
g2t <- data.table(g2) 
g2T <- g2t[ , .(Totalcount = sum(Number)), by = .(DriveOrNot, Generation,Iterations)]

# p2 <- ggplot(data=g2T, aes(x=Generation, y=Totalcount, group=DriveOrNot)) +
#   geom_line(aes(color=DriveOrNot)) +
#   geom_point() +
#   facet_grid(cols = vars(g2T$Iterations)) +
#   ggtitle("AP2 drive")
# 
# print(p2)


g3 <- subset(g, Trait == "CP")
g3$DriveOrNot <- ifelse(g3$Allele2 == "Transgene", "CP", "unmarked")
g3_keeps <- c("Iterations", "Generation","DriveOrNot" ,"Number")
g3 <- g3[g3_keeps]
g3t <- data.table(g3) 
g3T <- g3t[ , .(Totalcount = sum(Number)), by = .(DriveOrNot, Generation,Iterations)]

# p3 <- ggplot(data=g3T, aes(x=Generation, y=Totalcount, group=DriveOrNot)) +
#   geom_line(aes(color=DriveOrNot)) +
#   geom_point() +
#   facet_grid(cols = vars(g3T$Iterations)) +
#   ggtitle("CP drive")
# 
# print(p3)

gzT <- subset(gzT, DriveOrNot != "unmarked")
g1T <- subset(g1T, DriveOrNot != "unmarked")
g2T <- subset(g2T, DriveOrNot != "unmarked")
g3T <- subset(g3T, DriveOrNot != "unmarked")

notall <- rbind(gzT,g1T,g2T,g3T)
all <- merge(gntott, notall, by=c("Iterations","Generation"), all=TRUE)

# nall <- all[rowSums(is.na(all)) > 0,]
# nall <- nall[rep(1:nrow(nall),each=4),]
# genes <- rep(all$DriveOrNot[0:4],nrow(nall)/4)
# nall$DriveOrNot <- genes
# nall$Totalcount <- 0
# all <- na.omit(all)
# all <- rbind(all, nall)

all <- na.omit(all)
rownames(all) <- NULL
all <- complete(all,Iterations,Generation, DriveOrNot)
all[is.na(all)] <- 0

all$Sample <- ifelse(all$Population > samplesize,samplesize,all$Population)
all$Freq <- all$Totalcount/all$Sample
all$Generation <- all$Generation - 1
all$Freq[is.nan(all$Freq)] <- 0



pallz <- ggplot(data=all, aes(x=Generation,y=Freq)) +
  geom_line(aes(group=Iterations, color=DriveOrNot), alpha = 0.5) +
  facet_grid(cols = vars(all$DriveOrNot)) +
  stat_summary(aes(group=DriveOrNot), fun = mean, geom = 'line', size=1, alpha=1) + theme_bw() + 
  scale_x_continuous(breaks=seq(0,10,1), expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values=c("#B59CA3", "#F2CB4A", "#ACB462", "#D4E1ED")) + theme(panel.spacing = unit(0.75, "lines")) +
  labs(y="Transgenic frequency")

pallz_f <- pallz +
  theme(title=element_text(size=18, hjust=0.5),
        axis.title=element_text(size=16),
        axis.text = element_text(size=12),legend.text=element_text(size=10),strip.text.x = element_text(size = 12))

print(pallz)


ggsave(filename="T.png", plot=pallz_f, device="png", path=getwd(),
       dpi=500, height=12, units="cm")



#all <- subset(all, Iterations < 5)
# pall <- ggplot(data=all, aes(x=Generation, y=Freq, group=DriveOrNot)) +
#   geom_line(aes(color=DriveOrNot)) +
#   facet_grid(cols = vars(all$Iterations)) +
#   ggtitle("Dave Simulation") +
#   ylim(0,0.8)
# 
# print(pall)

#write.csv(all,file = path.expand('~/Desktop/Output_all.csv'), row.names = FALSE)

gc()
