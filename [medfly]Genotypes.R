library(ggplot2)
require(data.table) 
#install.packages("viridis")  # Install
library("viridis")
library(tidyr)
library(dplyr)

setwd(paste(file.path(path.expand('~'),'Desktop'),"/model", sep = ""))

setwd("/Users/nikolai/Desktop/model/")
data<-read.table("modeloutput.csv", header=F, sep=",")

g <- subset(data, V7 == "all")
g <- subset(g, V1 <= 3)
g <- subset(g, V3 != "Sex")
g <- subset(g, V3 != "Sex_Karyotype")
g <- subset(g, V3 != "Eggs")

g$V7 <- NULL
colnames(g) <- c("Iterations","Generation","Trait","Allele1","Allele2","Number")
g$Genotypes <- paste(g$Allele1,g$Allele2)
g$Allele1 <- NULL
g$Allele2 <- NULL
g <- droplevels(g)
g <- complete(g,Iterations,Generation, Trait,Genotypes)
g[is.na(g)] <- 0
g <- setDT(g)[, Fraction := Number / sum(Number), by=list(Generation, Iterations, Trait)]


g <- subset(g, Trait == "TRA")

pF <- ggplot(data=g, aes(x=Generation,y=Fraction,color=Genotypes)) +
  geom_line() + 
  geom_point(aes(shape=Genotypes)) +
  scale_shape_manual(values=1:10) +
  facet_grid(Trait ~ Iterations) +
  scale_color_viridis(discrete = TRUE,option = "D") +
  theme_minimal() + theme(panel.spacing = unit(0.75, "lines"), panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 20)) +
  ylab("Fraction of Individuals")

print(pF)

pFp <- pF +
  theme(title=element_text(size=14, hjust=0.5),
        axis.title=element_text(size=14),
        axis.text = element_text(size=10),strip.text.x = element_text(size = 10), legend.text=element_text(size=10))

ggsave(filename=paste(type,"_3pop_fractions.png",sep=""), plot=pFp, device="png", path=getwd(),dpi=500, height=12, units="cm")

pA <- ggplot(data=g, aes(x=Generation,y=Fraction,color=Genotypes)) +
  stat_summary(aes(group=Genotypes), fun = "mean" ,geom = 'line', size=1, alpha=1) +
  facet_grid(cols = vars(g$Trait)) + scale_color_viridis(discrete = TRUE,option = "D")+
  theme_minimal() + theme(panel.spacing = unit(0.75, "lines"), panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0,20,1), expand = c(0, 0)) +
  ylab("Number of Individuals (mean)")

print(pA)

pAo <- pA +
  theme(title=element_text(size=18, hjust=0.5),
  axis.title=element_text(size=16),
  axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10))

ggsave(filename=paste(type,"_genotypes.png",sep=""), plot=pAo, device="png", path=getwd(),dpi=500, height=12, units="cm")

gc()