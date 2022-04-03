library(ggplot2)
require(data.table) 
#install.packages("viridis")  # Install
library("viridis")
library(tidyr)
library(dplyr)

setwd(paste(file.path(path.expand('~'),'Desktop'),"/model", sep = ""))

# --- data input
data<-read.table("modeloutput.csv", header=F, sep=",")

g <- subset(data, V7 == "all")
g <- subset(g, V3 != "Sex")
g <- subset(g, V3 != "Eggs")
g$V7 <- NULL
colnames(g) <- c("Iterations","Generation","Trait","Allele1","Allele2","Number")
g$Generation <- g$Generation - 1
g$Genotypes <- paste(g$Allele1,g$Allele2)
g$Allele1 <- NULL
g$Allele2 <- NULL
g <- droplevels(g)
g <- complete(g,Iterations,Generation, Trait,Genotypes)
g[is.na(g)] <- 0



pA <- ggplot(data=g, aes(x=Generation,y=Number,color=Genotypes)) +
  stat_summary(aes(group=Genotypes), fun = mean, geom = 'line', size=1, alpha=1) +
  facet_grid(cols = vars(g$Trait)) + scale_color_viridis(discrete = TRUE,option = "D")+
  theme_minimal() + theme(panel.spacing = unit(0.75, "lines"), panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0,10,1), expand = c(0, 0)) + ylab("Number of Individuals")

print(pA)

pAo <- pA +
   theme(title=element_text(size=18, hjust=0.5),
         axis.title=element_text(size=16),
         axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10))
 
ggsave(filename="G.png", plot=pAo, device="png", path=getwd(),dpi=500, height=12, units="cm")


# 
# gz <- subset(g, Trait == "ZPG")
# 
# pZ <- ggplot(data=gz, aes(x=Generation,y=Number,color=Genotypes)) +
#   stat_summary(aes(group=Genotypes), fun = mean, geom = 'line', size=1, alpha=1) + scale_color_viridis(discrete = TRUE,option = "D")+
#   theme_minimal() 
# 
# print(pZ)


gc()


