library(ggplot2)
require(data.table) 
#install.packages("viridis")  # Install
library("viridis")
library(tidyr)
library(dplyr)

setwd("/Users/nikolai/Desktop/model/")

# --- data input
#data<-read.table("Output.csv", header=F, sep=",")

#type <- "white_tra"
#type <- "ffer_tra"
#type <- "DOM_tra"
#type <- "tra"
type <- "ffer_Xshred"


input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")

g <- subset(data, V7 == "all")
g <- subset(g, V1 <= 6)
g <- subset(g, V3 != "Sex")
g <- subset(g, V3 != "Sex_Karyotype")
g$V7 <- NULL
colnames(g) <- c("Iterations","Generation","Trait","Allele1","Allele2","Number")
g$R1[g$Allele1 == "R1"] <- "R1 carrier"
g$R1[g$Allele1 != "R1"] <- "Other"
g$R1[g$Allele2 == "R1"] <- "R1 carrier"
# g$R2[g$Allele1 == "R2"] <- "R2 carrier"
# g$R2[g$Allele1 != "R2"] <- "Other"
# g$R2[g$Allele2 == "R2"] <- "R2 carrier"
g$Allele1 <- NULL
g$Allele2 <- NULL
g <- droplevels(g)
g <- complete(g,Iterations,Generation, Trait, R1)
g[is.na(g)] <- 0
g <- aggregate(Number ~ ., data=g, FUN=sum)

pF <- ggplot(data=g, aes(x=Generation,y=Number,color=R1)) +
  geom_line() + 
  geom_point(aes(shape=R1)) +
  scale_shape_manual(values=1:10) +
  facet_grid(Trait ~ Iterations) +
  scale_color_viridis(discrete = TRUE,option = "D") +
  theme_minimal() + theme(panel.spacing = unit(0.75, "lines"), panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 20)) +
  ylab("Number of Individuals")
print(pF)


pFp <- pF +
  theme(title=element_text(size=14, hjust=0.5),
        axis.title=element_text(size=14),
        axis.text = element_text(size=10),strip.text.x = element_text(size = 10), legend.text=element_text(size=10))
ggsave(filename=paste(type,"_genotypes.png",sep=""), plot=pFp, device="png", path=getwd(),dpi=500, height=12, units="cm")

gc()
