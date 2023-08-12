library(ggplot2)
require(data.table)
library(tidyr)
library(dplyr)
library("viridis")

setwd(paste(file.path(path.expand('~'),'Desktop'),"/model", sep = ""))
a<-read.table("modeloutput.csv", header=F, sep=",")


process_data <- function(dataset) {
  gn <- subset(dataset, V3 == "Sex")
  gn_keeps <- c("V1", "V2","V6")
  gntot <- gn[gn_keeps]
  colnames(gntot) <- c("Iterations","Generation","Number")
  gntott <- data.table(gntot)
  gntott <- gntott[ , .(Population = sum(Number)), by = .(Generation,Iterations)]
  gntott[Population > 1]$Population <- 1
  gntott$Iterations <- NULL
  gntotta<- aggregate(Population ~ Generation, gntott, sum)
  return(gntotta)
}

proca <- process_data(a)

pEX2 <- ggplot(data=proca, aes(x=Generation,y=Population)) +
  geom_line(size=3) +
  xlim(1,20) +
  ylim(0,100) +
  scale_color_viridis(discrete = TRUE,option = "D") +
  theme(panel.border=element_blank()) +
  ylab("Surviving Populations (%)")  +
  theme(title=element_text(size=10, hjust=0.5),
        axis.title=element_text(size=16),
        axis.text = element_text(size=16),strip.text.x = element_text(size = 16), legend.text=element_text(size=16))
print(pEX2)


# pEX <- ggplot(data=a, aes(x=Generation,y=Population, group=Type)) +
#   geom_line(aes(color=Type), size=3) +
#   xlim(4,20) +
#   ylim(0,100) +
#   scale_color_viridis(discrete = TRUE,option = "D") +
#   theme(panel.border=element_blank()) +
#   ylab("Surviving Populations (%)")  +
#   theme(title=element_text(size=10, hjust=0.5),
#   axis.title=element_text(size=16),
#   axis.text = element_text(size=16),strip.text.x = element_text(size = 16), legend.text=element_text(size=16))
# 
# print(pEX)
 

gc()
