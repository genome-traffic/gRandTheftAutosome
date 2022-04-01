library(ggplot2)
require(data.table)
library(tidyr)
library(dplyr)
library("viridis")

setwd("/Users/nikolai/Desktop/model/")

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

type <- "white_tra"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d1 <- process_data(data)
d1$Type <- "white_tra"

type <- "ffer_tra"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d2 <- process_data(data)
d2$Type <- "ffer_tra"

type <- "ffer"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d3 <- process_data(data)
d3$Type <- "ffer"

type <- "tra"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d4 <- process_data(data)
d4$Type <- "tra"

type <- "DOM_tra"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d5 <- process_data(data)
d5$Type <- "tra(dom)"

type <- "ffer_Xshred"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d6 <- process_data(data)
d6$Type <- "ffer_Xshred"

type <- "SIT"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d7 <- process_data(data)
d7$Type <- "SIT"

type <- "dsx"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d8 <- process_data(data)
d8$Type <- "dsx"

type <- "ffer_moy"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d9 <- process_data(data)
d9$Type <- "ffer_moy"

type <- "ffer_tra_50"
input <- paste("Output_", type ,".csv", sep="")
data<-read.table(input, header=F, sep=",")
d10 <- process_data(data)
d10$Type <- "ffer_tra_50"

a <- d10
#a <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)

pEX <- ggplot(data=a, aes(x=Generation,y=Population, group=Type)) +
  geom_line(aes(color=Type), size=3) +
  xlim(4,20) +
  ylim(0,100) +
  scale_color_viridis(discrete = TRUE,option = "D") +
  theme(panel.border=element_blank()) +
  ylab("Surviving Populations (%)")  +
  theme(title=element_text(size=10, hjust=0.5),
  axis.title=element_text(size=16),
  axis.text = element_text(size=16),strip.text.x = element_text(size = 16), legend.text=element_text(size=16))

print(pEX)



 

gc()
