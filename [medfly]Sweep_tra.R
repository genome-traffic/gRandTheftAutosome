library(ggplot2)
require(data.table)
library(tidyr)
library(dplyr)
library("viridis")

setwd(paste(file.path(path.expand('~'),'Desktop'),"/model", sep = ""))
s <- read.table("modelsweepoutput.csv", header=F, sep=",")
#s <- read.table("sweep/ffer_ffer/modelsweepoutput.csv", header=F, sep=",")

colnames(s) <- c("iterations","hdr","activity","r2","survival")
s$survival <- s$survival/1000
s$extinction = 1 - s$survival
s$survival <- NULL
se <- aggregate(cbind(extinction) ~ hdr + activity + r2, s, mean)

hdr_list <- unique(se$hdr)

for (h in hdr_list) 
    {
      print(h)
      
      se1 <- subset(se, hdr == h) 
      
      model <- loess(extinction ~ activity + r2, data = se1)
      
      see1 <- with(se1, expand.grid(activity = seq(min(se1$activity), max(se1$activity),len = 500), r2 = seq(min(se1$r2), max(se1$r2), len = 500)))
      see1$extinction <- c(predict(model, newdata = see1, se1 = FALSE))
      see1$extinction[which(see1$extinction > 1)] <- 1
      see1$extinction[which(see1$extinction < 0)] <- 0
      
      pCoarse <- ggplot(se1, aes(activity, r2, z = extinction)) + geom_contour_filled() + ggtitle(h)
      print(pCoarse)
      
      ggsave(filename=paste(h, "coarse.png",sep=""), plot=pCoarse, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      ggsave(filename=paste(h, "coarse.pdf",sep=""), plot=pCoarse, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      
      pFine <- ggplot(see1, aes(activity, r2, z = extinction)) + geom_contour_filled(bins = 10) + ggtitle(h)
      print(pFine)
      
      ggsave(filename=paste(h, "fine1.png",sep=""), plot=pFine, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      ggsave(filename=paste(h, "fine1.pdf",sep=""), plot=pFine, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      
      pFine2 <- ggplot(see1, aes(activity, r2, z = extinction)) + geom_raster(aes(fill = extinction)) +
        geom_contour(color= 'black', bins = 10, alpha = 0.25) +
        scale_fill_viridis_c(limits=c(0, 1)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(title=element_text(size=12, hjust=0.5),
              axis.title=element_text(size=12),
              axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10)) +
        theme(legend.position="none")
      print(pFine2)
      
      ggsave(filename=paste(h, "fine2.png",sep=""), plot=pFine2, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      ggsave(filename=paste(h, "fine2.pdf",sep=""), plot=pFine2, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      
      pFine2L <- ggplot(see1, aes(activity, r2, z = extinction)) + geom_raster(aes(fill = extinction)) +
        geom_contour(color= 'black', bins = 10, alpha = 0.25) +
        scale_fill_viridis_c(limits=c(0, 1)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(title=element_text(size=12, hjust=0.5),
              axis.title=element_text(size=12),
              axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10)) 
      print(pFine2L)
      
      ggsave(filename=paste(h, "Lfine2.png",sep=""), plot=pFine2L, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      ggsave(filename=paste(h, "Lfine2.pdf",sep=""), plot=pFine2L, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      

      pFine3 <- ggplot(see1, aes(activity, r2, z = extinction)) + geom_raster(aes(fill = extinction)) +
        scale_fill_viridis_c(limits=c(0, 1)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(title=element_text(size=12, hjust=0.5),
              axis.title=element_text(size=12),
              axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10)) +
        theme(legend.position="none")
      print(pFine3)
      
      ggsave(filename=paste(h, "fine3.png",sep=""), plot=pFine3, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      ggsave(filename=paste(h, "fine3.pdf",sep=""), plot=pFine3, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      
      
      gc()

  }

# library(ggplot2)
# require(data.table)
# library(tidyr)
# library(dplyr)
# library("viridis")
# 
# setwd(paste(file.path(path.expand('~'),'Desktop'),"/model", sep = ""))
# s <- read.table("modelsweepoutput.csv", header=F, sep=",")
# colnames(s) <- c("iterations","activity","r2","survival")
# s[s$survival > 1,]$survival <- 1
# s$extinction = 1 - s$survival
# s$survival <- NULL
# 
# se <- aggregate(cbind(extinction) ~ activity + r2, s, mean)
# model <- loess(extinction ~ activity + r2, data = se)
# see <- with(se, expand.grid(activity = seq(min(se$activity), max(se$activity),len = 100), r2 = seq(min(se$r2), max(se$r2), len = 100)))
# see$extinction <- c(predict(model, newdata = see, se = FALSE))
# 
# pCoarse <- ggplot(se, aes(activity, r2, z = extinction)) + geom_contour_filled()
# print(pCoarse)
# 
# pFine <- ggplot(see, aes(activity, r2, z = extinction)) + geom_contour_filled()
# print(pFine)
# 
# pFine2 <- ggplot(see, aes(activity, r2, z = extinction)) + geom_raster(aes(fill = extinction)) +
#   geom_contour(color= 'black', bins = 15, alpha = 0.25) +
#   scale_fill_viridis_c() +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme(title=element_text(size=12, hjust=0.5),
#         axis.title=element_text(size=12),
#         axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10)) +
#   theme(legend.position="none")
# print(pFine2)
# 
# ggsave(filename="unnamed.png", plot=pFine2, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
# ggsave(filename="unnamed.pdf", plot=pFine2, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")
# 
# gc()
