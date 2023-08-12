library(ggplot2)
require(data.table)
library(tidyr)
library(dplyr)
library("viridis")

setwd(paste(file.path(path.expand('~'),'Desktop'),"/model", sep = ""))
s <- read.table("modeltimesweepoutput.csv", header=F, sep=",")
#s <- read.table("timesweep/tra_ffer/modeltimesweepoutput.csv", header=F, sep=",")

h="time"
colnames(s) <- c("iterations","hdr","activity","r2","time_to_ext")
s[is.na(s$time_to_ext),]$time_to_ext <- 100

st <- aggregate(cbind(time_to_ext) ~ hdr + activity, s, mean)
model <- loess(time_to_ext ~ activity + hdr, data = st)
st1 <- with(st, expand.grid(activity = seq(min(st$activity), max(st$activity),len = 200), hdr = seq(min(st$hdr), max(st$hdr), len = 200)))
st1$time_to_ext <- c(predict(model, newdata = st1, st = FALSE))

st1$time_to_ext[which(st1$time_to_ext > 100)] <- 100
st1$time_to_ext[which(st1$time_to_ext < 1)] <- 1

pCoarse <- ggplot(st1, aes(activity,hdr, z = time_to_ext)) + geom_contour_filled() 
print(pCoarse)
ggsave(filename=paste(h, "tte_coarse.png",sep=""), plot=pCoarse, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
ggsave(filename=paste(h, "tte_coarse.pdf",sep=""), plot=pCoarse, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      
      
pFine <- ggplot(st1, aes(activity, hdr, z = time_to_ext)) + geom_raster(aes(fill = time_to_ext)) +
        geom_contour(color= 'black', bins = 10, alpha = 0.25) +
        scale_fill_viridis_c(option="magma", direction = -1, limits=c(0, 100)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(title=element_text(size=12, hjust=0.5),
              axis.title=element_text(size=12),
              axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10)) 
      
print(pFine)
ggsave(filename=paste(h, "tte_fine1.png",sep=""), plot=pFine, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
ggsave(filename=paste(h, "tte_fine1.pdf",sep=""), plot=pFine, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      
pFine2 <- ggplot(st1, aes(activity, hdr, z = time_to_ext)) + geom_raster(aes(fill = time_to_ext)) +
  geom_contour(color= 'black', bins = 10, alpha = 0.25) +
  scale_fill_viridis_c(option="magma", direction = -1, limits=c(0, 100)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(title=element_text(size=12, hjust=0.5),
        axis.title=element_text(size=12),
        axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10)) + theme(legend.position="none")

print(pFine2)
ggsave(filename=paste(h, "tte_fine2.png",sep=""), plot=pFine2, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
ggsave(filename=paste(h, "tte_fine2.pdf",sep=""), plot=pFine2, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")

pFine3 <- ggplot(st1, aes(activity, hdr, z = time_to_ext)) + geom_raster(aes(fill = time_to_ext)) +
        scale_fill_viridis_c(option="magma", direction = -1) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(title=element_text(size=12, hjust=0.5),
              axis.title=element_text(size=12),
              axis.text = element_text(size=12),strip.text.x = element_text(size = 12), legend.text=element_text(size=10)) +
        theme(legend.position="none")
      print(pFine3)
      
ggsave(filename=paste(h, "tte_fine3.png",sep=""), plot=pFine3, device="png", path=getwd(),dpi=500, height=10, width = 10, units="cm")
ggsave(filename=paste(h, "tte_fine3.pdf",sep=""), plot=pFine3, device="pdf", path=getwd(),dpi=500, height=10, width = 10, units="cm")
      
gc()

