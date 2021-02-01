#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
var <- args[1]
id <- args[2]
source( "read_data.r")
source( "wave_dec.r")
source( "wave_rep_check.r")
source( "spread_check.r")
source( "/home/cristianl/projects/bliss/src/gamma_get_shape_rate_from_dataset.r")
#
#
if ( var=="PP") {
  res <- read_data(file=paste0("raw/",var,"_",id,"_H_4.txt"))
  values <- res$values
  time_stamps <- res$date_times
  vmin <- 0
  vmax <- 100
  pads <- 0
  len_spread_check <- 8
} else if (var=="T") {
  res <- read_data(file=paste0("raw/",var,"_",id,"_H_1.txt"))
  values <- res$values
  time_stamps <- res$date_times
  vmin <- -40
  vmax <- 50
  pads <- NA
  len_spread_check <- 5
}
prefix <- paste0( var, "_", id)
#
# wavelets
res <- wave_dec( values, 
                 time_stamps, 
                 flag = rep( 0, length( values)),
                 date_format="%Y-%m-%d %H:00:00",
                 vmin = vmin, 
                 vmax = vmax, 
                 pads = pads)
res_dwt <- dwt( res$val, "haar", n.levels = res$n_lev)
for (i in 1:res$n_lev) {
  res_dwt <- dwt( res$val, "haar", n.levels = i)
  res_str <- 2**i
  if (res_str < 24) {
    res_str<-paste0(round(res_str,0),"h")
  } else if ( (res_str/24)<365 ) {
    res_str<-paste0(round(res_str/24,1),"d")
  } else {
    res_str<-paste0(round(res_str/(365*24),2),"y")
  }
  png(file=paste0("pngs/",prefix,"_wavedec_",formatC(i,flag="0",width=3),".png"), height=800, width=800)
  par(mar=c(5,5,5,1))
  plot(res_dwt[[i+1]],main=paste("wavelet decomposition, depth=",i,"resolution=",res_str),
       type="l",lwd=2,col="blue", xlab="index", ylab="wavelet-filtered signal")
  dev.off()
}
#
res <- wave_rep_check( values, 
                       time_stamps, 
                       flag = rep( 0, length( values)),
                       date_format="%Y-%m-%d %H:00:00",
                       len_spread_check = len_spread_check,
                       vmin = vmin, 
                       vmax = vmax, 
                       pads = pads)
#
values[values<vmin] <- vmin
values[values>vmax] <- vmax
png( file=paste0("pngs/",prefix,"_final.png"), height=800, width=800)
plot( time_stamps, values, pch=21, bg="blue",col="blue" )
points( time_stamps[which(res==1)], values[which(res==1)], pch=21, bg="red",col="red" ) 
dev.off()
#
q()
