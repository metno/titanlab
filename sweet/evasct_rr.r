#!/usr/bin/env Rscript
# --~- synsct_rr.r -~--
# Test sct over synthetic data
# See the software repository here: https://github.com/cristianlussana/sweet
#..............................................................................
#Copyright and license
# Copyright (C) 2020 MET Norway. The software is licensed under GPL version 3 
# or (at your option) any later version.
# https://www.gnu.org/licenses/gpl-3.0.en.html
# 
# History:
# 09.11.2020 - Cristian Lussana. Original code.
# -----------------------------------------------------------------------------
#
rm( list = ls())
#
# -----------------------------------------------------------------------------
# Libraries
suppressPackageStartupMessages( library( "argparser"))
suppressPackageStartupMessages( library( "sp"))
suppressPackageStartupMessages( library( "raster"))
suppressPackageStartupMessages( library( "rgdal"))
suppressPackageStartupMessages( library( "ncdf4"))
suppressPackageStartupMessages( library( "dotnc"))
#options(warn = 2, scipen = 999)
options( scipen = 999999999)
#
# -----------------------------------------------------------------------------
# Constants
# CRS strings
proj4.wgs84     <- "+proj=longlat +datum=WGS84"
proj4.ETRS_LAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
proj4.utm33     <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4.lcc       <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
#
#
#==============================================================================
# MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN -
#==============================================================================
t0<-Sys.time()
# path to the sweet.r
sweet_path <- Sys.getenv( "SWEET_PATH")
#------------------------------------------------------------------------------
# Load functions
for (file in list.files(path = file.path( sweet_path, "functions"),
                        pattern = ".r", full.names=T) ) 
  source(file)
rm(file)
#------------------------------------------------------------------------------
# Command line arguments
argv <- synsct_argparser()
#
#------------------------------------------------------------------------------
# Read input nc-file
obsnet <- read_obsNet( file=argv$ffin_obs, crs_in=proj4.wgs84, crs_out=proj4.lcc,
                       exclude_prid = 100, 
                       extent_out=extent( argv$gridded_extent[1], argv$gridded_extent[2], 
                                          argv$gridded_extent[3], argv$gridded_extent[4]))
obsnet_or <- obsnet
print("-- obsNet --")
print(obsnet$n)
#
#------------------------------------------------------------------------------
# Read input nc-file
#if ( !file.exists( argv$ffin_fields)) boom(str=argv$ffin_fields, code=1) 
#nc <- nc_open( argv$ffin_fields, readunlim=FALSE )
#v1 <- nc$var[[1]]
#data <- ncvar_get( nc, v1 ) 
#x <- nc$dim[[1]]$vals
#y <- nc$dim[[2]]$vals
#ens <- nc$dim[[3]]$vals
#proj4 <- ncatt_get( nc, varid=nc$var[[2]], attname="proj4")$value
#nc_close( nc)
## dim(data) x,y,ensemble
#nx <- length( x)
#ny <- length( y)
#nens <- length( ens)
#dx <- abs( x[2] - x[1])
#dy <- abs( y[2] - y[1])
#xmin <- min(x) - dx/2
#xmax <- max(x) + dx/2
#ymin <- min(y) - dy/2
#ymax <- max(y) + dy/2
#r <- raster( extent( xmin, xmax, ymin, ymax), res = c( dx, dy), crs = proj4)
#print("-- created raster --")
#print(r)
#
#------------------------------------------------------------------------------
dir_in<-"/home/cristianl/data/sweet/synsct_rr/res"
dir_out<-"/home/cristianl/data/sweet/synsct_rr/res_png"
i<-0
res<-list()
vth <- vector()
vscore <- vector()
vinnov <- vector()
vscore_in <- vector()
vscore_d <- vector()
vscore_v <- vector()
vscore_a <- vector()
vscore_xl <- vector()
vscore_l <- vector()
vscore_m <- vector()
vscore_s <- vector()
argv$rr_lscale <- formatC( argv$rr_lscale, width=6, flag="0", format="fg")
argv$pGE <- formatC( argv$pGE, width=2, flag="0")
argv$thinobs_perc <- formatC( argv$thinobs_perc, width=2, flag="0")
argv$synsct_rr_nens <- formatC( argv$synsct_rr_nens, width=4, flag="0", format="fg")
if ( argv$boxcox_lambda == 0.3) {
  bstr <- "03"
} else {
  bstr <- "05"
}
if (argv$eva_score == "pofd") {
  score <- "pofd"
  score_lab <- "POFD"
  ylim <- c(0,0.2)
} else if (argv$eva_score == "pod") {
  score <- "pod"
  score_lab <- "POD"
  ylim <- c(0,1)
  if (argv$pGE=="00") q()
} else if (argv$eva_score == "acc") {
  score <- "acc"
  score_lab <- "ACC"
  ylim <- c(0,1)
} else if (argv$eva_score == "ets") {
  score <- "ets"
  score_lab <- "ETS"
  ylim <- c(0,1)
  if (argv$pGE=="00") q()
}
#
if ( !file.exists( file.path( dir_out, "evasct_rr_res.txt"))) 
    cat( file=file.path( dir_out, "evasct_rr_res.txt"), append=T,
         "score;lscale;bstr;th;pGE;thinobs_perc;synsct_rr_nens;values;\n")
for (th in argv$t_score_eva) {
  ffin <- file.path( dir_in, paste0("synsct_rr_res_l",argv$rr_lscale,"_b",bstr,"_th",th,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_rr_nens,".dat"))
  print( ffin)
  if ( !file.exists( ffin)) next
  i <- i+1
  res[[i]] <- read_sctRes( file=ffin)
  vth[i] <- th
  if ( i == 1) {
    ffin00 <- file.path( dir_in, paste0("synsct_rr_res_l",argv$rr_lscale,"_b",bstr,"_th",th,"_pGE00_sel",argv$thinobs_perc,"_n",argv$synsct_rr_nens,".dat"))
    auxres <- read_sctRes( file=ffin00)
  }
  #
  undef<-(-999)
  ix <- which( res[[i]][,2] != undef)
  if ( length(ix) > 0) {
    vscore[i] <- score_fun( x=res[[i]][ix,2], x_ref=res[[i]][ix,4], lab=score, threshold=.9, threshold1=.9, type="above") 
    cat( file=file.path( dir_out, "evasct_rr_res.txt"), append=T,
         paste( score, argv$rr_lscale, bstr, th,
                argv$pGE, argv$thinobs_perc, 
                argv$synsct_rr_nens, round( vscore[i],5), sep=";", "\n"))
  }
}
#
#------------------------------------------------------------------------------
ffout <- file.path( dir_out, paste0("synsct_rr_res_",score,"vsth_l",argv$rr_lscale,"_b",bstr,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_rr_nens,".png"))
png( file=ffout, width=800, height=800)
par(mar=c(4,4,1,1))
plot( as.numeric(vth), vscore, xlab="",ylab="", main="", axes=F, ylim=ylim)
abline(h=seq(0,1,by=0.05),col="gray",lty=3)
abline(h=seq(0,1,by=0.1),col="gray",lty=2)
lines(vth,vscore,col="red",lwd=4)
points( as.numeric(vth), vscore, pch=21, bg="pink",cex=4)
abline(h=c(0,1))
#legend(x="topright",lty=1,col=c("white",col),legend=c("sod",usod),cex=2,lwd=6)
axis(1,cex.axis=3)
axis(2,cex.axis=3)
#mtext(1,text="SCT threshold",line=3, cex=2)
#mtext(2,text=score_lab,line=2, cex=2)
#mtext(3,text=paste("l=",argv$rr_lscale," b=",argv$boxcox_lambda," pGE=",argv$pGE," sel=",argv$thinobs_perc," n=",argv$synsct_rr_nens),line=2, cex=2)
text(y=(ylim[2]-0.05*(ylim[2]-ylim[1])), x=10, paste0("P(GE)=",as.integer(argv$pGE),"%"), cex=4 )
box()
devnull <- dev.off()
cat(paste("  written file",ffout,"\n"))
q()
#
# score as a function of sct-threshold
print(vth)
png( file=ffout, width=800, height=800)
par(mar=c(4,4,4,1))
plot( as.numeric(vth), vscore, xlab="",ylab="", main="", axes=F, ylim=ylim)
  lines(vth[ix],vscore[ix],col=col[s],lwd=3)
#  lines(vth[ix],vscore_d[ix],col=col[s],lwd=3,lty=2)
#  lines(vth[ix],vscore_m[ix],col=col[s],lwd=3,lty=2)
#  lines(vth[ix],vscore_v[ix],col=col[s],lwd=3,lty=2)
  polygon( c(vth[ix],vth[ix[length(ix):1]]), c(vscore_v[ix],vscore_d[ix[length(ix):1]]), col=col[s], density=20)
points( as.numeric(vth), vscore, pch=21, bg="darkgray",cex=2)
abline(h=0)
legend(x="topright",lty=1,col=c("white",col),legend=c("sod",usod),cex=2,lwd=6)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext(1,text="SCT threshold",line=3, cex=2)
mtext(2,text=score_lab,line=2, cex=2)
mtext(3,text=paste("l=",argv$rr_lscale," b=",argv$boxcox_lambda," pGE=",argv$pGE," sel=",argv$thinobs_perc," n=",argv$synsct_rr_nens),line=2, cex=2)
box()
devnull <- dev.off()
cat(paste("  written file",ffout,"\n"))
#
# score as a function of sct-threshold and intensity
ffout <- file.path( dir_out, paste0("synsct_rr_res_",score,"vsthint_l",argv$rr_lscale,"_b",bstr,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_rr_nens,".png"))
usod <- unique(vsod)
col <- rev(rainbow(length(usod)))
print(vth)
png( file=ffout, width=800, height=800)
par(mar=c(4,4,4,1))
plot( as.numeric(vth), vscore_s, xlab="",ylab="", main="", axes=F, ylim=ylim)
for (s in 1:length(usod)) {
  ix<-which(vsod==usod[s])
  lines(vth[ix],vscore_xl[ix],col=col[s],lwd=3)
#  lines(vth[ix],vscore_l[ix],col=col[s],lwd=3)
#  lines(vth[ix],vscore_m[ix],col=col[s],lwd=3)
#  lines(vth[ix],vscore_s[ix],col=col[s],lwd=3)
#  polygon( c(vth[ix],vth[ix[length(ix):1]]), c(vscore_v[ix],vscore_d[ix[length(ix):1]]), col=col[s], density=20)
}
points( as.numeric(vth), vscore_s, pch=21, bg="darkgray",cex=2)
abline(h=0)
legend(x="topright",lty=1,col=c("white",col),legend=c("sod",usod),cex=2,lwd=6)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext(1,text="SCT threshold",line=3, cex=2)
mtext(2,text=score_lab,line=2, cex=2)
mtext(3,text=paste("l=",argv$rr_lscale," b=",argv$boxcox_lambda," pGE=",argv$pGE," sel=",argv$thinobs_perc," n=",argv$synsct_rr_nens),line=2, cex=2)
box()
devnull <- dev.off()
cat(paste("  written file",ffout,"\n"))
#
# score as a function of sct-threshold
usod <- unique(vsod)
col <- rev(rainbow(length(usod)))
print(vth)
png( file=ffout, width=800, height=800)
par(mar=c(4,4,4,1))
plot( as.numeric(vth), vscore_s, xlab="",ylab="", main="", axes=F, ylim=ylim)
for (s in 1:length(usod)) {
  ix<-which(vsod==usod[s])
  lines(vth[ix],vscore_xl[ix],col=col[s],lwd=3)
#  lines(vth[ix],vscore_l[ix],col=col[s],lwd=3)
#  lines(vth[ix],vscore_m[ix],col=col[s],lwd=3)
#  lines(vth[ix],vscore_s[ix],col=col[s],lwd=3)
#  polygon( c(vth[ix],vth[ix[length(ix):1]]), c(vscore_v[ix],vscore_d[ix[length(ix):1]]), col=col[s], density=20)
}
points( as.numeric(vth), vscore_s, pch=21, bg="darkgray",cex=2)
abline(h=0)
legend(x="topright",lty=1,col=c("white",col),legend=c("sod",usod),cex=2,lwd=6)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext(1,text="SCT threshold",line=3, cex=2)
mtext(2,text=score_lab,line=2, cex=2)
mtext(3,text=paste("l=",argv$rr_lscale," b=",argv$boxcox_lambda," pGE=",argv$pGE," sel=",argv$thinobs_perc," n=",argv$synsct_rr_nens),line=2, cex=2)
box()
devnull <- dev.off()
cat(paste("  written file",ffout,"\n"))
#
q()
