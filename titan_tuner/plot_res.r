#!/usr/bin/env Rscript
# --~- titan_tuner_sctres.r -~--
# 
# See the software repository here: 
#..............................................................................
#Copyright and license
# Copyright (C) 2018 MET Norway. The software is licensed under GPL version 3 
# or (at your option) any later version.
# https://www.gnu.org/licenses/gpl-3.0.en.html
# 
# History:
# 07.04.2021 - Cristian Lussana. Original code.
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
suppressPackageStartupMessages( library( "RANN"))
#options(warn = 2, scipen = 999)
options( bitmapType="cairo", scipen = 999999999)

#
# -----------------------------------------------------------------------------
# Constants
# CRS strings
proj4.wgs84     <- "+proj=longlat +datum=WGS84"
proj4.ETRS_LAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
proj4.utm33     <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4.lcc       <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
ffout_default   <- "out.nc"

# -----------------------------------------------------------------------------
# FUNCTIONS

# + manage fatal error
boom <- function( str=NA, code=NA) {
  cat("Fatal Error ")
  if ( !is.na(code)) {
    if ( code == 1) cat("file not found ")
  }
  if ( !is.na(str)) cat( str)
  cat("\n")
  quit( status= 1)
}

#+ the end 
rip <- function( str=NA, code=NA, t0=NA) {
  cat( "the End : ")
  if ( !is.na(code) ) {
    if ( code == 1 ) cat( "normal exit : ")
  }
  if ( !is.na(t0)) {
    t1 <- Sys.time()
    cat( paste( "total time=", round(t1-t0,1), attr(t1-t0,"unit")," "))
  }
  if ( !is.na(str)) cat( str)
  cat("\n")
  quit( status= 0 )
}


#==============================================================================
# MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN -
#==============================================================================
t0<-Sys.time()
# path to the titan functions is stored in the enviroment var TITANR_FUN
ttuner_path <- Sys.getenv( "TTUNER_PATH")
#------------------------------------------------------------------------------
# Load functions
for (file in list.files(path = ttuner_path, pattern = ".r", full.names=T) ) 
  source(file)
rm(file)
#-----------------------------------------------------------------------------
# Read command line arguments
argv <- ttun_argparser()
#------------------------------------------------------------------------------
# Time sequence

res <- ttun_timeseq( argv)
n_tseq <- res$n_tseq
tseq   <- res$tseq
rm( res)

#------------------------------------------------------------------------------
# Read Input data 

data <- integer(0)
for (f in 1:length(argv$ffin_to_plot)) { # MAIN LOOP @@BEGIN@@ (jump to @@END@@)
  if (!file.exists(argv$ffin_to_plot[f])) {
    print(paste("file not found",argv$ffin_to_plot[f]))
    next
  }
  aux <- read.table( file=argv$ffin_to_plot[f], header=T, sep=";", 
                     stringsAsFactors=T, strip.white=T)
  ix <- which( strptime( aux$time, format="%Y-%m-%dT%H", tz="UTC") %in% tseq)
  if ( length(ix) == 0) next
  data <- rbind( data, aux[ix,])
} # @@END@@
rm( aux, ix)

#------------------------------------------------------------------------------
# 

vrad <- sort( unique( data$radius))
vthr <- sort( unique( data$thr))
nrad <- length( vrad)
nthr <- length( vthr)

pod4mean  <- array( data=NA, dim=c(nrad,nthr))
pofa4mean <- array( data=NA, dim=c(nrad,nthr))
ets4mean  <- array( data=NA, dim=c(nrad,nthr))
niso4mean <- array( data=NA, dim=c(nrad,nthr))

for (i in 1:nrad) {
  flag <- data$radius == vrad[i]
  for (j in 1:nthr) {
    ix <- which( flag & data$thr == vthr[j])
    if ( length(ix) == 0) next
    pod4mean[i,j]  <- mean( data$pod[ix])
    pofa4mean[i,j] <- mean( data$pofa[ix])
    ets4mean[i,j]  <- mean( data$ets[ix])
    niso4mean[i,j] <- mean( data$p[ix] - (data$a[ix]+data$b[ix]+data$c[ix]+data$d[ix]))
    print( data$p[ix] - (data$a[ix]+data$b[ix]+data$c[ix]+data$d[ix]))
  }
}

col <- rev(rainbow(nrad))
png(file=paste0(argv$ffoutpref_plot,"_roc.png"),width=800,height=800)
plot(0,0,col="white",xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
lines(-100:100,-100:100,lwd=2,lty=2)
for (i in 1:nrad) {
  points( pofa4mean[i,], pod4mean[i,],pch=21,bg=col[i])
  lines( pofa4mean[i,], pod4mean[i,], col=col[i])
}
axis(1)
axis(2)
mtext(side=1,line=3,text="POFA")
mtext(side=2,line=3,text="POD")
legend(x="bottomright",fill=col,legend=vrad)
box()
aux <- dev.off()

png(file=paste0(argv$ffoutpref_plot,"_ets.png"),width=800,height=800)
plot(0,0,col="white",xlim=c(0,max(vthr)),ylim=c(min(c(0,ets4mean)),max(ets4mean)),axes=F,xlab="",ylab="")
#lines(-100:100,-100:100,lwd=2,lty=2)
for (i in 1:nrad) {
  points( vthr, ets4mean[i,],pch=21,bg=col[i])
  lines(  vthr, ets4mean[i,], col=col[i])
}
axis(1)
axis(2)
mtext(side=1,line=3,text="Threshold")
mtext(side=2,line=3,text="ETS")
legend(x="bottomright",fill=col,legend=vrad)
box()
aux <- dev.off()

png(file=paste0(argv$ffoutpref_plot,"_niso.png"),width=800,height=800)
plot(0,0,col="white",xlim=c(0,max(vthr)),ylim=c(0,max(c(10,niso4mean))),axes=F,xlab="",ylab="")
#lines(-100:100,-100:100,lwd=2,lty=2)
for (i in 1:nrad) {
  points( vthr, niso4mean[i,],pch=21,bg=col[i])
  lines(  vthr, niso4mean[i,], col=col[i])
}
axis(1)
axis(2)
mtext(side=1,line=3,text="Threshold")
mtext(side=2,line=3,text="niso")
legend(x="bottomright",fill=col,legend=vrad)
box()
aux <- dev.off()

#------------------------------------------------------------------------------
# Normal exit

rip( str="Normal Exit", code=0, t0=t0)
