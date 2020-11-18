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
dat <- read_sweetT( file=argv$ffin_sim, only_meta=T, close=F)
obsnet <- list( n=dat$meta$n, lat=dat$meta$lat, lon=dat$meta$lon,
                x=dat$meta$x, y=dat$meta$y, z=dat$meta$z,
                isin=dat$meta$isin)
nens <- dat$meta$nsamples
vertprof <- list( t0=dat$meta$t0, gamma=dat$meta$gamma,
                  h0=dat$meta$h0, h1i=dat$meta$h1i, a=dat$meta$a )
conn_in <- dat$conn
rm(dat)
#print("-- obsNet --")
#print(obsnet$n)
#
#------------------------------------------------------------------------------
# synsct_tg_res_a01_th02_sod02_pGE00_sel00_n002.dat

dir_in<-"/home/cristianl/data/sweet/synsct_tg/res"
dir_out<-"/home/cristianl/data/sweet/synsct_tg/res_png"
i<-0
res<-list()
vth <- vector()
vsod <- vector()
vscore <- vector()
vscore_in <- vector()
vscore_d <- vector()
vscore_s <- vector()
vscore_m <- vector()
isin <- rep( obsnet$isin, argv$synsct_tg_nens)
argv$pGE <- formatC( argv$pGE, width=2, flag="0")
argv$thinobs_perc <- formatC( argv$thinobs_perc, width=2, flag="0")
argv$synsct_tg_nens <- formatC( argv$synsct_tg_nens, width=3, flag="0")
argv$a_vertprof_ix <- formatC( argv$a_vertprof_ix, width=2, flag="0")
if ( argv$pGE == "00" ) {
  score <- "pofd"
  score_lab <- "POFD"
  ylim <- c(0,0.1)
} else {
  score <- "ets"
  score_lab <- "ETS"
  ylim <- c(0,1)
}
for (th in argv$t_score_eva) {
  for (sod in argv$t_sod_eva) {
    ffin <- file.path( dir_in,
             paste0("synsct_tg_res_a",argv$a_vertprof_ix,"_th",th,"_sod",sod,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_tg_nens,".dat"))
#    print( ffin)
    if ( !file.exists( ffin)) next
    i <- i+1
    res[[i]] <- read_sctRes( file=ffin)
    vth[i] <- th
    vsod[i] <- sod
    ix <- which( res[[i]][,2] != argv$undef)
    vscore[i] <- score_fun(x=res[[i]][ix,2], x_ref=res[[i]][ix,15], lab=score, threshold=.9, threshold1=.9, type="above") 
    ix <- which( res[[i]][,13] > 0.85 & res[[i]][,2] != argv$undef)
    if ( length(ix) > 0) vscore_d[i] <- score_fun(x=res[[i]][ix,2], x_ref=res[[i]][ix,15], lab=score, threshold=.9, threshold1=.9, type="above") 
    ix <- which( res[[i]][,13] > 0.45 & res[[i]][,13] <= 0.85 & res[[i]][,2] != argv$undef)
    if ( length(ix) > 0) vscore_m[i] <- score_fun(x=res[[i]][ix,2], x_ref=res[[i]][ix,15], lab=score, threshold=.9, threshold1=.9, type="above") 
    ix <- which( res[[i]][,13] <= 0.45 & res[[i]][,2] != argv$undef)
    if ( length(ix) > 0) vscore_s[i] <- score_fun(x=res[[i]][ix,2], x_ref=res[[i]][ix,15], lab=score, threshold=.9, threshold1=.9, type="above") 
    ix <- which( res[[i]][,2] != argv$undef & isin==1)
    vscore_in[i] <- score_fun(x=res[[i]][ix,2], x_ref=res[[i]][ix,15], lab=score, threshold=.9, threshold1=.9, type="above") 
  }
}
#
# score as a function of sct-threshold
ffout <- file.path( dir_out, paste0("synsct_tg_res_",score,"vsth_a",argv$a_vertprof_ix,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_tg_nens,".png"))
usod <- unique(vsod)
col <- rev(rainbow(length(usod)))
png( file=ffout, width=800, height=800)
par(mar=c(4,4,4,1))
plot( as.numeric(vth), vscore, xlab="",ylab="", main="", axes=F, ylim=ylim)
for (s in 1:length(usod)) {
  ix<-which(vsod==usod[s])
  lines(vth[ix],vscore[ix],col=col[s],lwd=3)
#  lines(vth[ix],vscore_d[ix],col=col[s],lwd=3,lty=2)
#  lines(vth[ix],vscore_m[ix],col=col[s],lwd=3,lty=2)
#  lines(vth[ix],vscore_s[ix],col=col[s],lwd=3,lty=2)
  polygon( c(vth[ix],vth[ix[length(ix):1]]), c(vscore_s[ix],vscore_d[ix[length(ix):1]]), col=col[s], density=20)
}
points( as.numeric(vth), vscore, pch=21, bg="darkgray",cex=2)
abline(h=0)
legend(x="topright",lty=1,col=c("white",col),legend=c("sod",usod),cex=2,lwd=6)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext(1,text="SCT threshold",line=3, cex=2)
mtext(2,text=score_lab,line=2, cex=2)
mtext(3,text=paste("a=",argv$a_vertprof_ix," pGE=",argv$pGE," sel=",argv$thinobs_perc," n=",argv$synsct_tg_nens),line=2, cex=2)
box()
devnull <- dev.off()
cat(paste("  written file",ffout,"\n"))
#
# score as a function of sct-threshold inside Folldal
ffout <- file.path( dir_out, paste0("synsct_tg_resin_",score,"vsth_a",argv$a_vertprof_ix,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_tg_nens,".png"))
usod <- unique(vsod)
col <- rev(rainbow(length(usod)))
png( file=ffout, width=800, height=800)
par(mar=c(4,4,4,1))
plot( as.numeric(vth), vscore_in, xlab="",ylab="", main="", axes=F, ylim=c(0,1))
for (s in 1:length(usod)) {
  ix<-which(vsod==usod[s])
  lines(vth[ix],vscore_in[ix],col=col[s],lwd=3)
}
points( as.numeric(vth), vscore_in, pch=21, bg="darkgray",cex=2)
abline(h=0)
legend(x="topright",lty=1,col=c("white",col),legend=c("sod",usod),cex=2,lwd=6)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext(1,text="SCT threshold",line=3, cex=2)
mtext(2,text=score_lab,line=2, cex=2)
mtext(3,text=paste("a=",argv$a_vertprof_ix," pGE=",argv$pGE," sel=",argv$thinobs_perc," n=",argv$synsct_tg_nens),line=2, cex=2)
box()
devnull <- dev.off()
cat(paste("  written file",ffout,"\n"))
q()
