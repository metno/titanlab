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
#print("-- obsNet --")
#print(obsnet$n)
#
#------------------------------------------------------------------------------
# Read input nc-file
if ( !file.exists( argv$ffin_fields)) boom(str=argv$ffin_fields, code=1) 
nc <- nc_open( argv$ffin_fields, readunlim=FALSE )
v1 <- nc$var[[1]]
data <- ncvar_get( nc, v1 ) 
x <- nc$dim[[1]]$vals
y <- nc$dim[[2]]$vals
ens <- nc$dim[[3]]$vals
proj4 <- ncatt_get( nc, varid=nc$var[[2]], attname="proj4")$value
nc_close( nc)
# dim(data) x,y,ensemble
nx <- length( x)
ny <- length( y)
nens <- length( ens)
dx <- abs( x[2] - x[1])
dy <- abs( y[2] - y[1])
xmin <- min(x) - dx/2
xmax <- max(x) + dx/2
ymin <- min(y) - dy/2
ymax <- max(y) + dy/2
r <- raster( extent( xmin, xmax, ymin, ymax), res = c( dx, dy), crs = proj4)
#print("-- created raster --")
#print(r)
#
#------------------------------------------------------------------------------
dir_in<-"/home/cristianl/data/sweet/synsct_rr/res"
dir_out<-"/home/cristianl/data/sweet/synsct_rr/res_png"
i<-0
res<-list()
ntot <- vector()
iso <- vector()
vth <- vector()
vsod <- vector()
vscore <- vector()
vinnov <- vector()
vscore_in <- vector()
vscore_d <- vector()
vscore_v <- vector()
rr <- c( 0.1, 1, 2, 4, 8, 16, 32, 64, 128)
nrr <- length( rr)
vscore_r <- array( data=NA, dim=c( length(argv$t_score_eva) * length(argv$t_sod_eva), nrr)) 
n_r <- array( data=NA, dim=c( length(argv$t_score_eva) * length(argv$t_sod_eva), nrr)) 
argv$rr_lscale <- formatC( argv$rr_lscale, width=6, flag="0", format="fg")
argv$pGE <- formatC( argv$pGE, width=2, flag="0")
argv$thinobs_perc <- formatC( argv$thinobs_perc, width=2, flag="0")
argv$synsct_rr_nens <- formatC( argv$synsct_rr_nens, width=4, flag="0", format="fg")
if ( argv$boxcox_lambda == 0.3) {
  bstr <- "03"
} else {
  bstr <- "05"
}
if ( argv$pGE == "00" ) {
  score <- "pofd"
  score_lab <- "POFD"
  ylim <- c(0,0.18)
} else {
  score <- "ets"
  score_lab <- "ETS"
  ylim <- c(0,1)
}
#
#-----------------------------------------------------------------------------------------------
for (th in argv$t_score_eva) {
  for (sod in argv$t_sod_eva) {
    ffin <- file.path( dir_in,
             paste0("synsct_rr_res_l",argv$rr_lscale,"_b",bstr,"_th",th,"_sod",sod,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_rr_nens,".dat"))
    cat( paste("----> file", ffin,"\n"))
    if ( !file.exists( ffin)) next
    i <- i+1
    res[[i]] <- read_sctRes( file=ffin)
    vth[i] <- as.numeric(th)
    vsod[i] <- as.numeric(sod)
    #
    undef <- unique(res[[i]][which(res[[i]][,11]<(-500)),11])
    if (length(undef)==0) undef<-(-999)
    iso[i] <- length( which( res[[i]][,2] %in% c(11,12))) 
    ntot[i] <- length( res[[i]][,2] %in% c(0,1,11,12))
    ix <- which( res[[i]][,2] %in% c(0,1) & res[[i]][,15] %in% c(0,1))
    if ( length(ix) > 0) vscore[i] <- score_fun(x=res[[i]][ix,2], x_ref=res[[i]][ix,15], lab=score, threshold=.9, threshold1=.9, type="above") 
#    ix <- which( res[[i]][,13] > 0.45 & res[[i]][,2] != undef)
#    if ( length(ix) > 0) vscore_d[i] <- score_fun(x=res[[i]][ix,2], x_ref=res[[i]][ix,15], lab=score, threshold=.9, threshold1=.9, type="above") 
#    ix <- which( res[[i]][,13] <= 0.45 & res[[i]][,2] != undef)
#    if ( length(ix) > 0) vscore_v[i] <- score_fun(x=res[[i]][ix,2], x_ref=res[[i]][ix,15], lab=score, threshold=.9, threshold1=.9, type="above")
    for (j in 1:nrr) {
      ix <- which( res[[i]][,17] >= rr[j] & res[[i]][,2] %in% c(0,1) & res[[i]][,15] %in% c(0,1))
      n_r[i,j] <- length(ix)
      if ( length(ix) > 0) vscore_r[i,j] <- score_fun(x=res[[i]][ix,2], x_ref=res[[i]][ix,15], lab=score, threshold=.9, threshold1=.9, type="above")
    }
    #
    ffout <- file.path( dir_out, paste0("synsct_rr_res_hourglass_l",argv$rr_lscale,"_b",bstr,"_th",th,"_sod",sod,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_rr_nens,".png"))
    hourglass_contingencyTable( ffout=ffout, 
                                an_res=res[[i]][,9],
                                cv_res=res[[i]][,10],
                                ge=res[[i]][,15],
                                dqc=res[[i]][,2],
                                sig2o=res[[i]][,14],
                                sct=res[[i]][,3],
                                sod=res[[i]][,5],
                                par=list(png_width=1200,png_height=1200,undef=undef,probs=c(0.01, 0.25, 0.5, 0.75, 0.99),sig2o=c(1, 10)))
    #
    ffout <- file.path( dir_out, paste0("synsct_rr_res_map_l",argv$rr_lscale,"_b",bstr,"_th",th,"_sod",sod,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_rr_nens,".png"))
    png( file=ffout, width=1200, height=1200)
    aux <- res[[i]][,1] == 1 & !is.na(res[[i]][,2]) 
    ia <- which( aux & res[[i]][,15] == 1 & res[[i]][,2] == 1)
    ib <- which( aux & res[[i]][,15] == 0 & res[[i]][,2] == 1)
    ic <- which( aux & res[[i]][,15] == 1 & res[[i]][,2] == 0)
    id <- which( aux & res[[i]][,15] == 0 & res[[i]][,2] == 0)
    n <- length(which(aux))
    par ( mar = c( 1, 1, 1, 1))
#    plot( obsnet$x, obsnet$y, pch=21, col="white", bg="white", xlab="", ylab="", axes=F)
    dat<-t(data[,,1])
    r[]<-dat
    image(r, breaks=c(0,0.1,2,4,8,16,32,64,128,256,512), col=c("gray",rev(rainbow(9))), axes=F)
    points( obsnet$x[id], obsnet$y[id], pch=21, bg= "green", cex=2)
    points( obsnet$x[ia], obsnet$y[ia], pch=21, bg= "gold", cex=2)
    points( obsnet$x[ic], obsnet$y[ic], pch=21, bg= "cornflowerblue", cex=2)
    points( obsnet$x[ib], obsnet$y[ib], pch=21, bg= "pink", cex=2)
    lines(c(xmin+10000,xmin+60000),c(ymin+10000,ymin+10000),lwd=8)
    lines(c(xmin+10000,xmin+10000),c(ymin+10000,ymin+60000),lwd=8)
    legend( x="topleft", pch=c(21,21,21,21), pt.bg=c("gold","pink","cornflowerblue","green"), cex=2.5,
            legend=c("hits(T1/F1)","falsePos(0/1)","misses(1/0)","corrNeg(0/0)"))
    dev.off()
    cat(paste("  written file",ffout,"\n"))
  }
}
print("isolated cases")
print(iso)
print("total number of cases")
print(ntot)
#
#----------------------------------------------------------------------------------------------
usod <- as.numeric( argv$t_sod_eva)
col_usod <- rev( rainbow( length( usod)))
uth <- as.numeric( argv$t_score_eva)
col_uth <- rev( rainbow( length( uth)))
l_str <- round( as.numeric( argv$rr_lscale) / 1000, 0)
#
#----------------------------------------------------------------------------------------------
# score as a function of sct-threshold
ffout <- file.path( dir_out, paste0("synsct_rr_res_",score,"vsth_l",argv$rr_lscale,"_b",bstr,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_rr_nens,".png"))
png( file=ffout, width=800, height=800)
par(mar=c(5,5,4,1))
plot( as.numeric(vth), vscore, xlab="",ylab="", main="", axes=F, ylim=ylim, col="white")
for (s in 1:length(usod)) {
  ix<-which(vsod==usod[s])
  lines(vth[ix],vscore[ix],col=col_usod[s],lwd=4)
}
points( as.numeric(vth), vscore, col=col_usod[1],cex=2)
abline(h=0)
#legend(x="topright",lty=1,col=c("white",col_usod),legend=c("sod",usod),cex=2,lwd=6)
axis(1,cex.axis=1.5)
axis(2,cex.axis=1.5)
mtext(1,text="SCT threshold",line=3, cex=2)
mtext(2,text=score_lab,line=2, cex=2)
if ( argv$thinobs_perc == "00") thinobs_str <- "100%"
if ( argv$thinobs_perc == "50") thinobs_str <- "50%"
if ( score == "pofd" ) {
  mtext( 3, text=paste0("l=",l_str,"km, Box-Cox transf=",argv$boxcox_lambda,", data used=",thinobs_str), line=2, cex=2.5)
  abline( h=seq(-10,10, by=0.05), lty=2, col="gray")
} else {
  mtext( 3, text=paste0("l=",l_str,"km, Pge=",argv$pGE,", Box-Cox transf=",argv$boxcox_lambda,", data used=",thinobs_str), line=2, cex=2.5)
  abline( h=seq(-10,10, by=0.1), lty=2, col="gray")
}
box()
devnull <- dev.off()
cat( paste( "====> written file", ffout, "\n"))
#
#----------------------------------------------------------------------------------------------
# score as a function of precipitation amount
ffout <- file.path( dir_out, paste0("synsct_rr_res_",score,"vsthint_l",argv$rr_lscale,"_b",bstr,"_pGE",argv$pGE,"_sel",argv$thinobs_perc,"_n",argv$synsct_rr_nens,".png"))
#print(vscore_r)
#print(n_r)
if ( score == "pofd") xleg <- "topleft"
if ( score == "ets")  xleg <- "topright"
png( file=ffout, width=800, height=800)
par( mar=c(5,5,4,1))
plot( 1:nrr, 1:nrr, xlab="",ylab="", main="", axes=F, ylim=ylim, col="white")
for (s in 1:length(usod)) {
  for (t in 1:length(uth)) {
    ix<-which( vsod == usod[s] & vth == uth[t] )
    lines(1:nrr, vscore_r[ix,], col=col_uth[t], lwd=4)
  }
}
abline(h=0)
axis( 1, cex.axis=1.5, at=1:nrr, labels=rr)
axis( 2, cex.axis=1.5)
mtext( 1, text="precipitation amount greater than (mm)",line=3, cex=2)
mtext( 2, text=score_lab, line=2.3, cex=2)
if ( score == "pofd" ) {
  mtext( 3, text=paste0("l=",l_str,"km, Box-Cox transf=",argv$boxcox_lambda,", data used=",thinobs_str), line=2, cex=2.5)
  abline( h=seq(-10,10, by=0.05), lty=2, col="gray")
} else {
  mtext( 3, text=paste0("l=",l_str,"km, Pge=",argv$pGE,", Box-Cox transf=",argv$boxcox_lambda,", data used=",thinobs_str), line=2, cex=2.5)
  abline( h=seq(-10,10, by=0.1), lty=2, col="gray")
}
legend( x=xleg, lty=1, col=c("white",col_uth), legend=c("Tsct",uth), cex=2, lwd=6, bg="white")
box()
devnull <- dev.off()
cat( paste( "====> written file", ffout, "\n"))
#
q()
