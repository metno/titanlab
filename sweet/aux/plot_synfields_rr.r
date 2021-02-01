#!/usr/bin/env Rscript
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
options( scipen = 999999999, warn=2)
#
# Load functions
titanlib_path <- "/home/cristianl/projects/titanlib/build/extras"
dyn.load( file.path( titanlib_path, 
                     paste("SWIG/R/titanlib", .Platform$dynlib.ext, sep="")))
source(   file.path( titanlib_path,"SWIG/R/titanlib.R"))
#
#------------------------------------------------------------------------------
# Constants
proj4.wgs84 <- "+proj=longlat +datum=WGS84"
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
if ( !file.exists( argv$ffin_fields)) boom(str=argv$ffin_fields, code=1) 
nc   <- nc_open( argv$ffin_fields, readunlim=FALSE )
v1   <- nc$var[[1]]
data <- ncvar_get( nc, v1 ) 
x   <- nc$dim[[1]]$vals
y   <- nc$dim[[2]]$vals
ens <- nc$dim[[3]]$vals
proj4 <- ncatt_get( nc, varid=nc$var[[2]], attname="proj4")$value
nc_close( nc)
# dim(data) x,y,ensemble
nx   <- length( x)
ny   <- length( y)
nens <- length( ens)
dx   <- abs( x[2] - x[1])
dy   <- abs( y[2] - y[1])
xmin <- min(x) - dx/2
xmax <- max(x) + dx/2
ymin <- min(y) - dy/2
ymax <- max(y) + dy/2
r  <- raster( extent( xmin, xmax, ymin, ymax), res = c( dx, dy), crs = proj4)
print("-- created raster --")
print(r)
#
#------------------------------------------------------------------------------
# Read observational network
obsnet <- read_obsNet( file=argv$ffin_obs, 
                       crs_in=proj4.wgs84,
                       crs_out=proj4,
                       exclude_prid = 100, 
                       extent_out=extent( xmin, xmax, ymin, ymax))
obsnet_or <- obsnet
print("-- obsNet --")
print(obsnet$n)
#
#------------------------------------------------------------------------------
# 
proj4.lcc<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06" 
b_utm33<-readOGR("/home/cristianl/data/geoinfo/TM_WORLD_BORDERS_UTM33/TM_WORLD_BORDERS_UTM33-0.2.shp","TM_WORLD_BORDERS_UTM33-0.2",verbose=F)
b<-spTransform(b_utm33,CRS(proj4.lcc))
#
library(fool)
col<-load_color_table(path="/home/cristianl/projects/rpackages/fool/color_tables",abbrv="precip_11lev")
ncl<-length(col)
print(ncl)
#breaks_mean<-round(c(0,seq(0.1,34,length=ncl)),1)
breaks_mean<-round(c(0,0.1,1,2,4,8,15,25,50,75,100,150,200),1)
#--------------------------------------------------------------
if (argv$ffin_fields=="/home/cristianl/data/sweet/rr/synrr_l010000_n0100.nc") lab <- c("(a)","(b)","(c)")
if (argv$ffin_fields=="/home/cristianl/data/sweet/rr/synrr_l100000_n0100.nc") lab <- c("(d)","(e)","(f)")
if (argv$ffin_fields=="/home/cristianl/data/sweet/rr/synrr_l200000_n0100.nc") lab <- c("(g)","(h)","(i)")
r[]<-t(data[,,1])
png( file=paste0(argv$ffout,"1.png"), width=800, height=800)
par( mar = c(2,2.5,0.5,0.5) )
image( r, col=col, breaks=breaks_mean, xlab="",main="",ylab="", cex.axis=2)
lines(b,lwd=4,col="gray30")
if (lab[1]=="(a)") {
  points(obsnet$x,obsnet$y,cex=2)
  legend(x="topright",fill=rev(col),legend=rev(breaks_mean[1:ncl]),bg="white",cex=3)
}
text(x=-600000,y=-50000,cex=5,labels=lab[1])
dev.off()
r[]<-t(data[,,2])
png( file=paste0(argv$ffout,"2.png"), width=800, height=800)
par( mar = c(2,2.5,0.5,0.5) )
image( r, col=col, breaks=breaks_mean, xlab="",main="",ylab="", cex.axis=2)
lines(b,lwd=4,col="gray30")
text(x=-600000,y=-50000,cex=5,labels=lab[2])
dev.off()
r[]<-t(data[,,3])
png( file=paste0(argv$ffout,"3.png"), width=800, height=800)
par( mar = c(2,2.5,0.5,0.5) )
image( r, col=col, breaks=breaks_mean, xlab="",main="",ylab="", cex.axis=2)
lines(b,lwd=4,col="gray30")
text(x=-600000,y=-50000,cex=5,labels=lab[3])
dev.off()

q()
