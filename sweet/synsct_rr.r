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
#
#------------------------------------------------------------------------------
# Define a raster based on the input fields
r <- raster( extent( xmin, xmax, ymin, ymax), res = c( dx, dy), crs = proj4)
print("-- created raster --")
print(r)
#
#------------------------------------------------------------------------------
# Read observational network
obsnet <- read_obsNet( file=argv$ffin_obs, crs_in=proj4.wgs84, crs_out=proj4,
                       exclude_prid = 100, 
                       extent_out=extent( xmin, xmax, ymin, ymax))
obsnet_or <- obsnet
print("-- obsNet --")
print(obsnet$n)
#------------------------------------------------------------------------------
# thinning of observations
flag <- F
if ( !is.na( argv$thinobs_perc)) {
  ix <- 1:obsnet$n
  if ( !is.na( argv$thinobs_prid)) {
    ix <- which( obsnet$prid == argv$thinobs_prid)
  }
  if (argv$thinobs_perc>0) {
    nrem <- ceiling( length( ix) * argv$thinobs_perc / 100)
    ixrem <- sample(ix, nrem)
    ixkeep <- (1:obsnet$n)[-ixrem]
    flag <- T
  } else {
    ixkeep <- 1:obsnet$n
  }
  obsnet$n <- length(ixkeep)
  obsnet$lat <- obsnet$lat[ixkeep]
  obsnet$lon <- obsnet$lon[ixkeep]
  obsnet$x <- obsnet$x[ixkeep]
  obsnet$y <- obsnet$y[ixkeep]
  obsnet$z <- obsnet$z[ixkeep]
  obsnet$prid <- obsnet$prid[ixkeep]
  print("-- obsNet --")
  print(obsnet$n)
} else {
  ixkeep <- 1:obsnet$n
}
#
#------------------------------------------------------------------------------
# SCT - Loop over fields
# initialization
debug <- F
obs_to_check <- rep( 1, obsnet$n)
elevs        <- rep( 0, obsnet$n)
background_values <- 0
# from the command line
tpos_score <- rep( argv$tpos_score, obsnet$n)
tneg_score <- rep( argv$tneg_score, obsnet$n)
t_sod <- rep( argv$t_sod, obsnet$n)
eps2 <- rep( argv$eps2, obsnet$n)
min_horizontal_scale <- argv$inner_radius/10
max_horizontal_scale <- argv$inner_radius
for (e in 1:nens) {
  dat<-t(data[,,e])
  r[]<-dat
  values <- extract( r, cbind( obsnet$x, obsnet$y))
  if ( !is.na( argv$pGE)) {
    nbad <- ceiling( obsnet$n * argv$pGE / 100)
    true_flag <- rep( 0, obsnet$n)
    ixbad <- sample(1:obsnet$n, nbad)
    true_flag[ixbad] <- rep(1,nbad)
    values[ixbad] <- runif( nbad, min = argv$value_min, max = argv$value_max)
  } else {
    true_flag <- rep(0,obsnet$n)
  }
  if ( !is.na( argv$boxcox_lambda)) 
    values <- boxcox( values, argv$boxcox_lambda)
  # 
  t00<-Sys.time()
  res <- sct( obsnet$lat, obsnet$lon, elevs, values, obs_to_check, background_values, argv$background_elab_type, argv$num_min, argv$num_max, argv$inner_radius, argv$outer_radius, argv$num_iterations, argv$num_min_prof, argv$min_elev_diff, min_horizontal_scale, max_horizontal_scale, argv$kth_closest_obs_horizontal_scale, argv$vertical_scale, argv$value_min, argv$value_max, argv$sig2o_min, argv$sig2o_max, eps2, tpos_score, tneg_score, t_sod, debug)
  print(Sys.time()-t00)
  nres <- length(res)
  res[[nres+1]] <- true_flag
  if ( flag) {
    res_bak <- res
    for (i in 1:length(res)) {
      res[[i]] <- vector(mode="numeric", length=obsnet_or$n)
      res[[i]][] <- argv$undef
      res[[i]][ixkeep] <- res_bak[[i]]
    }
    true_flag <- res[[nres+1]]
  } 
  #
  a <- length( which( true_flag==1 & res[[1]]==1))
  c <- length( which( true_flag==1 & res[[1]]==0))
  b <- length( which( true_flag==0 & res[[1]]==1))
  d <- length( which( true_flag==0 & res[[1]]==0))
  rand <- (a+c) * (a+b) / (a+b+c+d)
  ets <- (a-rand) / (a+b+c-rand)
  acc <- (a+d)/(a+b+c+d)
  pod <- a/(a+c)
  pofa <- b/(b+d)
  print( paste("a(bad) b c d", a,"(",length(which( true_flag==1)),")", b, c, d, a+b+c+d))
  print( paste("acc pod pofa ets", round(acc,2), round(pod,2), round(pofa,2), round(ets,2)))
  #
  if (!exists("conn_out")) conn_out<-NA
  conn_out <- write_sctRes( conn_out, argv$ffout, res, e, open=(e==1), close=(e==nens))
  print( paste("------ written",e,"/",nens,"--------------------------------"))
  png(file=paste0("fig_",formatC(e,flag=0,width=3),".png"),width=800,height=800)
  par(mar=c(1,1,1,1))
  image(r,breaks=c(0,0.1,1,2,4,8,16,32,64,128,1000),col=c("beige",rev(rainbow(9))),xlab="",ylab="",main="",axes=F)
  points(obsnet$x,obsnet$y)
  ix <- which(res[[1]]==1)
  points(obsnet$x[ix],obsnet$y[ix],pch=21,bg="yellow",cex=3)
  box()
  aux <- dev.off()
}
#
#------------------------------------------------------------------------------
rip( str="Normal Exit", code=0, t0=t0)
