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
dat <- read_sweetT( file=argv$ffin_sim, only_meta=T, close=F)
obsnet <- list( n=dat$meta$n, lat=dat$meta$lat, lon=dat$meta$lon,
                x=dat$meta$x, y=dat$meta$y, z=dat$meta$z, 
                prid=dat$meta$prid, isin=dat$meta$isin)
vertprof <- list( t0=dat$meta$t0, gamma=dat$meta$gamma,
                  h0=dat$meta$h0, h1i=dat$meta$h1i, a=dat$meta$a[argv$a_vertprof_ix] )
conn_in <- dat$conn
rm(dat)
obsnet_or <- obsnet
print("-- obsNet --")
print(obsnet$n)
#
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
  obsnet$isin <- obsnet$isin[ixkeep]
  print("-- obsNet --")
  print(obsnet$n)
} else {
  ixkeep <- 1:obsnet$n
}
#
#------------------------------------------------------------------------------
# read values
#values <- read_sweetT( conn=conn_in, open=T, close=T, ens=argv$a_vertprof_ix)$res_tot
datin <- read_sweetT( conn=conn_in, open=F, close=T, ens=argv$a_vertprof_ix)
values_or <- datin$res_tot[ixkeep]
#
#------------------------------------------------------------------------------
# SCT - Loop over fields
# initialization
debug <- F
obs_to_check <- rep( 1, obsnet$n)
background_values <- 0
# from the command line
tpos_score <- rep( argv$tpos_score, obsnet$n)
tneg_score <- rep( argv$tneg_score, obsnet$n)
t_sod <- rep( argv$t_sod, obsnet$n)
eps2 <- rep( argv$eps2, obsnet$n)
min_horizontal_scale <- argv$inner_radius/10
max_horizontal_scale <- argv$inner_radius
for (e in 1:argv$synsct_tg_nens) {
  values <- values_or
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
  res <- sct( obsnet$lat, obsnet$lon, obsnet$z, values, obs_to_check, background_values, argv$background_elab_type, argv$num_min, argv$num_max, argv$inner_radius, argv$outer_radius, argv$num_iterations, argv$num_min_prof, argv$min_elev_diff, min_horizontal_scale, max_horizontal_scale, argv$kth_closest_obs_horizontal_scale, argv$vertical_scale, argv$value_min, argv$value_max, argv$sig2o_min, argv$sig2o_max, eps2, tpos_score, tneg_score, t_sod, debug)
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
  r <- (a+c) * (a+b) / (a+b+c+d)
  ets <- (a-r) / (a+b+c-r)
  acc <- (a+d)/(a+b+c+d)
  pod <- a/(a+c)
  pofa <- b/(b+d)
  print( paste("a(bad) b c d", a,"(",length(which( true_flag==1)),")", b, c, d, a+b+c+d))
  print( paste("acc pod pofa ets", round(acc,2), round(pod,2), round(pofa,2), round(ets,2)))
  #
  if (!exists("conn_out")) conn_out<-NA
  conn_out <- write_sctRes( conn_out, argv$ffout, res, e, open=(e==1), close=(e==argv$synsct_tg_nens))
  print( paste("------ written",e,"/",argv$synsct_tg_nens,"--------------------------------"))
#  png(file=paste0("fig_",formatC(e,flag=0,width=3),".png"),width=800,height=800)
#  par(mar=c(1,1,1,1))
#  image(r,breaks=c(0,0.1,1,2,4,8,16,32,64,128,1000),col=c("beige",rev(rainbow(9))),xlab="",ylab="",main="",axes=F)
#  points(obsnet$x,obsnet$y)
#  ix <- which(res[[1]]==1)
#  points(obsnet$x[ix],obsnet$y[ix],pch=21,bg="yellow",cex=3)
#  box()
#  dev.off()
}
#
#------------------------------------------------------------------------------
rip( str="Normal Exit", code=0, t0=t0)
