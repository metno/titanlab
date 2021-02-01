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
  values <- round(values,1)
  values_or <- values 
  values_minok <- values_or-1
  values_maxok <- values_or+1
  values_min <- argv$value_min
  values_max <- argv$value_max
  values_low <- values_or-20
  values_up  <- values_or+20
  #
  bkg <- rep(background_values,length(obsnet$lat))
  t00<-Sys.time()
  res<-sct( as.numeric(obsnet$lat), as.numeric(obsnet$lon), as.numeric(obsnet$z), as.numeric(values), as.integer(obs_to_check), as.numeric(bkg), as.character(argv$background_elab_type), as.integer(argv$num_min), as.integer(argv$num_max), as.numeric(argv$inner_radius), as.numeric(argv$outer_radius), as.integer(argv$num_iterations), as.integer(argv$num_min_prof), as.numeric(argv$min_elev_diff), as.numeric(min_horizontal_scale), as.numeric(max_horizontal_scale), as.integer(argv$kth_closest_obs_horizontal_scale), as.numeric(argv$vertical_scale), as.numeric(values_min), as.numeric(values_max), as.numeric(values_low), as.numeric(values_up), as.numeric(values_minok), as.numeric(values_maxok), as.numeric(eps2), as.numeric(tpos_score), as.numeric(tneg_score), debug)
  print(Sys.time()-t00)
  nres <- length(res)
#  flag  <- res[[1]]
#  score <- res[[2]]
  res[[nres+1]] <- true_flag
  res[[nres+2]] <- values 
  res[[nres+3]] <- values_or 
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
  a <- which( true_flag==1 & res[[1]]==1)
  c <- which( true_flag==1 & res[[1]]==0)
  b <- which( true_flag==0 & res[[1]]==1)
  d <- which( true_flag==0 & res[[1]]==0)
  iso <- which( res[[1]]>1)
print(res[[1]][iso])
  png("out.png",width=800,height=800)
  plot(values_or,obsnet$z)
  points(values_or[d],obsnet$z[d],pch=21,bg="cornflowerblue")
  points(values_or[a],obsnet$z[a],pch=21,bg="red")
  points(values_or[b],obsnet$z[b],pch=21,bg="pink")
  points(values_or[c],obsnet$z[c],pch=21,bg="cyan")
  ix<-which(res[[2]]>=0)
  text(values_or[ix],obsnet$z[ix],round(res[[2]][ix],1),cex=1.5,col="darkred")
  dev.off()
  ff<-"/home/cristianl/data/geoinfo/meps_gmted2010_1km_topo_topdown.nc"
  ex<-as(extent(-340000,-150000,-180000,0),'SpatialPolygons'); crs(ex)<-CRS("+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06")
  raux<-try(read_dotnc(nc.file=ff,
                       nc.varname="altitude",
                       topdown=F,
                       out.dim=list(ndim=3,
                                    tpos=3,
                                    epos=NULL,
                                    names=c("x","y","time")),
                       proj4="+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06",
                       nc.proj4=list(var=NULL,
                                     att=NULL),
                       selection=list(t=nc4.getTime(ff,format="%Y%m%d%H%M%S")[1],
                                      e=NULL,t_format="%Y%m%d%H%M%S")))
  r <- disaggregate( crop(raux$stack,ex), fact=4,method="bilinear")
  png("map.png",width=800,height=800)
  par(mar=c(2,2,0.5,0.5))
  plot(obsnet$x,obsnet$y,col="white", xlab="",ylab="")
  col<-gray(seq(0.3,0.9,length=15))
  image(r,add=T,col=col,breaks=seq(150,2000,length=16))
  ix<-which(res[[2]]<0)
  points(obsnet$x[ix],obsnet$y[ix],pch=21,bg="gray",cex=1)
  points(obsnet$x[d],obsnet$y[d],pch=21,bg="cornflowerblue",cex=1)
  points(obsnet$x[a],obsnet$y[a],pch=21,bg="red",cex=1)
  points(obsnet$x[b],obsnet$y[b],pch=21,bg="pink",cex=1)
  points(obsnet$x[c],obsnet$y[c],pch=21,bg="cyan",cex=1)
  points(obsnet$x[iso],obsnet$y[iso],pch=23,bg="gray",cex=2)
  ix<-which(res[[2]]>=0)
  text(obsnet$x[a],obsnet$y[a],round(res[[2]][a],1),cex=1.5,col="black")
  text(obsnet$x[c],obsnet$y[c],round(res[[2]][c],1),cex=1.5,col="darkred")
  dev.off()
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
