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
#-----------------------------------------------------------------------------
# Multi-cores run
#if ( !is.na( argv$cores)) {
#  suppressPackageStartupMessages( library( "parallel"))
#  if ( argv$cores==0) argv$cores <- detectCores()
#  print( paste( "--> multi-core run, cores=", argv$cores))
#}

dyn.load( file.path( argv$titanlib_path, paste0( "titanlib", .Platform$dynlib.ext)))

source( file.path( argv$titanlib_path, "titanlib.R"))

#------------------------------------------------------------------------------
# Time sequence

res <- ttun_timeseq( argv)
n_tseq <- res$n_tseq
tseq   <- res$tseq
rm( res)

#------------------------------------------------------------------------------
# -.- Main loop overt time -.-
#  Read Input files / elaboration

n     <- 0
ui    <- diag(6)
first <- T

paropt_tot <- array( data=NA, dim=c( n_tseq, 6))

# prepare output file
cat( file=argv$ffout, append=F,
     "time;p;thr;kth;inner_r;outer_r;num_max_outer;vertical_scale;a;b;c;d;acc;pod;pofa;ets;\n")

# loop over all the n_tseq times
for (t in 1:n_tseq) { # MAIN LOOP @@BEGIN@@ (jump to @@END@@)

#  if (argv$verbose & t%%100==0) 

  cat( paste( "timestep", t, "/", n_tseq,
              "elapsed time", round(Sys.time()-t0,2), 
              attr(Sys.time()-t0,"unit"),"\n"))

  ffin <- replaceDate( string       = argv$ffin_template,
                       date.str     = format( tseq[t], format=argv$ffin_date.format,tz="GMT"),
                       year_string  = argv$year_string,
                       month_string = argv$month_string,
                       day_string   = argv$day_string,
                       hour_string  = argv$hour_string,
                       min_string   = argv$min_string,
                       sec_string   = argv$sec_string,
                       format       = argv$ffin_date.format)

  if (!file.exists(ffin)) {
    print(paste("file not found",ffin))
    return( NULL)
  }
  
  #
  #............................................................................
  # read data from input file
  
  data <- read_data()

  p <- length(data$lon)

  obsToCheck_lat    <- data$lat
  obsToCheck_lon    <- data$lon
  obsToCheck_z      <- data$z
  obsToCheck_val    <- data$val
  obsToCheck_prid   <- data$prid
  obsToCheck_chk    <- rep( 1, p)
  background_values <- 0 
  rm (data) 

  #............................................................................
  # statistics on distances

  xy <- spTransform( SpatialPoints( cbind( obsToCheck_lon, obsToCheck_lat), proj4string=CRS(proj4.wgs84)), CRS(proj4.lcc))
  obsToCheck_x <- attr( xy, "coords")[,1]
  obsToCheck_y <- attr( xy, "coords")[,2]
  nn2 <- nn2( cbind( obsToCheck_x, obsToCheck_y), 
              query = cbind( obsToCheck_x, obsToCheck_y), 
              k = 10) 
#              searchtype = "radius", radius = 500000)

  cat( paste0( "number of observations =", p, "\n"))
  cat( "average distances between a station and its nearest k-th station (k=1,2,...,10)\n")
  for (i in 1:10) cat( paste0( round( median(nn2$nn.dists[,i])), " "))
  cat("\n")

  if (argv$debug) {
    png( file="domain.png", width=800, height=800)
    plot( obsToCheck_lon, obsToCheck_lat)
    dev.off()
  }

  #............................................................................
  # prepare vectors of valid and admissible values

  if (argv$variable == "T") {

    background_elab_type.sct <- "VerticalProfileTheilSen"

    obsToCheck_mina <- obsToCheck_val - argv$Ta_delta
    obsToCheck_maxa <- obsToCheck_val + argv$Ta_delta
    obsToCheck_minv <- obsToCheck_val - argv$Tv_delta
    obsToCheck_maxv <- obsToCheck_val + argv$Tv_delta

    if ( !is.na( argv$PGE) & argv$PGE > 0) {

      if ( any( is.na( argv$PGE_prid))) argv$PGE_prid <- unique(obsToCheck_prid)

      res <- insert_ge( obsToCheck_val, obsToCheck_prid, argv$variable,
                        obsToCheck_mina, obsToCheck_maxa, 
                        obsToCheck_minv, obsToCheck_maxv,
                        argv$PGE, argv$PGE_prid, 1)

      ge             <- res$ge
      obsToCheck_val <- res$val 

      obsToCheck_mina <- obsToCheck_val - argv$Ta_delta
      obsToCheck_maxa <- obsToCheck_val + argv$Ta_delta
      obsToCheck_minv <- obsToCheck_val - argv$Tv_delta 
      obsToCheck_maxv <- obsToCheck_val + argv$Tv_delta
    }

  } else if (argv$variable == "RR") {

    background_elab_type.sct <- "MedianOuterCircle" 

    obsToCheck_mina <- pmin( pmax( obsToCheck_val - argv$RRa_delta, 0), 
                             pmax( obsToCheck_val - argv$RRa_fact * obsToCheck_val, 0))
    obsToCheck_maxa <- pmax( obsToCheck_val + argv$RRa_delta, obsToCheck_val + argv$RRa_fact * obsToCheck_val)
    obsToCheck_minv <- pmin( pmax( obsToCheck_val - argv$RRv_delta, 0), 
                             pmax( obsToCheck_val - argv$RRv_fact * obsToCheck_val, 0))
    obsToCheck_maxv <- pmax( obsToCheck_val + argv$RRv_delta, obsToCheck_val + argv$RRv_fact * obsToCheck_val)

    if ( !is.na( argv$PGE) & argv$PGE > 0) {

      if ( any( is.na( argv$PGE_prid))) argv$PGE_prid <- unique(obsToCheck_prid)

      res <- insert_ge( obsToCheck_val, obsToCheck_prid, argv$variable,
                        obsToCheck_mina, obsToCheck_maxa, 
                        obsToCheck_minv, obsToCheck_maxv,
                        argv$PGE, argv$PGE_prid, 2)

      ge             <- res$ge
      obsToCheck_val <- res$val 

      obsToCheck_mina <- pmin( pmax( obsToCheck_val - argv$RRa_delta, 0), 
                               pmax( obsToCheck_val - argv$RRa_fact * obsToCheck_val, 0))
      obsToCheck_maxa <- pmax( obsToCheck_val + argv$RRa_delta, obsToCheck_val + argv$RRa_fact * obsToCheck_val)
      obsToCheck_minv <- pmin( pmax( obsToCheck_val - argv$RRv_delta, 0), 
                               pmax( obsToCheck_val - argv$RRv_fact * obsToCheck_val, 0))
      obsToCheck_maxv <- pmax( obsToCheck_val + argv$RRv_delta, obsToCheck_val + argv$RRv_fact * obsToCheck_val)
    }

    obsToCheck_mina <- boxcox( x=obsToCheck_mina, lambda=argv$boxcox.lambda)
    obsToCheck_maxa <- boxcox( x=obsToCheck_maxa, lambda=argv$boxcox.lambda)
    obsToCheck_minv <- boxcox( x=obsToCheck_minv, lambda=argv$boxcox.lambda)
    obsToCheck_maxv <- boxcox( x=obsToCheck_maxv, lambda=argv$boxcox.lambda)
    obsToCheck_oval <- obsToCheck_val
    obsToCheck_val  <- boxcox( x=obsToCheck_val,  lambda=argv$boxcox.lambda)
  }

  ixe <- 1:p
  if ( !is.na( argv$PGE) & argv$PGE > 0) {
    if ( !any( is.na( argv$PGE_prid))) 
      ixe <- which( obsToCheck_prid %in% argv$PGE_prid) 
  }

  #............................................................................
  # Optimization (or testing) 

  # OPTimization if argv$theta is not defined
  if ( any( is.na( argv$theta))) {

    if (first) {
      theta <- argv$theta0
    } else { 
      theta <- paropt$par
    }

    paropt <- constrOptim( theta = theta, 
                           f=costf1, 
                           ui=ui, ci= argv$ci, 
                           grad=NULL,
                           outer.eps = 0.0001, 
                           control=list( fnscale=-1, parscale=argv$parscale))
  
    thr            <- paropt$par[1]
    kth            <- as.integer( paropt$par[2])
    inner_radius   <- paropt$par[3]
    outer_radius   <- max( c( paropt$par[3], paropt$par[4]))
    num_max_outer  <- as.integer( paropt$par[5])
    vertical_scale <- paropt$par[6]

  # TEST a set of parameters if argv$theta is defined
  } else {

    thr            <- argv$theta[1]
    kth            <- as.integer( argv$theta[2])
    inner_radius   <- argv$theta[3]
    outer_radius   <- max( c( argv$theta[3], argv$theta[4]))
    num_max_outer  <- as.integer( argv$theta[5])
    vertical_scale <- argv$theta[6]

  }

  paropt_tot[t,] <- c( thr, kth, inner_radius, outer_radius, num_max_outer, vertical_scale)

  flag <- rep( NA, p)

  for (i in 1:10) {

    ix <- which( flag %in% c(NA,0,11,12))

    obsToCheck_lat_i  <- obsToCheck_lat[ix]
    obsToCheck_lon_i  <- obsToCheck_lon[ix]
    obsToCheck_val_i  <- obsToCheck_val[ix]
    obsToCheck_chk_i  <- obsToCheck_chk[ix]
    obsToCheck_z_i    <- obsToCheck_z[ix]
    obsToCheck_mina_i <- obsToCheck_mina[ix]
    obsToCheck_maxa_i <- obsToCheck_maxa[ix]
    obsToCheck_minv_i <- obsToCheck_minv[ix]
    obsToCheck_maxv_i <- obsToCheck_maxv[ix]
    p_i <- length(ix)
    res <- sct_resistant(
                points = Points( obsToCheck_lat_i,
                                 obsToCheck_lon_i,
                                 obsToCheck_z_i),
                obsToCheck_val_i,
                obsToCheck_chk_i,
                rep( background_values, p_i),
                background_elab_type.sct,
                argv$num_min_outer.sct,
                num_max_outer,
                inner_radius,
                outer_radius,
                100,
                argv$num_min_prof.sct,
                argv$min_elev_diff.sct,
                argv$min_horizontal_scale.sct,
                argv$max_horizontal_scale.sct,
                kth,
                vertical_scale,
                obsToCheck_mina_i,
                obsToCheck_maxa_i,
                obsToCheck_minv_i,
                obsToCheck_maxv_i,
                rep( argv$eps2.sct, p_i),
                rep( thr, p_i),
                rep( thr, p_i),
                F)

    flag[ix] <- res[[1]]
    print(paste(length(ix),length(which(res[[1]]==1))))
  }

  a <- length( which( ge[ixe] == 1 & flag[ixe] == 1))
  c <- length( which( ge[ixe] == 1 & flag[ixe] == 0))
  b <- length( which( ge[ixe] == 0 & flag[ixe] == 1))
  d <- length( which( ge[ixe] == 0 & flag[ixe] == 0))
  rand <- (a+c) * (a+b) / (a+b+c+d)
  ets  <- (a-rand) / (a+b+c-rand)
  acc  <- (a+d)/(a+b+c+d)
  pod  <- a/(a+c)
  pofa <- b/(b+d)

  print( paste("---- thr kth inner_radius outer_radius num_max_outer dz= ", round(thr,4), kth, round(inner_radius), round(outer_radius), num_max_outer, round(vertical_scale)))
  print( paste0("TOT bad / a b c d= ", length(ixe), " ", length(which( ge==1)), " / ", a, " ", b, " ", c, " ", d))
  print( paste0("acc pod pofa ets= ", round(acc,2), " ", round(pod,2), " ", round(pofa,2), " ",round(ets,2)))

  if (argv$debug) {

    if (argv$variable == "T") {
      ffout <- paste0( "dqc_prof_",format( tseq[t],format="%Y%m%d",tz="UTC"),".png")
      png(file=ffout,width=800,height=800)
      plot( obsToCheck_val, obsToCheck_z, pch=21, bg="gray", col="gray", cex=1)
      ix <- which(ge==1)
      points( obsToCheck_val[ix], obsToCheck_z[ix], pch=21, bg="red", col="pink", cex=2)
      ix <- which(flag == 1)
      points( obsToCheck_val[ix], obsToCheck_z[ix], pch=4, col="black", cex=2)
      ix <- which(flag == 11 | flag == 12)
      points( obsToCheck_val[ix], obsToCheck_z[ix], col = "blue", cex=2, lwd=3)
      ix <- which(flag < 0)
      points( obsToCheck_val[ix], obsToCheck_z[ix], col = "cyan", cex=2, lwd=3)
      dev.off()
    } else if (argv$variable == "RR") {
      ffout <- paste0( "dqc_rr_",format( tseq[t],format="%Y%m%d",tz="UTC"),".png")
      png(file=ffout,width=800,height=800)
      plot( obsToCheck_lat, obsToCheck_oval, pch=21, bg="gray", col="gray", cex=1)
      ix <- which(ge==1)
      points( obsToCheck_lat[ix], obsToCheck_oval[ix], pch=21, bg="red", col="pink", cex=2)
      ix <- which(flag == 1)
      points( obsToCheck_lat[ix], obsToCheck_oval[ix], pch=4, col="black", cex=2)
      ix <- which(flag == 11 | flag == 12)
      points( obsToCheck_lat[ix], obsToCheck_oval[ix], col = "blue", cex=2, lwd=3)
      ix <- which(flag < 0)
      points( obsToCheck_lat[ix], obsToCheck_oval[ix], col = "cyan", cex=2, lwd=3)
      dev.off()
    }

    ffout <- paste0( "dqc_map_",format( tseq[t],format="%Y%m%d",tz="UTC"),".png")
    png(file=ffout,width=800,height=800)
    plot( obsToCheck_lon, obsToCheck_lat, pch=21, bg="gray", col="gray", cex=1)
    ix <- which(ge==1)
    points( obsToCheck_lon[ix], obsToCheck_lat[ix], pch=21, bg="red", col="pink", cex=2)
    ix <- which(flag == 1)
    points( obsToCheck_lon[ix], obsToCheck_lat[ix], pch=4, col="black", cex=2)
    ix <- which(flag == 11 | flag == 12)
    points( obsToCheck_lon[ix], obsToCheck_lat[ix], col = "blue", cex=2, lwd=3)
    ix <- which(flag < 0)
    points( obsToCheck_lon[ix], obsToCheck_lat[ix], col = "cyan", cex=2, lwd=3)
    dev.off()
  }

  cat( file=argv$ffout, append=T,
       paste0( format( tseq[t], format=argv$ffout_date.format, tz="GMT"),";", 
               p,";",
               round(thr,3),";",
               kth,";",
               round(inner_radius),";",
               round(outer_radius),";",
               num_max_outer,";",
               round(vertical_scale),";", 
               a,";", 
               b,";", 
               c,";", 
               d,";", 
               round(acc,4),";", 
               round(pod,4),";", 
               round(pofa,4),";", 
               round(ets,4),";\n")) 

q()
}

#------------------------------------------------------------------------------
# Normal exit

rip( str="Normal Exit", code=0, t0=t0)
