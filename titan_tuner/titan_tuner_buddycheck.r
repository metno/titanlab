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

# prepare output file
cat( file=argv$ffout, append=F,
     "time;p;radius;num_min;thr;max_elev_diff;elev_gradient;min_std;transformation;boxcox_par;a;b;c;d;acc;pod;pofa;ets;\n")

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

  #
  #............................................................................
  # statistics on distances

  xy <- spTransform( SpatialPoints( cbind( obsToCheck_lon, obsToCheck_lat), proj4string=CRS(proj4.wgs84)), CRS(proj4.lcc))
  obsToCheck_x <- attr( xy, "coords")[,1]
  obsToCheck_y <- attr( xy, "coords")[,2]
  k <- 20
  nn2 <- nn2( cbind( obsToCheck_x, obsToCheck_y), 
              query = cbind( obsToCheck_x, obsToCheck_y), 
              k = k) 
#              searchtype = "radius", radius = 500000)

  cat( paste0( "number of observations =", p, "\n"))
  cat( "average distances between a station and its nearest k-th station (k=1,2,..)\n")
  for (i in 1:k) cat( paste0( round( median(nn2$nn.dists[,i])), " "))
  cat("\n")

  if (argv$figs) {
    png( file="domain.png", width=800, height=800)
    plot( obsToCheck_lon, obsToCheck_lat)
    dev.off()
  }
  #............................................................................
  # prepare vectors of valid and admissible values

  theta <- c( argv$radius, argv$num_min, argv$thr, argv$max_elev_diff, 
              argv$elev_gradient, argv$min_std) 

  if ( !is.na( argv$PGE) & argv$PGE > 0) {

    if ( any( is.na( argv$PGE_prid))) argv$PGE_prid <- unique(obsToCheck_prid)

    res <- insert_ge( obsToCheck_val, obsToCheck_prid,
                      argv$ge0_min, argv$ge0_max,
                      NA, NA, 
                      NA, NA,
                      argv$PGE, argv$PGE_prid, argv$ge_strategy)

    ge             <- res$ge
    obsToCheck_val <- res$val 

  }

  obsToCheck_oval <- obsToCheck_val

  if ( argv$transformation) 
    obsToCheck_val  <- boxcox( x=obsToCheck_val,  lambda=argv$boxcox.lambda)

  ixe <- 1:p
  if ( !is.na( argv$PGE) & argv$PGE > 0) {
    if ( !any( is.na( argv$PGE_prid))) 
      ixe <- which( obsToCheck_prid %in% argv$PGE_prid) 
  }

  #............................................................................
  # Optimization 

  # OPTimization if argv$theta_i is not defined
#  if ( !any( is.na( argv$theta_i))) {
#
#    maxl <- 100
#    theta_l <- array( data=NA, dim=c(maxl,12))
#    theta_l[1,] <- theta
#
#    for (l in 2:100) {
#
#      for (j in 1:length(argv$theta_i))  {
#
#        i <- argv$theta_i[j]
#        min <- max( c( argv$theta_i_min[j], theta[i]/2), na.rm=T)
#        max <- min( c( argv$theta_i_max[j], theta[i]*2), na.rm=T)
#       
#        paropt <- optimize( costf_sctres_justone, lower=min, upper=max, tol = 0.001,  maximum=T, theta=c(theta,i))
#
#        theta[i] <- as.numeric(paropt[1])
#  
#      }
#
#      theta_l[l,] <- theta
#      
#      theta_devperc <- abs(theta_l[l,] - theta_l[l-1,]) / abs(theta_l[l,])
#      theta_devperc[which(abs(theta_l[l,])==0)] <- 0.00001
#      if ( all( theta_devperc < 0.01)) break
#
#    }
#
#  }

  #............................................................................
  # Test the parameters 

  for (thr in argv$thr_vec) {
    flag <- rep( NA, p)
    for (i in 1:10) {

      ix <- which( flag %in% c(NA,0,11,12))

      obsToCheck_lat_i  <- obsToCheck_lat[ix]
      obsToCheck_lon_i  <- obsToCheck_lon[ix]
      obsToCheck_oval_i <- obsToCheck_oval[ix]
      obsToCheck_chk_i  <- obsToCheck_chk[ix]
      obsToCheck_z_i    <- obsToCheck_z[ix]

      obsToCheck_val_i  <- obsToCheck_oval_i

      if ( argv$transformation) 
        obsToCheck_val_i <- boxcox( x=obsToCheck_val_i,  lambda=argv$boxcox.lambda)

      p_i <- length(ix)

      res <- buddy_check(
                points = Points( obsToCheck_lat_i,
                                 obsToCheck_lon_i,
                                 obsToCheck_z_i),
                obsToCheck_val_i,
                as.numeric(argv$radius), # radius 
                as.integer(argv$num_min), # num_min 
#                as.numeric(argv$thr), # threshold
                as.numeric(thr), # threshold
                as.numeric(argv$max_elev_diff), # max_elev_diff
                as.numeric(argv$elev_gradient), # elev_gradient
                as.numeric(argv$min_std), # min_std
                100,
                rep(1,p_i))

      flag[ix] <- res
  
      nge_i <- length( which( res == 1))
      print( paste("i nge_i",i,nge_i))

      if (nge_i == 0) break

    }

    a <- length( which( ge[ixe] == 1 & flag[ixe] == 1))
    c <- length( which( ge[ixe] == 1 & flag[ixe] == 0))
    b <- length( which( ge[ixe] == 0 & flag[ixe] == 1))
    d <- length( which( ge[ixe] == 0 & flag[ixe] == 0))
    rand <- (a+c) * (a+b) / (a+b+c+d)
    ets  <- (a-rand) / (a+b+c-rand)
    if ((a+b+c-rand)==0) ets <- 0
    acc  <- (a+d)/(a+b+c+d)
    pod  <- a/(a+c)
    pofa <- b/(b+d)

    print( "---- 1 radius, 2 num_min, 3 thr, 4 max_elev_diff, 5 elev_gradient, 6 min_std")
    print( paste( round(argv$radius,1), round(argv$num_min), round(thr,1),
           round(argv$max_elev_diff), round(argv$elev_gradient),
           round(argv$min_std,2)))
    print( paste0("TOT bad / a b c d= ", length(ixe), " ", length(which( ge==1)), " / ", a, " ", b, " ", c, " ", d))
    print( paste0("acc pod pofa ets= ", round(acc,2), " ", round(pod,2), " ", round(pofa,2), " ",round(ets,2)))

    if (argv$figs) {

      ffout <- paste0( argv$ffout_png1,"_",format( tseq[t],format=argv$ffout_date.format,tz="UTC"),".png")
      png(file=ffout,width=800,height=800)
      plot( obsToCheck_oval, obsToCheck_z, pch=21, bg="gray", col="gray", cex=1)
#      points( obsToCheck_oval[ixe], obsToCheck_z[ixe], pch=21, bg="gray", col="green", cex=2, lwd=3)
      ix <- which(ge==1)
      points( obsToCheck_oval[ix], obsToCheck_z[ix], pch=21, bg="red", col="red", cex=2, lwd=3)
      ix <- which(flag == 1)
      points( obsToCheck_oval[ix], obsToCheck_z[ix], pch=4, col="black", cex=2, lwd=3)
      ix <- which(flag == 11 | flag == 12)
      points( obsToCheck_oval[ix], obsToCheck_z[ix], col = "blue", cex=2, lwd=3)
      ix <- which(flag < 0)
      points( obsToCheck_oval[ix], obsToCheck_z[ix], col = "cyan", cex=2, lwd=3)
#      ix <- which(flag[ixe] == 0)
#      points( obsToCheck_oval[ixe[ix]], obsToCheck_z[ixe[ix]], col = "green", cex=2, lwd=3)
      dev.off()

      ffout <- paste0( argv$ffout_png2,"_",format( tseq[t],format=argv$ffout_date.format,tz="UTC"),".png")
      png(file=ffout,width=800,height=800)
      plot( obsToCheck_lat, obsToCheck_oval, pch=21, bg="gray", col="gray", cex=1)
#      points( obsToCheck_lat[ixe], obsToCheck_oval[ixe], pch=21, bg="gray", col="green", cex=2, lwd=3)
      ix <- which(ge==1)
      points( obsToCheck_lat[ix], obsToCheck_oval[ix], pch=21, bg="red", col="red", cex=2, lwd=3)
      ix <- which(flag == 1)
      points( obsToCheck_lat[ix], obsToCheck_oval[ix], pch=4, col="black", cex=2, lwd=3)
      ix <- which(flag == 11 | flag == 12)
      points( obsToCheck_lat[ix], obsToCheck_oval[ix], col = "blue", cex=2, lwd=3)
      ix <- which(flag < 0)
      points( obsToCheck_lat[ix], obsToCheck_oval[ix], col = "cyan", cex=2, lwd=3)
#      ix <- which(flag[ixe] == 0)
#      points( obsToCheck_lat[ixe[ix]], obsToCheck_oval[ixe[ix]], col = "green", cex=2, lwd=3)
      dev.off()

      ffout <- paste0( argv$ffout_png3,"_",format( tseq[t],format=argv$ffout_date.format,tz="UTC"),".png")
      png(file=ffout,width=800,height=800)
      plot( obsToCheck_lon, obsToCheck_oval, pch=21, bg="gray", col="gray", cex=1)
#      points( obsToCheck_lon[ixe], obsToCheck_oval[ixe], pch=21, bg="gray", col="green", cex=2, lwd=3)
      ix <- which(ge==1)
      points( obsToCheck_lon[ix], obsToCheck_oval[ix], pch=21, bg="red", col="red", cex=2, lwd=3)
      ix <- which(flag == 1)
      points( obsToCheck_lon[ix], obsToCheck_oval[ix], pch=4, col="black", cex=2, lwd=3)
      ix <- which(flag == 11 | flag == 12)
      points( obsToCheck_lon[ix], obsToCheck_oval[ix], col = "blue", cex=2, lwd=3)
      ix <- which(flag < 0)
      points( obsToCheck_lon[ix], obsToCheck_oval[ix], col = "cyan", cex=2, lwd=3)
#      ix <- which(flag[ixe] == 0)
#      points( obsToCheck_lon[ixe[ix]], obsToCheck_oval[ixe[ix]], col = "green", cex=2, lwd=3)
      dev.off()
  
      ffout <- paste0( argv$ffout_png4,"_",format( tseq[t],format=argv$ffout_date.format,tz="UTC"),".png")
      png(file=ffout,width=800,height=800)
      plot( obsToCheck_lon, obsToCheck_lat, pch=21, bg="gray", col="gray", cex=1)
#    points( obsToCheck_lon[ixe], obsToCheck_lat[ixe], pch=21, bg="gray", col="green", cex=2, lwd=3)
      ix <- which(ge==1)
      points( obsToCheck_lon[ix], obsToCheck_lat[ix], pch=21, bg="red", col="red", cex=2, lwd=3)
      ix <- which(flag == 1)
      points( obsToCheck_lon[ix], obsToCheck_lat[ix], pch=4, col="black", cex=2, lwd=3)
      ix <- which(flag == 11 | flag == 12)
      points( obsToCheck_lon[ix], obsToCheck_lat[ix], col = "blue", cex=2, lwd=3)
      ix <- which(flag < 0)
      points( obsToCheck_lon[ix], obsToCheck_lat[ix], col = "cyan", cex=2, lwd=3)
#    ix <- which(flag[ixe] == 0)
#    points( obsToCheck_lon[ixe[ix]], obsToCheck_lat[ixe[ix]], col = "green", cex=2, lwd=3)
      dev.off()

    }

    cat( file=argv$ffout, append=T,
         paste0( format( tseq[t], format=argv$ffout_date.format, tz="GMT"),";", 
                 p,";",
                 round(argv$radius,1),";",
                 round(argv$num_min),";",
#                 round(argv$thr,1),";",
                 round(thr,1),";",
                 round(argv$max_elev_diff),";",
                 round(argv$elev_gradient),";",
                 round(argv$min_std,2),";",
                 as.integer(argv$transformation),";",
                 round(argv$boxcox.lambda,2),";",
                 a,";", 
                 b,";", 
                 c,";", 
                 d,";", 
                 round(acc,4),";", 
                 round(pod,4),";", 
                 round(pofa,4),";", 
                 round(ets,4),";\n")) 

  }
}
#------------------------------------------------------------------------------
# Normal exit

rip( str="Normal Exit", code=0, t0=t0)
