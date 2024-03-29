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
     "time;p;num_min_outer;num_max_outer;inner_radius;outer_radius;kth;vertical_scale;thr;a_delta;v_delta;a_fact;v_fact;boxcox_par;a;b;c;d;acc;pod;pofa;ets;\n")

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

  theta <- c( argv$num_min_outer, argv$num_max_outer, argv$inner_radius, 
               argv$outer_radius, argv$kth, argv$vertical_scale, argv$thr,
               argv$a_delta, argv$v_delta, argv$a_fact, argv$v_fact,
               argv$boxcox.lambda)

  obsToCheck_mina <- pmin( pmax( obsToCheck_val - argv$a_delta,                 argv$plau_min), 
                           pmax( obsToCheck_val - argv$a_fact * obsToCheck_val, argv$plau_min))
  obsToCheck_maxa <- pmax( pmin( obsToCheck_val + argv$a_delta,                 argv$plau_max),
                           pmin( obsToCheck_val + argv$a_fact * obsToCheck_val, argv$plau_max))
  obsToCheck_minv <- pmin( pmax( obsToCheck_val - argv$v_delta,                 argv$plau_min), 
                           pmax( obsToCheck_val - argv$v_fact * obsToCheck_val, argv$plau_min))
  obsToCheck_maxv <- pmax( pmin( obsToCheck_val + argv$v_delta,                 argv$plau_max),
                           pmin( obsToCheck_val + argv$v_fact * obsToCheck_val, argv$plau_max))

  if ( !is.na( argv$PGE) & argv$PGE > 0) {

    if ( any( is.na( argv$PGE_prid))) argv$PGE_prid <- unique(obsToCheck_prid)

    res <- insert_ge( obsToCheck_val, obsToCheck_prid,
                      argv$ge0_min, argv$ge0_max,
                      obsToCheck_mina, obsToCheck_maxa, 
                      obsToCheck_minv, obsToCheck_maxv,
                      argv$PGE, argv$PGE_prid, argv$ge_strategy)

    ge             <- res$ge
    obsToCheck_val <- res$val 

  }

  obsToCheck_mina <- pmin( pmax( obsToCheck_val - argv$a_delta,                 argv$plau_min), 
                           pmax( obsToCheck_val - argv$a_fact * obsToCheck_val, argv$plau_min))
  obsToCheck_maxa <- pmax( pmin( obsToCheck_val + argv$a_delta,                 argv$plau_max),
                           pmin( obsToCheck_val + argv$a_fact * obsToCheck_val, argv$plau_max))
  obsToCheck_minv <- pmin( pmax( obsToCheck_val - argv$v_delta,                 argv$plau_min), 
                           pmax( obsToCheck_val - argv$v_fact * obsToCheck_val, argv$plau_min))
  obsToCheck_maxv <- pmax( pmin( obsToCheck_val + argv$v_delta,                 argv$plau_max),
                           pmin( obsToCheck_val + argv$v_fact * obsToCheck_val, argv$plau_max))

  obsToCheck_oval <- obsToCheck_val

  if ( argv$transformation) {

    obsToCheck_mina <- boxcox( x=obsToCheck_mina, lambda=argv$boxcox.lambda)
    obsToCheck_maxa <- boxcox( x=obsToCheck_maxa, lambda=argv$boxcox.lambda)
    obsToCheck_minv <- boxcox( x=obsToCheck_minv, lambda=argv$boxcox.lambda)
    obsToCheck_maxv <- boxcox( x=obsToCheck_maxv, lambda=argv$boxcox.lambda)
    obsToCheck_val  <- boxcox( x=obsToCheck_val,  lambda=argv$boxcox.lambda)

  }

  ixe <- 1:p
  if ( !is.na( argv$PGE) & argv$PGE > 0) {
    if ( !any( is.na( argv$PGE_prid))) 
      ixe <- which( obsToCheck_prid %in% argv$PGE_prid) 
  }

  #............................................................................
  # Optimization 

  # OPTimization if argv$theta_i is not defined
  if ( !any( is.na( argv$theta_i))) {

    maxl <- 3
    theta_l <- array( data=NA, dim=c(maxl,12))
    theta_l[1,] <- theta

    print( paste( ">>>>TIME  t=", format( tseq[t], format=argv$ffin_date.format,tz="GMT")))

    for (l in 2:maxl) {

      print( paste( "++++ ITERATION l=", l))

      for (j in 1:length(argv$theta_i))  {

        i <- argv$theta_i[j]
        print( paste( "<<<< PARAMETER i (j/tot)=", i, "(",j,"/",length(argv$theta_i),")"))
        min <- argv$theta_i_min[j]
        max <- argv$theta_i_max[j]
        if (i == 1) {         # num_min_outer
          max <- min( c( argv$theta_i_max[j], (theta[2]-1)), na.rm=T)
        } else if (i == 2)  { # num_max_outer
          min <- max( c( argv$theta_i_min[j], (theta[1]+1)), na.rm=T)
        } else if (i == 3)  { # inner_radius
          max <- min( c( argv$theta_i_max[j], (theta[4]-1)), na.rm=T)
        } else if (i == 4)  { # outer_radius
          min <- max( c( argv$theta_i_min[j], (theta[3]+1)), na.rm=T)
        }
        print( paste( " PARAMETER value (before) =", theta[i]))
        print( paste( " min/max =", min, max))
       
        paropt <- optimize( costf_sctres_justone, lower=min, upper=max, tol = 0.01,  maximum=T, theta=c(theta,i))

        theta[i] <- as.numeric(paropt[1])
        
        print( paste( "---- PARAMETER value (after)=", theta[i]))
  
      }

      theta_l[l,] <- theta
      
#      theta_devperc <- abs(theta_l[l,] - theta_l[l-1,]) / abs(theta_l[l,])
#      theta_devperc[which(abs(theta_l[l,])==0)] <- 0.00001
#      if ( all( theta_devperc < 0.01)) break

    }

  }

  #............................................................................
  # Test the parameters 

  flag <- rep( NA, p)

  for (i in 1:10) {

    ix <- which( flag %in% c(NA,0,11,12))

    obsToCheck_lat_i  <- obsToCheck_lat[ix]
    obsToCheck_lon_i  <- obsToCheck_lon[ix]
    obsToCheck_oval_i  <- obsToCheck_oval[ix]
    obsToCheck_chk_i  <- obsToCheck_chk[ix]
    obsToCheck_z_i    <- obsToCheck_z[ix]

    obsToCheck_val_i  <- obsToCheck_oval_i
    obsToCheck_mina_i <- pmin( pmax( obsToCheck_oval_i - theta[8],                      argv$plau_min), 
                               pmax( obsToCheck_oval_i - theta[10] * obsToCheck_oval_i, argv$plau_min))
    obsToCheck_maxa_i <- pmax( pmin( obsToCheck_oval_i + theta[8],                      argv$plau_max),
                               pmin( obsToCheck_oval_i + theta[10] * obsToCheck_oval_i, argv$plau_max))
    obsToCheck_minv_i <- pmin( pmax( obsToCheck_oval_i - theta[9],                      argv$plau_min), 
                               pmax( obsToCheck_oval_i - theta[11] * obsToCheck_oval_i, argv$plau_min))
    obsToCheck_maxv_i <- pmax( pmin( obsToCheck_oval_i + theta[9],                      argv$plau_max),
                               pmin( obsToCheck_oval_i + theta[11] * obsToCheck_oval_i, argv$plau_max))

    if ( argv$transformation) {

      obsToCheck_mina_i <- boxcox( x=obsToCheck_mina_i, lambda=theta[12])
      obsToCheck_maxa_i <- boxcox( x=obsToCheck_maxa_i, lambda=theta[12])
      obsToCheck_minv_i <- boxcox( x=obsToCheck_minv_i, lambda=theta[12])
      obsToCheck_maxv_i <- boxcox( x=obsToCheck_maxv_i, lambda=theta[12])
      obsToCheck_val_i  <- boxcox( x=obsToCheck_val_i,  lambda=theta[12])

    }

    p_i <- length(ix)

    res <- sct_resistant(
              points = Points( obsToCheck_lat_i,
                               obsToCheck_lon_i,
                               obsToCheck_z_i),
              obsToCheck_val_i,
              obsToCheck_chk_i,
              rep( background_values, p_i),
              argv$background_elab_type,
              as.integer(theta[1]), # num_min_outer,
              as.integer(theta[2]), # num_max_outer
              theta[3], # inner_radius
              theta[4], # outer_radius
              100,
              argv$num_min_prof, 
              argv$min_elev_diff,
              argv$min_horizontal_scale,
              argv$max_horizontal_scale,
              as.integer(theta[5]), # kth
              theta[6], # vertical_scale
              obsToCheck_mina_i, 
              obsToCheck_maxa_i,
              obsToCheck_minv_i,
              obsToCheck_maxv_i,
              rep( argv$eps2, p_i),
              rep( theta[7], p_i), # thr
              rep( theta[7], p_i), # thr
              F)

    flag[ix] <- res[[1]]

    nge_i <- length( which( res[[1]] == 1))
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

  print( "---- 1 num_min_outer, 2 num_max_outer, 3 inner_radius, 4 outer_radius, 5 kth, 6 vertical_scale, 7 thr, 8 a_delta, 9 v_delta, 10 a_fact, 11 v_fact, 12 boxcox_par")
  print(theta)
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
               round(theta[1]),";",
               round(theta[2]),";",
               round(theta[3]),";",
               round(theta[4]),";",
               round(theta[5]),";",
               round(theta[6]),";",
               round(theta[7],3),";",
               round(theta[8],1),";",
               round(theta[9],1),";",
               round(theta[10],2),";",
               round(theta[11],2),";",
               round(theta[12],2),";",
               a,";", 
               b,";", 
               c,";", 
               d,";", 
               round(acc,4),";", 
               round(pod,4),";", 
               round(pofa,4),";", 
               round(ets,4),";\n")) 

}

#------------------------------------------------------------------------------
# Normal exit

rip( str="Normal Exit", code=0, t0=t0)
