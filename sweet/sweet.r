#!/usr/bin/env Rscript
# --~- sweet.r -~--
# SWEET - spatially consistent stochastic weather generator
# See the software repository here: https://github.com/cristianlussana/sweet
#..............................................................................
#Copyright and license
# Copyright (C) 2018 MET Norway. The software is licensed under GPL version 3 
# or (at your option) any later version.
# https://www.gnu.org/licenses/gpl-3.0.en.html
# 
# History:
# 06.11.2020 - Cristian Lussana. Original code.
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
ffout_default   <- "out.nc"

# -----------------------------------------------------------------------------
# FUNCTIONS

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
#-----------------------------------------------------------------------------
# Read command line arguments
argv <- argparser()
#-----------------------------------------------------------------------------
# Multi-cores run
if ( !is.na( argv$cores)) {
  suppressPackageStartupMessages( library( "parallel"))
  if ( argv$cores==0) argv$cores <- detectCores()
  print( paste( "--> multi-core run, cores=", argv$cores))
}
#-----------------------------------------------------------------------------
# Determine the points
if (argv$gridded) {
  library( raster)
  r <- raster( extent( argv$gridded_extent[1], argv$gridded_extent[2], 
                       argv$gridded_extent[3], argv$gridded_extent[4]),
               res = argv$gridded_res, 
               crs = argv$gridded_crs)
  xy <- xyFromCell( r ,1:ncell(r))
  x_points <- xy[,1]
  y_points <- xy[,2]
  z_points <- mvn_sim( x = x_points, 
                       y = y_points,
                       mean = 1000,
                       stdev= 200,
                       length_scale = 50000)
  n_points <- length( x_points)
  print("-- created raster --")
  print(r)
} else {
  # Read observational network
  obsnet <- read_obsNet( file=argv$ffin_obs, crs_in=proj4.wgs84, crs_out=proj4.lcc,
                         extent_out=extent( argv$extent[1], argv$extent[2], 
                                            argv$extent[3], argv$extent[4]))
  x_points <- obsnet$x
  y_points <- obsnet$y
  z_points <- obsnet$z
  prid_points <- obsnet$prid
  print("-- obsNet --")
  print(obsnet$n)
}
#-----------------------------------------------------------------------------
# Simulate the field
if ( argv$sweet_t) {
  isin<- x_points >= argv$extent_small[1] & x_points <= argv$extent_small[2] & 
         y_points >= argv$extent_small[3] & y_points <= argv$extent_small[4]
  zrange <- range( z_points[isin], na.rm=T)
  t_h0_mean <- max( c(0,(zrange[1]-30)))
  t_h1i_mean <- zrange[2]-zrange[1]+130
#  a <- runif( n=argv$nsamples, min=argv$t_a_min, max=argv$t_a_max)
  a <- seq( as.integer(argv$t_a_min), as.integer(argv$t_a_max), by=1)
  i <- 1
  open_par <- list( n=obsnet$n, x=obsnet$x, y=obsnet$y, z=obsnet$z,
                    lat=obsnet$lat, lon=obsnet$lon,
                    prid=obsnet$prid,
                    isin=isin, nsamples=argv$nsamples, t0=argv$t_t0_mean,
                    gamma=argv$t_gamma_mean, h0=t_h0_mean, h1i=t_h1i_mean,
                    a=a)
  while ( i <= argv$nsamples) {
    val <- sweet_t( x = x_points, y = y_points, z = z_points,
                    t0_par    = argv$t_t0_mean,
                    gamma_par = argv$t_gamma_mean,
                    a_par     = a[i],
                    h0_par    = t_h0_mean,
                    h1i_par   = t_h1i_mean,
                    islocal   = isin)
    if (!exists("conn")) conn<-NA
    conn <- write_sweetT( conn=conn, file=argv$ffout, res=val, ens=i, 
                          open=(i==1), close=(i==argv$nsamples), open_par=open_par)
    br <- seq(range(val)[1],range(val)[2],length=15)
    col <- rev(rainbow(14))
    png(file=paste0("map_",formatC(i,width=2,flag="0"),".png"),width=800,height=800)
    plot( extent(argv$extent))
    plot( extent(argv$extent_small),add=T)
    for ( j in 1:length(col)) {
      ix <- which( val>=br[j] & val<br[j+1])
      points( x_points[ix], y_points[ix], bg=col[j], pch=21)
    }
    dev.off()
    aux <- tvertprof_Frei( z  = z_points,
                           t0 = argv$t_t0_mean,
                           gamma = argv$t_gamma_mean,
                           a   = a[i],
                           h0  = t_h0_mean,
                           h1i = t_h1i_mean)
    png(file=paste0("zgraph_",formatC(i,width=2,flag="0"),".png"),width=800,height=800)
    plot( val, z_points,xlim=range(c(val,aux),na.rm=T), main=paste("a=",round(a[i])))
    points( aux, z_points, pch=21, bg="gold", cex=.5)
    points( val[isin], z_points[isin], pch=21, bg="maroon", cex=1.5)
    dev.off()
    i <- i + 1
  }
} else if ( argv$sweet_rr) {
  i <- 0
  while ( i < argv$nsamples) {
    val <- sweet_rr( x_points, y_points, 
                   argv$rr_lscale, argv$rr_gamma_shape, argv$rr_gamma_rate)
    r[] <- val
    frac_wet <- length( which( val > 1)) / length(val)
    if ( frac_wet < 0.1) next
    i <- i + 1
    if ( i == 1) { s <- r } else { s <- stack( s, r) }
  }
}
#-----------------------------------------------------------------------------
# Add GEs
if ( argv$add_ges_repr) {
  if (  argv$sweet_t) { 
    m <- 0; em <- "additive";   cft <- "gaussian"; mn <- NA; mx <- NA
  } else if ( argv$sweet_rr) { 
    m <- 1; em <- "multiplicative"; cft <- "soar"; mn <- 0; mx <- NA
  }
  val <- sweet_add_ges_repr( val = val, x = x_points, y = y_points,
                             err_model = em,
                             mean = m,
                             stdev = argv$ges_repr_stdev,
                             lscale = argv$ges_repr_lscale,
                             corr_fun_type = cft,
                             min = mn,
                             max = mx )
}
if ( argv$add_ges_unif) 
  val <- sweet_add_ges_unif( val, argv$ges_unif_frac, 
                             argv$ges_unif_min, argv$ges_unif_max)
#
#-----------------------------------------------------------------------------
# Output
if ( argv$gridded) {
  xy <- xyFromCell( s, 1:ncell(s))
  x  <- sort( unique( xy[,1]))
  y  <- sort( unique( xy[,2]), decreasing=T)
  r.list<-list()
  if (nlayers(s)>1) {
    grid <- array( data=NA, dim=c( length(x), length(y), nlayers(s), 1))
    for (i in 1:nlayers(s)) 
      grid[,,i,1] <- matrix( data=subset( s, subset=i), 
                             ncol=length(y), nrow=length(x))
  } else {
    grid     <- array(  data=NA, dim=c(length(x), length(y)))
    grid[,]  <- matrix( data=s,  ncol=length(y),  nrow=length(x))
  }
  r.list[[1]] <- grid
  rm( grid, s)
  out <- write_dotnc(grid.list = r.list,
                     times     = "197611220000",
                     file.name = argv$ffout,
                     grid.type = argv$ffout_gridtype,
                     x = x,
                     y = y,
                     var.name         = argv$ffout_varname,
                     var.longname     = argv$ffout_varlongname,
                     var.standardname = argv$ffout_varstandardname,
                     var.version      = argv$ffout_varversion,
                     times.unit       = "H",
                     reference        = argv$ffout_reference,
                     proj4.string     = argv$gridded_crs,
                     var.unit         = argv$ffout_varunit,
                     lonlat.out       = argv$ffout_lonlat,
                     round.dig        = argv$ffout_diground,
                     summary          = argv$ffout_summary,
                     source.string    = argv$ffout_sourcestring,
                     title            = argv$ffout_title,
                     comment          = argv$ffout_comment,
                     cf_1.7           = T)
  t1 <- Sys.time()
  print( paste( "writing output file", argv$ffout,
                " / time", round(t1-t0,1), attr(t1-t0,"unit")))
}
#------------------------------------------------------------------------------
# Normal exit
rip( str="Normal Exit", code=0, t0=t0)
