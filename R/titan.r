#!/usr/bin/env Rscript
# + TITAN - spatial data quality control for in-situ observations
# mailto: cristianl@met.no
# https://github.com/metno/TITAN
#-----------------------------------------------------------------------------
#  This file is free software: you may copy, redistribute and/or modify it  
#  under the terms of the GNU General Public License as published by the  
#  Free Software Foundation, either version 2 of the License, or (at your  
#  option) any later version.  
#  
#  This file is distributed in the hope that it will be useful, but  
#  WITHOUT ANY WARRANTY; without even the implied warranty of  
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
#  General Public License for more details.  
#  
#  You should have received a copy of the GNU General Public License  
#  along with this program.  If not, see <http://www.gnu.org/licenses/>. 
#-----------------------------------------------------------------------------
suppressPackageStartupMessages(library("argparser"))
suppressPackageStartupMessages(library("sp"))
suppressPackageStartupMessages(library("raster"))
suppressPackageStartupMessages(library("rgdal"))
options(warn = 2, scipen = 999)
#------------------------------------------------------------------------------
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
    cat( paste( "total time=", round(t1-t0,1), attr(t1-t0,"unit")))
  }
  if ( !is.na(str)) cat( str)
  cat("\n")
  quit( status= 0 )
}


#==============================================================================
#  MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN
#==============================================================================
t0 <- Sys.time() # game on
#
#-----------------------------------------------------------------------------
# path to the titan functions is stored in the enviroment var TITANR_FUN
titan_fun_path <- Sys.getenv( "TITANR_PATH")
#titan_fun_path <-"~/projects/titanlab/titanlab/R/functions"
## path to the titan main modules is stored in the enviroment var TITANR_MOD
#titan_mod_path <- Sys.getenv( "TITANR_MOD")
#
#-----------------------------------------------------------------------------
# load functions
fun_list <- c( "argparser.r",
               "read_data_to_check.r",
               "metadata_check.r",
               "spatconv.r",
               "read_dem.r",
               "read_laf.r",
               "ccrrt.r",
               "rr_windcorr.r",
               "read_fg.r",
               "read_fge.r",
               "check_z_against_dem.r",
               "plausibility_test.r",
               "climatological_check.r",
               "buddy_eve.r",
               "buddy.r",
               "fg_det.r",
               "oi_var_gridpoint_by_gridpoint.r",
               "netcdf_util.r",  
               "statistics_util.r",
               "wolff_correction.r",
               "interpolation_util.r",
               "misc_util.r",
               "sct_util.r",
               "debug_util.r")
for (fun in fun_list) {
  if ( !file.exists(file.path( titan_fun_path, fun)))
    boom( file.path( titan_fun_path, fun), code=1)
  source( file.path( titan_fun_path, fun))
}
rm( fun_list, fun)               
#
#-----------------------------------------------------------------------------
# check all main modules exists
#mod_list <- c( "main_read_dem.r", "main_read_laf.r"
#               "main_ccrrt.r", "main_rr_windcorr.r",
#               "main_read_fg.r", "main_read_fge.r",
#               "main_radar_in_output.r" ,
#               "main_output.r" )
#for (mod in mod_list) {
#  if ( !file.exists(file.path( titan_mod_path, mod)))
#    boom( file.path( titan_mod_path, mod), code=1)
#}
#rm( mod_list, mod)               
#
#-----------------------------------------------------------------------------
# define constants
#source( file.path( titan_mod_path, "main_constants.r"))
proj4_input_obsfiles_default    <- "+proj=longlat +datum=WGS84"
proj4_where_dqc_is_done_default <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
xy.dig.out_default <- 5
varname.y.out_default <- "lat"
varname.x.out_default <- "lon"
#
#-----------------------------------------------------------------------------
# read command line arguments and/or configuration file
argv <- argparser()
#
#-----------------------------------------------------------------------------
# Multi-cores run
if ( !is.na( argv$cores)) {
  suppressPackageStartupMessages( library( "parallel"))
  if ( argv$cores==0) argv$cores <- detectCores()
  cat( paste( "--> multi-core run, cores=", argv$cores, "\n"))
}
#
#-----------------------------------------------------------------------------
# read data
#data <- new.env( parent = emptyenv())
nfin  <- length( argv$input.files)
res <- read_data_to_check( argv)
extent  <- res$extent
data    <- res$data
dqcflag <- res$dqcflag
z       <- res$z
sctpog  <- res$sctpog
corep   <- res$corep
rm(res)
ndata <- length(data$lat)
#
#-----------------------------------------------------------------------------
# test for no metadata (1st round) 
#  1st round, check for missing metadata in the original data
#  2nd round, check for missing metadata after dem.fill 
dqcflag.bak <- dqcflag # bakup, used in main_read_dem.r (if dem.fill=T)
dqcflag <- metadata_check ( argv, data, z, extent, dqcflag)
#
#-----------------------------------------------------------------------------
# coordinate transformation
res <- spatconv( argv, data, extent)
x  <- res$x
y  <- res$y
xl <- res$xl
yl <- res$yl
e  <- res$e
rm(res)
#
#-----------------------------------------------------------------------------
# Read geographical information (optional) 
# digital elevation model
if (argv$dem | argv$dem.fill) { 
  res <- read_dem( argv, data, z, dqcflag)
  z <- res$z
  zdem <- res$zdem
  dqcflag <- res$dqcflag
  rm(res)
  rm(dqcflag.bak)
}
# land area fraction (%, e.g 10 and NOT 0.1)
if (argv$laf.sct) {
  laf <- read_laf( argv)
} else { # use a fake laf
  laf <- rep( 1, ndata)
}
#
#-----------------------------------------------------------------------------
# precipitation (in-situ) and temperature (field) cross-check
if (argv$ccrrt) {
  res <- ccrrt( argv, data, z, dqcflag)
  dqcflag <- res$dqcflag
  if (argv$rr.wcor) { t2m <- res$t2m } else { t2m <- NULL }
  rm(res)
}
#
#-----------------------------------------------------------------------------
# Correction for the wind-undercatch of precipitation 
if (argv$rr.wcor) 
 data  <- rr_windcorr( argv, data, z, dqcflag, t2m) 
#
#-----------------------------------------------------------------------------
# Read deterministic first guess (optional)
if (!is.na(argv$fg.file)) {
  res <- read_fg( argv)
  fg <- res$fg
  rrad <- res$rrad
  cool_aux <- res$cool_aux
  rm(res)
}
#
#-----------------------------------------------------------------------------
# Read first guess ensemble (optional)
if (!is.na(argv$fge.file)) {
  res <- read_fge( argv)
  fg.mu <- res$fg.mu
  fg.sd <- res$fg.sd
  rm(res)
}
#
#-----------------------------------------------------------------------------
# test for no metadata (2nd and final) 
dqcflag <- metadata_check ( argv, data, z, extent, dqcflag)
#
#-----------------------------------------------------------------------------
# check elevation against dem 
# NOTE: keep-listed stations canNOT be flagged here
if (argv$dem) 
  dqcflag <- check_z_against_dem( argv, data, z, zdem, nfin, dqcflag )
#
#-----------------------------------------------------------------------------
# plausibility test
# NOTE: keep-listed stations could be flagged here
dqcflag <- plausibility_test(argv,data,dqcflag)
#
#-----------------------------------------------------------------------------
# climatological check 
# NOTE: keep-listed stations canNOT be flagged here
# use only (probably) good observations
if (!is.na(argv$month.clim))
  dqcflag <- climatological_check(argv,data,dqcflag)
#
#-----------------------------------------------------------------------------
# buddy check (based on the definition of a binary yes/no event)
#  Define an event compare each observation against the average of neighbouring observations 
# NOTE: keep-listed stations are used but they canNOT be flagged here

if (argv$buddy_eve)
  dqcflag <- buddy_eve(argv,data,dqcflag)
#
#-----------------------------------------------------------------------------
# buddy check (standard)
#  compare each observation against the average of neighbouring observations 
# NOTE: keep-listed stations are used but they canNOT be flagged here
dqcflag <- buddy(argv,data,dqcflag)
#
#-----------------------------------------------------------------------------
# check against a first-guess (deterministic)
if (argv$fg) {
  dqcflag <- fg_det(argv,data,dqcflag)

#  if (!any(!is.na(fg))) {
#    if (argv$verbose | argv$debug) print("first guess is not defined (all NAs)")
#  } else {
#    if (argv$verbose | argv$debug) 
#      print(paste0("first-guess check det (",argv$fg.code,")"))
#    # set doit vector
#    doit<-vector(length=ndata,mode="numeric"); doit[]<-NA
#    thrvec<-vector(length=ndata,mode="numeric"); thrvec[]<-NA
#    thrposvec<-vector(length=ndata,mode="numeric"); thrposvec[]<-NA
#    thrnegvec<-vector(length=ndata,mode="numeric"); thrnegvec[]<-NA
#    thrpercvec<-vector(length=ndata,mode="numeric"); thrpercvec[]<-NA
#    thrpospercvec<-vector(length=ndata,mode="numeric"); thrpospercvec[]<-NA
#    thrnegpercvec<-vector(length=ndata,mode="numeric"); thrnegpercvec[]<-NA
#    fg_minval<-vector(length=ndata,mode="numeric"); fg_minval[]<-NA
#    fg_maxval<-vector(length=ndata,mode="numeric"); fg_maxval[]<-NA
#    obs_minval<-vector(length=ndata,mode="numeric"); obs_minval[]<-NA
#    obs_maxval<-vector(length=ndata,mode="numeric"); obs_maxval[]<-NA
#    fg_minval_perc<-vector(length=ndata,mode="numeric"); fg_minval_perc[]<-NA
#    fg_maxval_perc<-vector(length=ndata,mode="numeric"); fg_maxval_perc[]<-NA
#    obs_minval_perc<-vector(length=ndata,mode="numeric"); obs_minval_perc[]<-NA
#    obs_maxval_perc<-vector(length=ndata,mode="numeric"); obs_maxval_perc[]<-NA
#    fg_range<-range(fg,na.rm=T)
#    obs_range<-range(data$value,na.rm=T)
#    for (f in 1:nfin) {
#      if (!any(data$prid==argv$prid[f])) next
#      aux<-which(data$prid==argv$prid[f])
#      doit[aux]<-argv$doit.fg[f]
#      thrvec[aux]<-argv$thr.fg[f]
#      thrposvec[aux]<-argv$thrpos.fg[f]
#      thrnegvec[aux]<-argv$thrneg.fg[f]
#      thrpercvec[aux]<-argv$thrperc.fg[f]
#      thrpospercvec[aux]<-argv$thrposperc.fg[f]
#      thrnegpercvec[aux]<-argv$thrnegperc.fg[f]
#      fg_minval[aux]<-ifelse(is.na(argv$fg_minval.fg[f]),fg_range[1],
#                                                         argv$fg_minval.fg[f])
#      fg_maxval[aux]<-ifelse(is.na(argv$fg_maxval.fg[f]),fg_range[2],
#                                                         argv$fg_maxval.fg[f])
#      obs_minval[aux]<-ifelse(is.na(argv$obs_minval.fg[f]),obs_range[1],
#                                                           argv$obs_minval.fg[f])
#      obs_maxval[aux]<-ifelse(is.na(argv$obs_maxval.fg[f]),obs_range[2],
#                                                          argv$obs_maxval.fg[f])
#      fg_minval_perc[aux]<-ifelse(is.na(argv$fg_minval_perc.fg[f]),fg_range[1],
#                                                       argv$fg_minval_perc.fg[f])
#      fg_maxval_perc[aux]<-ifelse(is.na(argv$fg_maxval_perc.fg[f]),fg_range[2],
#                                                       argv$fg_maxval_perc.fg[f])
#      obs_minval_perc[aux]<-ifelse(is.na(argv$obs_minval_perc.fg[f]),obs_range[1],
#                                                      argv$obs_minval_perc.fg[f])
#      obs_maxval_perc[aux]<-ifelse(is.na(argv$obs_maxval_perc.fg[f]),obs_range[2],
#                                                       argv$obs_maxval_perc.fg[f])
#      rm(aux)
#    }
#    # use only (probably) good observations
#    ix<-which(is.na(dqcflag) & doit!=0)
#    if (length(ix)>0) {
#      dev<-data$value-fg
#      devperc<-dev/fg
#      flag_sus<-rep(F,ndata)
#      flag_to_check_basic<-is.na(dqcflag) & doit==1 &
#                     !is.na(data$value) & 
#                     !is.nan(data$value) & 
#                     is.finite(data$value) &
#                     !is.na(fg) & !is.nan(fg) & is.finite(fg)
#      flag_to_check<-flag_to_check_basic &
#                     data$value>=obs_minval & data$value<=obs_maxval &
#                     fg>=fg_minval & fg<=fg_maxval
#      # additive model
#      if (any(!is.na(thrvec))) 
#        flag_sus<-flag_sus | 
#         (!is.na(thrvec) & flag_to_check & abs(dev)>thrvec)
#      if (any(!is.na(thrposvec))) 
#        flag_sus<-flag_sus | 
#         (!is.na(thrposvec) & flag_to_check & dev>thrposvec)
#      if (any(!is.na(thrnegvec))) 
#        flag_sus<-flag_sus | 
#         (!is.na(thrnegvec) & flag_to_check & dev<0 & abs(dev)>thrnegvec)
#      # multiplicative model
#      flag_to_check<-flag_to_check_basic &
#                     data$value>=obs_minval_perc & data$value<=obs_maxval_perc &
#                     fg>=fg_minval_perc & fg<=fg_maxval_perc
#      if (any(!is.na(thrpercvec))) 
#        flag_sus<-flag_sus | 
#         (!is.na(thrpercvec) & flag_to_check & abs(devperc)>thrpercvec)
#      if (any(!is.na(thrpospercvec))) 
#        flag_sus<-flag_sus | 
#         (!is.na(thrpospercvec) & flag_to_check & 
#          dev>0 & abs(devperc)>thrpospercvec)
#      if (any(!is.na(thrnegpercvec))) 
#        flag_sus<-flag_sus | 
#         (!is.na(thrnegpercvec) & flag_to_check & 
#          dev<0 & abs(devperc)>thrnegpercvec)
#      ix_sus<-which(flag_sus)
#      rm(flag_sus,flag_to_check,flag_to_check_basic,dev,devperc)
#      rm(doit,thrvec,thrposvec,thrnegvec,thrpercvec)
#      rm(thrpospercvec,thrnegpercvec)
#      # set dqcflag
#      if (length(ix_sus)>0) dqcflag[ix_sus]<-argv$fg.code
#      rm(ix_sus)
#    }  else {
#      print("no valid observations left, no first-guess check")
#    }
#    if (argv$verbose | argv$debug) {
#      print(paste("# observations that fail the first-guess check (det)=",
#                  length(which(dqcflag==argv$fg.code))))
#      print("+---------------------------------+")
#    }
#    if (argv$debug) 
#      save.image(file.path(argv$debug.dir,"dqcres_fg.RData")) 
#    if (exists("doit")) rm(doit)
#    if (exists("thrvec")) rm(thrvec)
#    if (exists("thrposvec")) rm(thrposvec)
#    if (exists("thrnegvec")) rm(thrnegvec)  
#    if (exists("obs_minval_perc")) rm(obs_minval_perc)
#    if (exists("obs_maxval_perc")) rm(obs_maxval_perc)
#    if (exists("fg_minval_perc")) rm(fg_minval_perc)
#    if (exists("fg_maxval_perc")) rm(fg_maxval_perc)
#    if (exists("obs_minval")) rm(obs_minval)
#    if (exists("obs_maxval")) rm(obs_maxval)
#    if (exists("fg_minval")) rm(fg_minval)
#    if (exists("fg_maxval")) rm(fg_maxval)
#    if (exists("thrpercvec")) rm(thrpercvec)
#    if (exists("thrpospercvec")) rm(thrpospercvec)
#    if (exists("thrnegpercvec")) rm(thrnegpercvec)
#  }
}
#
q()
#-----------------------------------------------------------------------------
# check against a first-guess (ensemble)
if (argv$fge) {
  if (argv$verbose | argv$debug) {
    print(paste0("first-guess check ens (",argv$fge.code,")"))
  }
  # set doit vector
  doit<-vector(length=ndata,mode="numeric"); doit[]<-NA
  thrvec<-vector(length=ndata,mode="numeric"); thrvec[]<-NA
  thrposvec<-vector(length=ndata,mode="numeric"); thrposvec[]<-NA
  thrnegvec<-vector(length=ndata,mode="numeric"); thrnegvec[]<-NA
  thrpercvec<-vector(length=ndata,mode="numeric"); thrpercvec[]<-NA
  perc_minvalvec<-vector(length=ndata,mode="numeric"); perc_minvalvec[]<-NA
  thrpospercvec<-vector(length=ndata,mode="numeric"); thrpospercvec[]<-NA
  thrnegpercvec<-vector(length=ndata,mode="numeric"); thrnegpercvec[]<-NA
  throutvec<-vector(length=ndata,mode="numeric"); throutvec[]<-NA
  thrposoutvec<-vector(length=ndata,mode="numeric"); thrposoutvec[]<-NA
  thrnegoutvec<-vector(length=ndata,mode="numeric"); thrnegoutvec[]<-NA
  for (f in 1:nfin) {
    if (!any(data$prid==argv$prid[f])) next
    aux<-which(data$prid==argv$prid[f])
    doit[aux]<-argv$doit.fge[f]
    thrvec[aux]<-argv$thr.fge[f]
    thrposvec[aux]<-argv$thrpos.fge[f]
    thrnegvec[aux]<-argv$thrneg.fge[f]
    perc_minvalvec[aux]<-argv$perc.fge_minval[f]
    thrpercvec[aux]<-argv$thrperc.fge[f]
    thrpospercvec[aux]<-argv$thrposperc.fge[f]
    thrnegpercvec[aux]<-argv$thrnegperc.fge[f]
    throutvec[aux]<-argv$throut.fge[f]
    thrposoutvec[aux]<-argv$thrposout.fge[f]
    thrnegoutvec[aux]<-argv$thrnegout.fge[f]
    rm(aux)
  }
  # use only (probably) good observations
  ix<-which(is.na(dqcflag) & doit!=0)
  if (length(ix)>0) {
    dev<-data$value-fge.mu
    devperc<-dev/fge.mu
    devout<-dev/fge.sd
    flag_sus<-rep(F,ndata)
    flag_to_check<-is.na(dqcflag) & doit==1 &
                   !is.na(data$value) & 
                   !is.nan(data$value) & 
                   is.finite(data$value) &
                   !is.na(fge.mu) & !is.nan(fge.mu) & is.finite(fge.mu)
    if (any(!is.na(thrvec))) 
      flag_sus<-flag_sus | 
       (!is.na(thrvec) & flag_to_check & abs(dev)>thrvec)
    if (any(!is.na(thrposvec))) 
      flag_sus<-flag_sus | 
       (!is.na(thrposvec) & flag_to_check & dev>thrposvec)
    if (any(!is.na(thrnegvec))) 
      flag_sus<-flag_sus | 
       (!is.na(thrnegvec) & flag_to_check & dev<0 & abs(dev)>thrnegvec)
    flag_to_check<-flag_to_check & fge.mu>=perc_minvalvec
    if (any(!is.na(thrpercvec))) 
      flag_sus<-flag_sus | 
       (!is.na(thrpercvec) & flag_to_check & abs(devperc)>thrpercvec)
    if (any(!is.na(thrpospercvec))) 
      flag_sus<-flag_sus | 
       (!is.na(thrpospercvec) & flag_to_check & 
        dev>0 & abs(devperc)>thrpospercvec)
    if (any(!is.na(thrnegpercvec))) 
      flag_sus<-flag_sus | 
       (!is.na(thrnegpercvec) & flag_to_check & 
        dev<0 & abs(devperc)>thrnegpercvec)
    flag_to_check<-flag_to_check & 
                   !is.na(fge.sd) & !is.nan(fge.sd) & is.finite(fge.sd) 
    if (any(!is.na(throutvec))) 
      flag_sus<-flag_sus | 
       (!is.na(throutvec) & flag_to_check & abs(devout)>throutvec)
    if (any(!is.na(thrposoutvec))) 
      flag_sus<-flag_sus | 
       (!is.na(thrposoutvec) & flag_to_check & 
        dev>0 & abs(devout)>thrposoutvec)
    if (any(!is.na(thrnegoutvec))) 
      flag_sus<-flag_sus | 
       (!is.na(thrnegoutvec) & flag_to_check & 
        dev<0 & abs(devout)>thrnegoutvec)
    ix_sus<-which(flag_sus)
    rm(flag_sus,flag_to_check,dev,devperc,devout)
    rm(doit,thrvec,thrposvec,thrnegvec,perc_minvalvec,thrpercvec)
    rm(thrpospercvec,thrnegpercvec,throutvec,thrposoutvec,thrnegoutvec)
    # set dqcflag
    if (length(ix_sus)>0) dqcflag[ix_sus]<-argv$fge.code
    rm(ix_sus)
  }  else {
    print("no valid observations left, no first-guess check")
  }
  if (argv$verbose | argv$debug) {
    print(paste("# observations that fail the first-guess check (ens)=",
                length(which(dqcflag==argv$fge.code))))
    print("+---------------------------------+")
  }
  if (argv$debug) 
    save.image(file.path(argv$debug.dir,"dqcres_fge.RData")) 
}
#
#-----------------------------------------------------------------------------
# SCT - Spatial Consistency Test
# NOTE: keep-listed stations are used but they canNOT be flagged here
if (argv$verbose | argv$debug) 
  print(paste0("SCT (",argv$sct.code,")"))
nprev<-0
# set doit vector
doit<-vector(length=ndata,mode="numeric"); doit[]<-NA
eps2.sct<-vector(length=ndata,mode="numeric"); eps2.sct[]<-NA
T2vec<-vector(length=ndata,mode="numeric"); T2vec[]<-NA
T2posvec<-vector(length=ndata,mode="numeric"); T2posvec[]<-NA
T2negvec<-vector(length=ndata,mode="numeric"); T2negvec[]<-NA
for (f in 1:nfin) {
  if (!any(data$prid==argv$prid[f])) next
  aux<-which(data$prid==argv$prid[f])
  doit[aux]<-argv$doit.sct[f]
  eps2.sct[aux]<-argv$eps2.sct[f]
  T2vec[aux]<-argv$thr.sct[f]
  T2posvec[aux]<-argv$thrpos.sct[f]
  T2negvec[aux]<-argv$thrneg.sct[f]
  rm(aux)
}
# test
for (i in 1:argv$i.sct) {
  # use only (probably) good observations with doit!=0
  ix<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) & doit!=0 )
  if (length(ix)>0) {
    t0a<-Sys.time()
    #--------------------------------------------------------------------------
    # SCT station-by-station
    if (argv$stn_by_stn.sct) {
      if (i>1) break # the real SCT loop follows and it needs to be done just once
      sct_loop<-T
      cont_sct_loop<-0
      # obs_to_check[i] = should we check the i-th observation?
      obs_to_check<-rep(T,length(dqcflag))
      obs_to_check[!is.na(doit) & doit!=1]<-F
      # obs_to_use[i] = should we use the i-th observation for SCT?
      #  note that obs_to_use is not updated within the sct_loop
      obs_to_use<-rep(T,length(dqcflag))
      if (argv$transf.sct) {
        yo_sct<-boxcox(x=data$value,lambda=argv$boxcox.lambda)
      } else {
        yo_sct<-data$value
      }
      fg_min<-ifelse(argv$transf.sct,
                     boxcox(x=argv$vmin,lambda=argv$boxcox.lambda),
                     argv$vmin)
      fg_max<-ifelse(argv$transf.sct,
                     boxcox(x=argv$vmax,lambda=argv$boxcox.lambda),
                     argv$vmax)
      if (argv$usefge.sct) {
        if (argv$transf.sct) {
          b_sct<-boxcox(x=fge.mu,lambda=argv$boxcox.lambda)
        } else {
          b_sct<-fge.mu
        }
        argv$fglab.sct<-NA
        obs_to_use<-!is.na(b_sct) & !is.nan(b_sct) & is.finite(b_sct)
      } else if (argv$usefg.sct) {
        if (argv$transf.sct) {
          b_sct<-boxcox(x=fg,lambda=argv$boxcox.lambda)
        } else {
          b_sct<-fg
        }
        argv$fglab.sct<-NA
        obs_to_use<-!is.na(b_sct) & !is.nan(b_sct) & is.finite(b_sct)
      }
      while (sct_loop & cont_sct_loop<(length(ix)/2)) {
        cont_sct_loop<-cont_sct_loop+1
        t00a<-Sys.time()
        # ixg= index to observations to check for the current iteration
        ixg<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) & 
                    obs_to_check & obs_to_use )
        nobs_to_check<-length(ixg)
        xgrid_spint<-x[ixg]
        ygrid_spint<-y[ixg]
        zgrid_spint<-z[ixg]
        lafgrid_spint<-laf[ixg]
        yo_to_check<-yo_sct[ixg]
        # ixg= index to observations to use in SCT for the current iteration
        if (argv$usefge.sct | argv$usefg.sct) xb_spint<-b_sct[ixg]
        ixo<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) & doit!=0 &
                    obs_to_use )
        xobs_spint<-x[ixo]
        yobs_spint<-y[ixo]
        zobs_spint<-z[ixo]
        lafobs_spint<-laf[ixo]
        eps2_spint<-eps2.sct[ixo]
        nobs<-length(ixo)
        yo_spint<-yo_sct[ixo]
        if (argv$usefge.sct | argv$usefg.sct) yb_spint<-b_sct[ixo]
        # CV-analysis and variances at station points
        if (!is.na(argv$cores)) {
          arr<-t(mcmapply(oi_var_gridpoint_by_gridpoint,
                          1:nobs_to_check,
                          mc.cores=argv$cores,
                          SIMPLIFY=T,
                          dh=argv$DhorMin.sct,
                          box_o_nearest_halfwidth=argv$box_o_nearest_halfwidth.sct,
                          dz=argv$Dver.sct,
                          lafmin=argv$lafmin.sct,
                          dh_adaptive=T,
                          corr=argv$corr.sct,
                          pmax=argv$pmax.sct,
                          fg=argv$fglab.sct,
                          fg_gamma=argv$fg_gamma.sct,
                          fg_min=fg_min,
                          fg_max=fg_max,
                          succ_corr=argv$succ_corr.sct,
                          y_elab=F,
                          loocv=T,
                          o_errvar_min=argv$o_errvar_min.sct,
                          o_errvar_max=argv$o_errvar_max.sct,
                          xa_errvar_max=argv$xa_errvar_max.sct,
                          xa_errvar_min=argv$xa_errvar_min))
        # no-multicores
        } else {
          arr<-t(mapply(oi_var_gridpoint_by_gridpoint,
                        1:nobs_to_check,
                        SIMPLIFY=T,
                        dh=argv$DhorMin.sct,
                        box_o_nearest_halfwidth=argv$box_o_nearest_halfwidth.sct,
                        dz=argv$Dver.sct,
                        lafmin=argv$lafmin.sct,
                        dh_adaptive=T,
                        corr=argv$corr.sct,
                        pmax=argv$pmax.sct,
                        fg=argv$fglab.sct,
                        fg_gamma=argv$fg_gamma.sct,
                        fg_min=fg_min,
                        fg_max=fg_max,
                        succ_corr=argv$succ_corr.sct,
                        y_elab=F,
                        loocv=T,
                        o_errvar_min=argv$o_errvar_min.sct,
                        o_errvar_max=argv$o_errvar_max.sct,
                        xa_errvar_max=argv$xa_errvar_max.sct,
                        xa_errvar_min=argv$xa_errvar_min))
        }
        yav<-arr[,1]
        yav_errvar<-arr[,2]
        yo_errvar<-arr[,3]
        dh_ref<-arr[,7]
        rm(arr)
        # probability of gross error
        pog<-(yav-yo_to_check)**2/(yav_errvar+yo_errvar)
        sctpog[ixg]<-pog
        # ''presumption of innocence'' apply here
        flag_sus<-rep(F,nobs_to_check)
        # compare pog with defined thresholds
        if (any(!is.na(T2posvec[ixg]))) 
          flag_sus<-flag_sus | 
                    (!is.na(T2posvec[ixg]) & !is.na(pog) & 
                     pog>T2posvec[ixg] & (yo_to_check-yav)>0)
        if (any(!is.na(T2negvec[ixg]))) 
          flag_sus<-flag_sus | 
                    (!is.na(T2negvec[ixg]) & !is.na(pog) & 
                     pog>T2negvec[ixg] & (yo_to_check-yav)<0)
        if (any(!is.na(T2vec[ixg]))) 
          flag_sus<-flag_sus | 
                    (!is.na(T2vec[ixg]) & !is.na(pog) & 
                     pog>T2vec[ixg])
        ix_sus<-which(flag_sus)
        rm(flag_sus)
        t01a<-Sys.time()
        # case of no suspect observations
        if (length(ix_sus)==0) {
          print(paste(" stn-by-stn iteration=",cont_sct_loop,
                      "/#obs checked=",length(ixg),
                      "/no suspects",
                      "/time",round(t01a-t00a,1),attr(t01a-t00a,"unit")))
          sct_loop<-F
        # case of just one suspect observation
        } else if (length(ix_sus)==1) {
          print(paste(" stn-by-stn iteration=",cont_sct_loop,
                      "/#obs checked=",length(ixg),
                      "/#sus=1",
                      "/time",round(t01a-t00a,1),attr(t01a-t00a,"unit")))
          dqcflag[ixg[ix_sus]]<-argv$sct.code
          sct_loop<-F
        # case of more than one suspect observation
        } else {
          #+ is the suspect observation the one with the largest pog in the neighbourhood? 
          find_the_largeErr<-function(i,dist_max){
            dist<-sqrt((xgrid_largeErr[i]-xgrid_largeErr)**2+
                       (ygrid_largeErr[i]-ygrid_largeErr)**2)
            ix_near<-which(dist<dist_max)
            ifelse(any(pog_largeErr[ix_near]>pog_largeErr[i]),F,T)
          }
          #
          xgrid_largeErr<-xgrid_spint[ix_sus]
          ygrid_largeErr<-ygrid_spint[ix_sus]
          pog_largeErr<-pog[ix_sus]
          largeErr<-mapply(find_the_largeErr,
                           1:length(pog_largeErr),
                           SIMPLIFY=T,
                           dist_max=dh_ref[ix_sus])
          # largeErr[i]=F, for all i
          # largeErr is unable to find local maxima... should not happen
          if (!any(largeErr)) {
            if (argv$verbose) print("warning: SCT anomalous behaviour in largeErr")
            sct_loop<-F
          }
          # observations that are locally the more suspicious ones are flagged
          dqcflag[ixg[ix_sus[which(largeErr)]]]<-argv$sct.code
          nsct_sus<-length(which(largeErr))
          # 
          # largeErr[i]=T, for all i. 
          # All the suspicious observations have been flagged simultaneously
          if (!any(!largeErr)) sct_loop<-F 
          # prepare for the next sct loop
          if (sct_loop & argv$fast.sct) {
            # optimization:
            #  observations having small pog are not be checked again
            #  observations having large pog are flagged as suspect (even if not local max)
            #  observations checked twice against the same CV-analysis are not checked again
            if (any(!is.na(T2vec[ixg]))) { 
              ix_not_to_check<-which(pog<(T2vec[ixg]/4) & 
                                     !is.na(T2vec[ixg]))
              if (length(ix_not_to_check)>0) 
                obs_to_check[ixg[ix_not_to_check]]<-F
              ix_superBad<-which(pog>(4*T2vec[ixg]) & 
                                 !is.na(T2vec[ixg]))
              if (length(ix_superBad)>0) 
                dqcflag[ixg[ix_superBad]]<-argv$sct.code
              nsct_sus<-nsct_sus+length(ix_superBad)
              rm(ix_not_to_check,ix_superBad)
            }
            if (any(!is.na(T2posvec[ixg]))) {
              ix_not_to_check<-which(pog<(T2posvec[ixg]/4) & 
                                     !is.na(T2posvec[ixg]) & 
                                     (yo_to_check-yav)>0)
              if (length(ix_not_to_check)>0) 
                obs_to_check[ixg[ix_not_to_check]]<-F
              ix_superBad<-which(pog>(4*T2posvec[ixg]) & 
                                 !is.na(T2posvec[ixg]) &
                                 (yo_to_check-yav)>0)
              if (length(ix_superBad)>0) 
                dqcflag[ixg[ix_superBad]]<-argv$sct.code
              nsct_sus<-nsct_sus+length(ix_superBad)
              rm(ix_not_to_check,ix_superBad)
            } 
            if (any(!is.na(T2negvec[ixg]))) {
              ix_not_to_check<-which(pog<(T2negvec[ixg]/4) & 
                                     !is.na(T2negvec[ixg]) & 
                                     (yo_to_check-yav)<0)
              if (length(ix_not_to_check)>0) 
                obs_to_check[ixg[ix_not_to_check]]<-F
              ix_superBad<-which(pog>(4*T2negvec[ixg]) & 
                                 !is.na(T2negvec[ixg]) &
                                 (yo_to_check-yav)<0)
              if (length(ix_superBad)>0) 
                dqcflag[ixg[ix_superBad]]<-argv$sct.code
              nsct_sus<-nsct_sus+length(ix_superBad)
              rm(ix_not_to_check,ix_superBad)
            }
            if (!exists("yav_prev")) {
              yav_prev<-dqcflag; yav_prev[]<-NA
              yav_prev[ixg]<-yav 
            } else {
              if (any(yav_prev[ixg]==yav)) 
                obs_to_check[ixg[which(yav_prev[ixg]==yav)]]<-F
              yav_prev[ixg]<-yav 
            }
          } # prepare for the next sct loop 
          print(paste(" stn-by-stn iteration=",cont_sct_loop,
                      "/#obs checked=",length(ixg),
                      "/#possibly sus=",length(ix_sus),
                      "/#sus=",nsct_sus,
                      "/time",round(t01a-t00a,1),attr(t01a-t00a,"unit")))
        } # end case of suspect observations =0 or =1 or >1
      } # end sct_loop
      if (cont_sct_loop>(length(ix)/2)) {
        print("Warning: SCT loop stopped. Too many iterations. Better check this out.")
      } 
      # compute coefficients of representativeness (if needed)
      if (any(is.na(argv$const.corep))) {
        t00a<-Sys.time()
        ixg<-which(is.na(dqcflag))
        nobs_to_check<-length(ixg)
        xgrid_spint<-x[ixg]
        ygrid_spint<-y[ixg]
        zgrid_spint<-z[ixg]
        lafgrid_spint<-laf[ixg]
        yo_to_check<-yo_sct[ixg]
        if (argv$usefge.sct | argv$usefg.sct) xb_spint<-b_sct[ixg]
        ixo<-ixg
        xobs_spint<-x[ixo]
        yobs_spint<-y[ixo]
        zobs_spint<-z[ixo]
        lafobs_spint<-laf[ixo]
        eps2_spint<-eps2.sct[ixo]
        nobs<-length(ixo)
        yo_spint<-yo_sct[ixo]
        if (argv$usefge.sct | argv$usefg.sct) yb_spint<-b_sct[ixo]
          # CV-analysis and variances at station points
        if (!is.na(argv$cores)) {
          arr<-t(mcmapply(oi_var_gridpoint_by_gridpoint,
                          1:nobs_to_check,
                          mc.cores=argv$cores,
                          SIMPLIFY=T,
                          dh=argv$DhorMin.sct,
                          box_o_nearest_halfwidth=argv$box_o_nearest_halfwidth.sct,
                          dz=argv$Dver.sct,
                          lafmin=argv$lafmin.sct,
                          dh_adaptive=T,
                          corr=argv$corr.sct,
                          pmax=argv$pmax.sct,
                          fg=argv$fglab.sct,
                          fg_gamma=argv$fg_gamma.sct,
                          fg_min=fg_min,
                          fg_max=fg_max,
                          succ_corr=argv$succ_corr.sct,
                          y_elab=F,
                          loocv=T,
                          o_errvar_min=argv$o_errvar_min.sct,
                          o_errvar_max=argv$o_errvar_max.sct,
                          xa_errvar_max=argv$xa_errvar_max.sct,
                          xa_errvar_min=argv$xa_errvar_min))
        # no-multicores
        } else {
          arr<-t(mapply(oi_var_gridpoint_by_gridpoint,
                        1:nobs_to_check,
                        SIMPLIFY=T,
                        dh=argv$DhorMin.sct,
                        box_o_nearest_halfwidth=argv$box_o_nearest_halfwidth.sct,
                        dz=argv$Dver.sct,
                        lafmin=argv$lafmin.sct,
                        dh_adaptive=T,
                        corr=argv$corr.sct,
                        pmax=argv$pmax.sct,
                        fg=argv$fglab.sct,
                        fg_gamma=argv$fg_gamma.sct,
                        fg_min=fg_min,
                        fg_max=fg_max,
                        succ_corr=argv$succ_corr.sct,
                        y_elab=F,
                        loocv=T,
                        o_errvar_min=argv$o_errvar_min.sct,
                        o_errvar_max=argv$o_errvar_max.sct,
                        xa_errvar_max=argv$xa_errvar_max.sct,
                        xa_errvar_min=argv$xa_errvar_min))
        }
        yo_errvar<-arr[,3]
        rm(arr)
        corep[ixo]<-yo_errvar/mean(yo_errvar)
        t01a<-Sys.time()
        print(paste(" repres coeff=",cont_sct_loop,
                    "/#obs=",length(ixg),
                    "/time",round(t01a-t00a,1),attr(t01a-t00a,"unit")))
      } # end calculation of representativeness coefficients
      if (exists("arr")) rm(arr)
      if (exists("ixo")) rm(ixo,xobs_spint,yobs_spint,zobs_spint,lafobs_spint,
                            eps2_spint,nobs,yo_spint)
      if (exists("yb_spint")) rm(yb_spint)
      if (exists("ixg")) rm(ixg,nobs_to_check,xgrid_spint,ygrid_spint,
                           zgrid_spint,lafgrid_spint,yo_to_check)
      if (exists("xb_spint")) rm(xb_spint)
      if (exists("yo_sct")) rm(yo_sct)
      if (exists("b_sct")) rm(b_sct)
      if (exists("yav_prev")) rm(yav_prev)
      if (exists("yav")) rm(yav)
      if (exists("yav_errvar")) rm(yav_errvar)
      if (exists("yo_errvar")) rm(yo_errvar)
      if (exists("dh_ref")) rm(dh_ref)
      # 
    # END SCT stn-by-stn
    #--------------------------------------------------------------------------
    # SCT split grid into boxes
    } else {
      # set min and max for the background values
      sctvmin<-ifelse(argv$variable=="RR",-1./argv$boxcox.lambda,
                                          argv$vmin)
      sctvmax<-ifelse(argv$variable=="RR",boxcox(argv$vmax,argv$boxcox.lambda),
                                          argv$vmax)
      # create the grid for SCT. SCT is done independently in each box
      # NOTE: box size around 100Km should be ok
      if (i==1) {
        r<-raster(e,ncol=argv$grid.sct[2],nrow=argv$grid.sct[1])
        xy<-xyFromCell(r,1:ncell(r))
        xr<-xy[,1]
        yr<-xy[,2]
        ir<-1:ncell(r)
        r[]<-1:ncell(r)
      }
      # define global 1D vector used in sct (1D for fast access)
      xtot<-x[ix]
      ytot<-y[ix]
      ztot<-z[ix]
      if (argv$variable=="RR") {
        ttot<-boxcox(x=data$value[ix],lambda=argv$boxcox.lambda)
      } else {
        ttot<-data$value[ix]
      }
      pridtot<-data$prid[ix]
      laftot<-laf[ix]
      # assign each station to the corresponding box
      if (argv$debug) itotdeb<-extract(r,cbind(x,y))
      itot<-extract(r,cbind(xtot,ytot))
      # count the number of observations in each box
      rnobs<-rasterize(cbind(xtot,ytot),r,ttot,fun=function(x,...)length(x))
      nr<-getValues(rnobs)
      # create the 4D array for the function call via apply
      ixyn<-cbind(ir,xr,yr,nr)
      # SCT within each (grid) box
      # NOTE: dqcflag is updated in "sct" function
      out<-apply(ixyn,
                 FUN=sct,MARGIN=1,
                 nmin=argv$n.sct,
                 dzmin=argv$dz.sct,
                 Dhmin=argv$DhorMin.sct,
                 Dz=argv$Dver.sct,
                 eps2=argv$eps2.sct,
                 T2=argv$thr.sct,
                 T2pos=argv$thrpos.sct,
                 T2neg=argv$thrneg.sct,
                 sus.code=argv$sct.code,
                 faster=argv$fast.sct)
    } # endif SCT stn-by-stn or boxes
  } else {
    print("no valid observations left, no SCT")
  }
  ncur<-length(which(dqcflag==argv$sct.code))
  if (argv$verbose | argv$debug) {
    t1a<-Sys.time()
    str<-"("
    for (f in 1:nfin) {
      if (f>1) str<-paste0(str,"; ") 
      aux<-which(dqcflag==argv$sct.code & data$prid==argv$prid[f])
      str<-paste0(str,"prid",argv$prid[f],"=",length(aux))
      rm(aux)
    }
    str<-paste0(str,")")
    print(paste("SCT, iteration=",i,
                "/time",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
    print(paste("# suspect observations=",ncur-nprev,str))
    rm(str)
  }
  if ((ncur-nprev)<=argv$break.sct) break
  nprev<-ncur
}
rm(doit)
if (argv$verbose | argv$debug) 
  print("+---------------------------------+")
if (argv$debug) 
  save.image(file.path(argv$debug.dir,"dqcres_sct.RData")) 
#
# coefficient of observation representativeness
#-----------------------------------------------------------------------------
# corep has been set by function sct to the observation error variance
if (!any(is.na(argv$const.corep))) {
  for (f in 1:nfin) {
    if (any(data$prid[ix]==argv$prid[f])) {
      ip<-which(data$prid[ix]==argv$prid[f])
      if (length(ip)>0) corep[ix[ip]]<-argv$const.corep[f]
    } else {
      print(paste("provider ",argv$prid[f],
      ": no valid data found to compute the coefficient of representativeness",sep=""))
    }
  }
} else {
  ix<-which(!is.na(corep) & (is.na(dqcflag) | dqcflag==argv$keep.code)) 
  if (length(ix)>0) {
    qmn<-0.25
    qmx<-0.75
    qav<-0.5
    acorep<-abs(corep[ix])
    # ecdf(x)(x) here should give us something similar to rank(x)/length(x)
    qcorep<-ecdf(acorep)(acorep)
    if (any(qcorep<qmn)) qcorep[which(qcorep<qmn)]<-qmn
    if (any(qcorep>qmx)) qcorep[which(qcorep>qmx)]<-qmx
    for (f in 1:nfin) {
      if (any(data$prid[ix]==argv$prid[f])) {
        ip<-which(data$prid[ix]==argv$prid[f] & qcorep<=qav)
        if (length(ip)>0)      
          corep[ix[ip]]<-argv$min.corep[f]+
            (argv$mean.corep[f]-argv$min.corep[f])*(qcorep[ip]-qmn)/(qav-qmn)
        ip<-which(data$prid[ix]==argv$prid[f] & qcorep>qav)
        if (length(ip)>0)      
          corep[ix[ip]]<-argv$mean.corep[f]+
            (argv$max.corep[f]-argv$mean.corep[f])*(qcorep[ip]-qav)/(qmx-qav)
      } else {
        print(paste("provider ",argv$prid[f],
        ": no valid data found to compute the coefficient of representativeness",sep=""))
      }
    }
  } else {
    print("no valid first guess for the observation error variance found")
  } 
}
if (argv$debug) 
  save.image(file.path(argv$debug.dir,"dqcres_sct.RData")) 
#-----------------------------------------------------------------------------
# cool test (Check fOr hOLes in the field)
if (argv$cool) {
  nprev<-0
  if (argv$verbose | argv$debug) 
    print(paste0("cool test (",argv$cool.code,")"))
  # set doit vector
  doit<-vector(length=ndata,mode="numeric")
  doit[]<-NA
  for (f in 1:nfin) doit[data$prid==argv$prid[f]]<-argv$doit.cool[f]
  ix<-which((is.na(dqcflag) | dqcflag==argv$keep.code) & doit!=0)
  ptmp<-length(ix)
  if (ptmp<1) {
    print("cool test no valid observations left, no test")
  } else {
    rgrid_cool<-raster(ext=e,resolution=argv$grid_res.cool)
    rgrid_cool[]<-NA
    xygrid_cool<-xyFromCell(rgrid_cool,1:ncell(rgrid_cool))
    xgrid_cool<-xygrid_cool[,1]
    ygrid_cool<-xygrid_cool[,2]
    rm(xygrid_cool)
    ngrid_cool<-length(ygrid_cool)
    if (!exists("xobs_cool_aux")) xobs_cool_aux<-integer(0)
    if (!exists("yobs_cool_aux")) yobs_cool_aux<-integer(0)
    if (!exists("pridobs_cool_aux")) pridobs_cool_aux<-integer(0)
    if (!exists("yo_cool_aux")) yo_cool_aux<-integer(0)
    # test
    for (i in 1:argv$i.cool) {
      t0a<-Sys.time()
      for (j in 1:length(argv$thres.cool)) {
        # use only (probably) good observations
        ix<-which((is.na(dqcflag) | dqcflag==argv$keep.code) & doit!=0)
        if (length(ix)>0) {
          xobs_to_check_cool<-x[ix]
          yobs_to_check_cool<-y[ix]
          pridobs_to_check_cool<-data$prid[ix]
          xobs_cool<-c(x[ix],xobs_cool_aux)
          yobs_cool<-c(y[ix],yobs_cool_aux)
          yo_cool<-c(data$value[ix],yo_cool_aux)
          rgrid1<-rasterize(x=cbind(xobs_cool,yobs_cool),
                            y=rgrid_cool,
                            field=yo_cool,fun=mean,na.rm=T)
          ix1<-which(!is.na(getValues(rgrid1)))
          xy1<-xyFromCell(rgrid1,ix1)
          xobs_cool<-xy1[,1]
          yobs_cool<-xy1[,2]
          yo_cool<-getValues(rgrid1)[ix1]
          if (!is.na(argv$cores)) {
            arr<-t(mcmapply(spint_cool,
                            1:ngrid_cool,
                            mc.cores=argv$cores,
                            SIMPLIFY=T,
                            thres=argv$thres.cool[j],
                            condition=argv$condition.cool[j],
                            dh_max=argv$dh_max.cool))
          # no-multicores
          } else {
            arr<-t(mapply(spint_cool,
                          1:ngrid_cool,
                          SIMPLIFY=T,
                          thres=argv$thres.cool[j],
                          condition=argv$condition.cool[j],
                          dh_max=argv$dh_max.cool))
          }
          rgrid_cool[]<-arr
          rc<-clump(rgrid_cool)
          dc<-getValues(rc)
          freq_rc<-freq(rc)
          ytmp<-extract(rc,cbind(xobs_to_check_cool,yobs_to_check_cool),na.rm=T)
          for (k in 1:length(freq_rc[,1])) {
            if (is.na(freq_rc[k,1])) next
            flag_k<-ytmp==freq_rc[k,1] & !is.na(ytmp)
            if (length(which(flag_k))<n.cool[j,1]) {
              for (f in 1:nfin) {
                ixijkf<-which(flag_k & pridobs_to_check_cool==argv$prid[f])
                if (length(ixijkf)<n.cool[j,(f+1)]) {
                  dqcflag[ix[ixijkf]]<-argv$cool.code
                }
              } 
            }
          }
        } else {
          print("no valid observations left, no cool test" )
        }
      } # end of loop over threshold that define events
      ncur<-length(which(dqcflag==argv$cool.code & !is.na(dqcflag)))
      if (argv$verbose | argv$debug) {
        t1a<-Sys.time()
        print(paste("cool test. iteration=",i,
                    "/time",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
        print(paste("# suspect observations=",ncur-nprev))
      } 
      if ((ncur-nprev)<=argv$break.cool) break
      nprev<-ncur
    } # end of loop over test iterations
    rm(doit)
    if (argv$verbose | argv$debug) 
      print("+---------------------------------+")
    if (argv$debug) 
      save.image(file.path(argv$debug.dir,"dqcres_cool.RData")) 
  } # end of "if (length(mask_cool)==0)"
} # end of if (argv$cool)
#
#-----------------------------------------------------------------------------
# check for isolated stations
# use only (probably) good observations
# NOTE: keep-listed stations canNOT be flagged here
# set doit vector
doit<-vector(length=ndata,mode="numeric")
doit[]<-NA
for (f in 1:nfin) doit[data$prid==argv$prid[f]]<-argv$doit.iso[f]
#
ix<-which(is.na(dqcflag) & doit!=0)
if (length(ix)>0) {
  # define global 1D vector used in nstat (1D for fast access)
  xtot<-x[ix]
  ytot<-y[ix]
  xy<-cbind(xtot,ytot)
  ns<-apply(xy,FUN=nstat,MARGIN=1,drmin=argv$dr.isol)
  sus<-which(ns<argv$n.isol & doit[ix]==1)
  # set dqcflag
  if (length(sus)>0) dqcflag[ix[sus]]<-argv$isol.code
} else {
  print("no valid observations left, no check for isolated observations")
}
rm(doit)
if (argv$verbose | argv$debug) {
  print(paste("# isolated observations=",length(which(dqcflag==argv$isol.code))))
  print("+---------------------------------+")
}
if (argv$debug) 
  save.image(file.path(argv$debug.dir,"dqcres_iso.RData")) 
#
#-----------------------------------------------------------------------------
# observations not flagged are assumed to be good observations 
dqcflag[is.na(dqcflag)]<-0
if (argv$verbose | argv$debug) {
  nsus<-length(which(dqcflag!=0))
  nok<-length(which(dqcflag==0))
  nsus_notna<-length(which(dqcflag!=0 & !is.na(data$value)))
  nok_notna<-length(which(dqcflag==0 & !is.na(data$value)))
  nnotna<-length(which(!is.na(data$value)))
  nna<-length(which(is.na(data$value)))
  print("summary:")
  print(" #  NAs, number of observations with no value (NAs)")
  print(" #  sus, number of suspicious observations or no-metadata")
  print(" # good, number of good observations")
  print(" NOTE for sus and good, the statistics consider only observations not NAs")
  print("summary:")
  print(paste0(" #  NAs= ",nna))
  print(paste0(" #  sus= ",nsus_notna," [",round(100*nsus_notna/nnotna,0),"%]"))
  print(paste0(" # good= ",nok," [", round(100*nok_notna/nnotna,0), "%]"))
   if (nfin>1) {
    for (f in 1:nfin) {
      nsus<-length(which(dqcflag!=0 & data$prid==argv$prid[f]))
      nok<-length(which(dqcflag==0 & data$prid==argv$prid[f]))
      nsus_notna<-length(which(dqcflag!=0 & 
                               !is.na(data$value) & 
                               data$prid==argv$prid[f]))
      nok_notna<-length(which(dqcflag==0 & 
                              !is.na(data$value) & 
                              data$prid==argv$prid[f]))
      nnotna<-length(which(!is.na(data$value) & 
                           data$prid==argv$prid[f]))
      nna<-length(which(is.na(data$value) & data$prid==argv$prid[f]))
      print(paste("--> summary provider",argv$prid[f]))
      print(paste0("  #  NAs= ",nna))
      print(paste0("  #  sus= ",nsus_notna," [",round(100*nsus_notna/nnotna,0),"%]"))
      print(paste0("  # good= ",nok," [", round(100*nok_notna/nnotna,0), "%]"))

    }
  }
  print("+---------------------------------+")
}
#
#-----------------------------------------------------------------------------
# Include radar-derived precipitation in the output file
if (argv$radarout) source( file.path( titan_mod_path, "main_radar_in_output.r"))
#
#-----------------------------------------------------------------------------
# write the output file
source( file.path( titan_mod_path, "main_output.r"))
#
#-----------------------------------------------------------------------------
# Normal exit
t1 <- Sys.time() # game over 
if ( argv$verbose) 
 cat( paste("normal exit /time", round(t1-t0,1), attr(t1-t0,"unit"), "\n"))
quit( status = 0 )
