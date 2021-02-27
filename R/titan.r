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
suppressPackageStartupMessages( library( "argparser"))
suppressPackageStartupMessages( library( "sp"))
suppressPackageStartupMessages( library( "raster"))
suppressPackageStartupMessages( library( "rgdal"))

options( warn = 2, scipen = 999)

#
#==============================================================================
#  MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN*MAIN
#==============================================================================
t0 <- Sys.time() # game on
#
#-----------------------------------------------------------------------------
# path to the titan functions is stored in the enviroment var TITANR_FUN
titan_fun_path <- Sys.getenv( "TITANR_PATH")
#
#-----------------------------------------------------------------------------
# load functions
fun_file <- file.path( titan_fun_path, "fun_list.r")
if ( !file.exists( fun_file))
    boom( fun_file, code=1)
source( fun_file)
for (fun in fun_list) {
  if ( !file.exists(file.path( titan_fun_path, fun)))
    boom( file.path( titan_fun_path, fun), code=1)
  source( file.path( titan_fun_path, fun))
}
rm( fun_file, fun_list, fun)               
# Alternatively, more elegant but difficult to find missing functions
#for (file in list.files(path = titan_fun_path, pattern = ".r", full.names=T) ) 
#  source(file)
#rm(file)
#
#-----------------------------------------------------------------------------
# define constants
proj4_input_obsfiles_default    <- "+proj=longlat +datum=WGS84"
proj4_where_dqc_is_done_default <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
xy.dig.out_default <- 5
varname.y.out_default <- "lat"
varname.x.out_default <- "lon"
#
#-----------------------------------------------------------------------------
# read command line arguments and/or configuration file
argv <- argparser()
print(argv$i.buddy)
#
#-----------------------------------------------------------------------------
# Load titan library
dyn.load( file.path( argv$titanlib_path, 
                     paste( "SWIG/R/titanlib", 
                            .Platform$dynlib.ext,
                            sep="")))
source( file.path( argv$titanlib_path, "SWIG/R/titanlib.R"))
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
nfin        <- length( argv$input.files)
res         <- read_data_to_check( argv, nfin)
extent      <- res$extent
data        <- res$data
ndata       <- length(data$lat)
dqcflag     <- res$dqcflag
z           <- res$z
sctpog      <- res$sctpog
corep       <- res$corep
dataopt     <- res$dataopt
varidx      <- res$varidx
varidx.opt  <- res$varidx.opt
rm(res)
#
#-----------------------------------------------------------------------------
# test for no metadata (1st round) 
#  1st round, check for missing metadata in the original data
#  2nd round, check for missing metadata after dem.fill 
dqcflag.bak <- dqcflag # bakup, used in main_read_dem.r (if dem.fill=T)
dqcflag <- metadata_check_r ( argv, data, z, extent, dqcflag)
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
  res     <- read_dem( argv, data, z, dqcflag, dqcflag.bak)
  z       <- res$z    # station elevations (either from input or dem) 
  zdem    <- res$zdem # dem elevation
  dqcflag <- res$dqcflag
  rm(res, dqcflag.bak)
}
# land area fraction (%, e.g 10 yes, 0.1 no)
if (argv$laf.sct) {
  laf <- read_laf( argv)
} else { # use a fake laf
  laf <- rep( 1, ndata)
}
#
#-----------------------------------------------------------------------------
# precipitation (in-situ) and temperature (field) cross-check
if (argv$ccrrt) {
  res     <- ccrrt( argv, data, z, dqcflag)
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
# Read deterministic first guess
if ( !is.na( argv$fg.file)) {
  res    <- read_fg( argv, extent)
  rfg    <- res$rfg
  rfgdem <- res$rfgdem
  rrad   <- NULL 
  if ( argv$radarout) rrad <- res$rfg
  rm(res)
} else {
  rfg      <- NULL
  rfgdem   <- NULL
  rrad     <- NULL 
}
#
#-----------------------------------------------------------------------------
# Read first guess ensemble
if ( !is.na( argv$fge.file)) {
  res <- read_fge( argv, extent)
  rfge    <- res$rfge
  rfgedem <- res$rfgedem
} else {
  rfge    <- NULL 
  rfgedem <- NULL 
}
#
#-----------------------------------------------------------------------------
# test for no metadata (2nd and final) 
dqcflag <- metadata_check_r ( argv, data, z, extent, dqcflag)
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
dqcflag <- plausibility_test( argv, data, dqcflag)
#
#-----------------------------------------------------------------------------
# climatological check 
# NOTE: keep-listed stations canNOT be flagged here
# use only (probably) good observations
if (!is.na(argv$month.clim))
  dqcflag <- climatological_check( argv, ndata, data, dqcflag)
#
#-----------------------------------------------------------------------------
# buddy check (based on the definition of a binary yes/no event)
#  Define an event compare each observation against the average of neighbouring observations 
# NOTE: keep-listed stations are used but they canNOT be flagged here
if (argv$buddy_eve)
  dqcflag <- buddy_eve( argv, ndata, data, dqcflag)
#
#-----------------------------------------------------------------------------
# buddy check (standard)
#  compare each observation against the average of neighbouring observations 
# NOTE: keep-listed stations are used but they canNOT be flagged here
if (argv$buddy)
  dqcflag <- buddy( argv, ndata, data, dqcflag)
#
#-----------------------------------------------------------------------------
# check against a first-guess (deterministic)
if (argv$fg) 
  dqcflag <- fgt_det( argv, ndata, data, x, y, rfg, rfgdem, dqcflag)
q()
#
#-----------------------------------------------------------------------------
# check against a first-guess (ensemble)
# print(fge.mu)
# print(fge.sd)
if (argv$fge)
  dqcflag <- fg_ens( argv,data,dqcflag,fge.mu,fge.sd)
#
#-----------------------------------------------------------------------------
# SCT - Spatial Consistency Test
# NOTE: keep-listed stations are used but they canNOT be flagged here
if (argv$sct)
  dqcflag <- sct_test( argv, ndata, data, dqcflag, x, y, z, laf)
#-----------------------------------------------------------------------------
# cool test (Check fOr hOLes in the field)
if (argv$cool) 
  dqcflag <- cool_test( argv, data, dqcflag, x, y)
#
#-----------------------------------------------------------------------------
# check for isolated stations
# use only (probably) good observations
if (argv$isolation_check)
  dqcflag <- isolation_test( argv, ndata, data, dqcflag, x, y)
#
#-----------------------------------------------------------------------------
# observations not flagged are assumed to be good observations 
dqcflag <- final_decision( data, dqcflag)
#
#-----------------------------------------------------------------------------
# write the output file
if (!argv$radarout) rrad <- NULL
write_output( argv, rrad, data, dqcflag, sctpog, corep, 
              dataopt, varidx, varidx.opt) 
#
#-----------------------------------------------------------------------------
# Normal exit
rip( code=1, t0=t0) # exit status is 0
