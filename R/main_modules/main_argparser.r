# create parser object
p <- arg_parser("titan")
# specify our desired options
#.............................................................................. 
# REQUIRED input 
p <- add_argument(p, "input",
                  help="input file",
                  type="character")
p <- add_argument(p, "output",
                  help="output file",
                  type="character",
                  default="output.txt")
# configuration file
p <- add_argument(p, "--config.file",
                  help="configuration file",
                  type="character",
                  default=NULL,
                  short="cf")
#.............................................................................. 
# VARIABLE definition 
p<- add_argument(p, "--variable",
                 help=paste("meteorological variable (T temperature,",
                            " RR precipitation, RH relative humidity,",
                            " SD surface_snow_thickness)"),
                 type="character",
                 default="T",
                 short="-var")
# parameter for the Box-Cox transformation (rquired for var=RR)
p <- add_argument(p, "--boxcox.lambda",
                  help="parameter used in the Box-Cox transformation (var RR)",
                  type="numeric",default=0.5,short="-l")
#.............................................................................. 
# titan path (use for the sct with smart boxes)
p <- add_argument(p, "--titan_path",
                  help="path to the directory where the TITAN code is",
                  type="character",
                  default=NULL,
                  short="-tip")
#.............................................................................. 
neg.str<-"Negative values can be specified in either one of these two ways: (1) by using the corresponding \"...neg...\" command line argument; (2) negative values start with \"_\" (e.g. _2=-2)"
# ADDITIONAL input files / providers
p <- add_argument(p, "--input.files",
                  help="additional input files (provider2 provider3 ...)",
                  type="character",
                  default=NULL,
                  nargs=Inf,
                  short="-i")
p <- add_argument(p, "--prid",
                  help="provider identifiers (provider1 provider2 provider3 ...)",
                  type="character",
                  default=NA,
                  nargs=Inf,
                  short="-pr")
p <- add_argument(p, "--input.offset",
                  help=paste("offset applied to the input files (one for each provider, default=0).",neg.str),
                  type="character",
                  default=NA,
                  nargs=Inf,
                  short="-io")
p <- add_argument(p, "--input.negoffset",
                  help="sign for the offsets (1=negative; 0=positive, def=0)",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-ion")
#
p <- add_argument(p, "--input.cfact",
                  help=paste("correction factor applied to the input files (one for each provider, default=1)",neg.str),
                  type="character",
                  default=NA,
                  nargs=Inf, 
                  short="-icf")
p <- add_argument(p, "--input.negcfact",
                  help="sign for the correction factors (1=negative; 0=positive, def=0)",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-icfn")
#.............................................................................. 
# DEBUG
p <- add_argument(p, "--debug",
                  help="debug mode",
                  flag=T,
                  short="-dbg")
p <- add_argument(p, "--debug.dir",
                  help="directory for debug output",
                  type="character",
                  default=".",
                  short="-dbgd")
p <- add_argument(p, "--verbose",
                  help="debug mode",
                  flag=T,
                  short="-v")
#.............................................................................. 
# Quality control codes
p <- add_argument(p, "--nometa.code",
                  help="quality code returned in case of missing metadata",
                  type="numeric",
                  default=1,
                  short="-nometac")
p <- add_argument(p, "--p.code",
  help="quality code returned in case of the check on plausible values fails",
                  type="numeric",
                  default=2,
                  short="-pcodec")
p <- add_argument(p, "--clim.code",
 help="quality code returned in case of the check on climatological values fails",
                  type="numeric",
                  default=3,
                  short="-climc")
p <- add_argument(p, "--buddy.code",
  help="quality code returned in case of the buddy check fails",
                  type="numeric",
                  default=4,
                  short="-buddyc")
p <- add_argument(p, "--sct.code",
                  help="quality code returned in case of SCT fails",
                  type="numeric",
                  default=5,
                  short="-sctc")
p <- add_argument(p, "--dem.code",
                  help="quality code returned in case of SCT fails",
                  type="numeric",
                  default=6,
                  short="-demc")
p <- add_argument(p, "--isol.code",
        help="quality code returned in case of isolation check fails",
                  type="numeric",
                  default=7,
                  short="-isolc")
p <- add_argument(p, "--fg.code",
 help="quality code returned in case of check against a first-guess field (deterministic) fails",
                  type="numeric",
                  default=8,
                  short="-fgc")
p <- add_argument(p, "--fge.code",
 help="quality code returned in case of check against a first-guess field (ensemble) fails",
                  type="numeric",
                  default=10,
                  short="-fgc")
p <- add_argument(p, "--ccrrt.code",
                  help=paste("quality code returned in case of precipitation",
                             "and temperature crosscheck fails"),
                  type="numeric",
                  default=11,
                  short="-ccrrtc")
p <- add_argument(p, "--cool.code",
             help=paste("quality code returned in case of cool check fails"),
                  type="numeric",
                  default=15,
                  short="-coolc")
p <- add_argument(p, "--buddy_eve.code",
  help="quality code returned in case of the buddy check event-based fails",
                  type="numeric",
                  default=13,
                  short="-buddyec")
p <- add_argument(p, "--black.code",
    help="quality code assigned to observations listed in the blacklist",
                  type="numeric",
                  default=100,
                  short="-blackc")
p <- add_argument(p, "--keep.code",
    help="quality code assigned to observations listed in the keep-list",
                  type="numeric",
                  default=100,
                  short="-keepc")
#.............................................................................. 
# standard value for the moist adiabatic lapse rate
p <- add_argument(p, "--gamma.standard",
  help="standard value for the moist adiabatic temperature lapse rate dT/dz",
                  type="numeric",
                  default=-0.0065)
#.............................................................................. 
# GEOGRAPHICAL domain definition  
# NOTE: lat-lon setup to have Oslo in a single box
p <- add_argument(p, "--lonmin",
                  help="longitude of south-eastern domain corner",
                  type="character",
                  default="3",
                  short="-lon")
p <- add_argument(p, "--lonmax",
                  help="longitude of south-western domain corner",
                  type="character",
                  default="33",
                  short="-lox")
p <- add_argument(p, "--latmin",
                  help="latitude of south-eastern domain corner",
                  type="character",
                  default="53.25",
                  short="-lan")
p <- add_argument(p, "--latmax",
                  help="latitude of north-western domain corner",
                  type="character",
                  default="71.8",
                  short="-lax")
p <- add_argument(p, "--dqc_inbox_only",
                  help="perform dqc only in the defined box (lonmin,lonmax,latmin,latmax)",
                  flag=T)

# transformation between coordinate reference systems
p <- add_argument(p, "--spatconv",
                  help="flag for conversion of spatial coordinates before running the data quality checks",
                  flag=T,
                  short="-c")
p <- add_argument(p, "--proj4from",
                  help="proj4 string for the original coordinate reference system (obsolete, use \"proj4_input_obsfiles\")",
                  type="character",
                  default=proj4_input_obsfiles_default,
                  short="-pf")
p <- add_argument(p, "--proj4_input_obsfiles",
                  help="proj4 string for the original coordinate reference system",
                  type="character",
                  default=proj4_input_obsfiles_default)
p <- add_argument(p, "--proj4to",
                  help="proj4 string for the coordinate reference system where the DQC is performed (obsolete, use \"proj4_where_dqc_is_done\"",
                  type="character",
                  default=proj4_where_dqc_is_done_default,
                  short="-pt")
p <- add_argument(p, "--proj4_where_dqc_is_done",
                  help="proj4 string for the coordinate reference system where the DQC is performed",
                  type="character",
                  default=proj4_where_dqc_is_done_default)
p <- add_argument(p, "--proj4_output_files",
                  help="proj4 string for the output coordinate reference system",
                  type="character",
                  default=proj4_input_obsfiles_default)
#.............................................................................. 
# duplicates
p <- add_argument(p, "--no_duplicates",
                  help="remove duplicates from input data",
                  flag=T)
p <- add_argument(p, "--dup.match_tol_x",
                  help="remove duplicates, matching tolerance for lat and lon (degrees)",
                  type="numeric",
                  default=0.00001)
p <- add_argument(p, "--dup.match_tol_z",
                  help="remove duplicates, matching tolerance for elevation (m)",
                  type="numeric",
                  default=1)
#.............................................................................. 
# INPUT/OUTPUT names
p <- add_argument(p, "--separator",
                  help="character vector, input file(s) separator character(s) (default '';'')",
                  type="character",
                  nargs=Inf,
                  default=NA)
p <- add_argument(p, "--separator.out",
                  help="separator character in the output file",
                  type="character",
                  default=";")
p<- add_argument(p, "--latlon.dig.out",
                 help="number of decimal digits for latitude and longitude in the output file  (obsolete, use xy.dig.out)",
                 type="numeric",
                 default=5,
                 short="-lldo")
p<- add_argument(p, "--xy.dig.out",
                 help="number of decimal digits for northing and easting coordinates in the output file",
                 type="numeric",
                 default=xy.dig.out_default)
p<- add_argument(p, "--elev.dig.out",
                 help="number of decimal digits for elevation in the output file",
                 type="numeric",
                 default=1,
                 short="-edo")
p<- add_argument(p, "--value.dig.out",
                 help="number of decimal digits for the returned value in the output file",
                 type="numeric",
                 default=1,
                 short="-vdo")
p <- add_argument(p, "--varname.lat",
                  help="character vector, latitude variable name(s) in the input file (default ''lat'')",
                  type="character",
                  nargs=Inf,
                  short="-vlat")
p <- add_argument(p, "--varname.lat.out",
                  help="latitude variable name in the output file (obsolete, use varname.y.out)",
                  type="character",
                  default="lat")
p <- add_argument(p, "--varname.y.out",
                  help="northing coordinate name in the output file",
                  type="character",
                  default=varname.y.out_default)
p <- add_argument(p, "--varname.lon",
                  help="character vector, longitude variable name(s) in the input file (default ''lon'')",
                  type="character",
                  nargs=Inf,
                  short="-vlon")
p <- add_argument(p, "--varname.lon.out",
                  help="longitude variable name in the output file (obsolete, use varname.x.out)",
                  type="character",
                  default="lon")
p <- add_argument(p, "--varname.x.out",
                  help="easting coordinate name in the output file",
                  type="character",
                  default="lon")
p <- add_argument(p, "--varname.elev",
                  help="character vector, elevation variable names(s) in the input file (default ''elev'')",
                  type="character",
                  nargs=Inf,
                  short="-vele")
p <- add_argument(p, "--varname.elev.out",
                  help="elevation variable name in the output file",
                  type="character",
                  default="elev")
p <- add_argument(p, "--elev_not_used",
                  help="elevation is not used (will be set to zero)",
                  flag=T)
p <- add_argument(p, "--varname.value",
                  help="character vector, variable name(s) in the input file (default ''value'')",
                  type="character",
                  nargs=Inf,
                  short="-vval")
p <- add_argument(p, "--varname.value.out",
                  help="name for the variable values (out)",
                  type="character",
                  default="value")
p <- add_argument(p, "--varname.fg.out",
                  help="name for the first guess (out)",
                  type="character",
                  default="fg")
p <- add_argument(p, "--varname.fge_mean.out",
                  help="name for the first guess ensemble mean (out)",
                  type="character",
                  default="fge_mean")
p <- add_argument(p, "--varname.fge_sd.out",
                  help="name for the first guess standard deviation (out)",
                  type="character",
                  default="fge_sd")
# output file
p <- add_argument(p, "--varname.opt",
     help="additional optional variables to be written on the output (out)",
                  type="character",
                  default=NA,
                  nargs=Inf,
                  short="-vopt")
p<- add_argument(p, "--varname.prid",
                 help="name for the provider identifier (out)",
                 type="character",
                 default="prid",
                 short="-vprid")
p<- add_argument(p, "--varname.dqc",
                 help="name for the data quality control flag (out)",
                 type="character",
                 default="dqc",
                 short="-vdqc")
p<- add_argument(p, "--varname.sct",
            help="name for the spatial consistency test returned value (out)",
                 type="character",
                 default="sct",
                 short="-vsct")
p<- add_argument(p, "--varname.rep",
            help="name for the coefficient of representativeness (out)",
                 type="character",
                 default="rep",
                 short="-vrep")
#.............................................................................. 
# metadata check
p <- add_argument(p, "--zmin",
                  help="minimum allowed elevation in the domain [m amsl]",
                  type="numeric",
                  default=0,
                  short="-z")
p <- add_argument(p, "--zmax",
                  help="maximum allowed elevation in the domain [m amsl]",
                  type="numeric",
                  default=2500,
                  short="-Z")
#.............................................................................. 
# Plausibility check
p <- add_argument(p, "--tmin",
                  help="minimum allowed temperature [K or degC] (deprecated, use vmin instead)",
                  type="numeric",
                  default=NA,
                  short="-tP")
p <- add_argument(p, "--tmax",
                  help="maximum allowed temperature [K or degC] (deprecated, use vmax instead)",
                  type="numeric",
                  default=NA,
                  short="-TP")
p <- add_argument(p, "--vmin",
                  help=paste("minimum allowed value [units of the variable specified]",neg.str),
                  type="character",
                  default="_50")
p <- add_argument(p, "--vmax",
                  help=paste("maximum allowed value [units of the variable specified]",neg.str),
                  type="character",
                  default="40")
p <- add_argument(p, "--vminsign",
                  help="minimum allowed value, sign [1=neg, 0=pos]",
                  type="numeric",
                  default=0)
p <- add_argument(p, "--vmaxsign",
                  help="maximum allowed value, sign [1=neg, 0=pos]",
                  type="numeric",
                  default=0)
#.............................................................................. 
# Climatological check
# default based on Norwegian hourly temperature from 2010-2017
p <- add_argument(p, "--tmin.clim",
                  help="minimum allowed temperature [K or degC] (deprecated)",
                  type="numeric",
                  nargs=12,
                  short="-tC",
                  default=rep(NA,12))
p <- add_argument(p, "--tmax.clim",
                  help="maximum allowed temperature [K or degC] (deprecated)",
                  type="numeric",
                  nargs=12,
                  short="-TC",
                  default=rep(NA,12))
p <- add_argument(p, "--vmin.clim",
                  help=paste("minimum allowed value [units of the variable specified]",neg.str),
                  type="character",
                  nargs=12,
                  default=c("_45","_45","_40","_35","_20","_15","_10","_15","_15","_20","_35","_45"))
p <- add_argument(p, "--vmax.clim",
                  help=paste("maximum allowed value [units of the variable specified]",neg.str),
                  type="character",
                  nargs=12,
                  default=c("20","20","25","25","35","35","40","40","35","30","25","20"))
p <- add_argument(p, "--month.clim",
                  help="month (number 1-12)",
                  type="numeric",
                  short="-mC",
                  default=NA)
p <- add_argument(p, "--vminsign.clim",
                  help="minimum allowed value, sign [1=neg, 0=pos]",
                  type="numeric",
                  nargs=12,
                  default=c(0,0,0,0,0,0,0,0,0,0,0,0))
p <- add_argument(p, "--vmaxsign.clim",
                  help="maximum allowed value, sign [1=neg, 0=pos]",
                  type="numeric",
                  nargs=12,
                  default=c(0,0,0,0,0,0,0,0,0,0,0,0))
#.............................................................................. 
# Buddy-check (event-based)
p <- add_argument(p, "--buddy_eve",
                  help="do the buddy check based on the definition of a binary event (yes/no)",
                  flag=T,
                  short="-Be")
p <- add_argument(p, "--thr_eve.buddy_eve",
                  help=paste("threshold(s) used to define the binary event (same units as the variable). It is possible to specify more than one threshold, then titan executes more than one check. (same units of the specified variable). Consider an arbitrary observation, each threshold defines two events: (i) event='yes' when observed value is less than the threshold and (ii) event='no' when the observd value is greater or equal to the threhsold."),
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--dr.buddy_eve",
                  help="perform each test within a (2*dr)-by-(2*dr) square-box around each observation [m] (default is 3000m). This is a numeric vector with the same dimension of the vector specifying the thresholds",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--i.buddy_eve",
                  help="number of iterations",
                  type="integer",
                  default=1,
                  short="-iBe")
p <- add_argument(p, "--thr.buddy_eve",
                  help="threshold(s) used to perform the quality checks. This is a numeric vector with the same dimension of the vector specifying the thresholds defining the binary events. Consider the j-th elment of thr.buddy_eve, there are two mode of operations. Mode 'A', when thr.buddy_eve[j] is less than 1. Mode 'B', when thr.buddy_eve[j] is equal to or greater than 1. Mode 'A', thr.buddy_eve[j] specifies a percentage and an observation can be flagged as suspect for two reasons: if the observed event is 'yes' ('no') and the percentage of 'yes' ('no') among the buddies is equal to or less than thr.buddy_eve[j]. Mode 'B', thr.buddy_eve[j] specifies a number of observations and an observation can be flagged as suspect for two reasons: if the observed event is 'yes' ('no') and the number of 'yes' ('no') among the buddies is less than thr.buddy_eve[j]. The default values is 0.05, i.e. 5% of the neighbouring observations.",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--n.buddy_eve",
                  help="consider an arbitrary observation, how many neighbouring observations do we need to call them 'buddies' and perform the check? We need a number greater than 'n'. This is a numeric vector with the same dimension of the vector specifying the thresholds defining the binary events. Default is set to 5.",
                  type="integer",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--dz.buddy_eve",
                  help="do not perform the check if at least one of the elevation differences between an observation and its buddies is equal to or greater than the threshold(s) 'dz'. This is a numeric vector with the same dimension of the vector specifying the thresholds defining the binary events. Default is 30 m.",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--prio.buddy_eve",
                  help="specify priorities. This is a numeric vector, with the dimension of the number of providers. One positive value for each provider. The smaller the value, the greater the priority. Priorities are used only in the first round of the check, when each observation is compared only against other observations having a priority equal to or grater than its own.",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-prBe")
p <- add_argument(p, "--usefg.buddy_eve",
                  help="should first-guess values be used in the test? 1=yes, otherwise=no. This is a numeric vector, with the same dimension of the vector specifying the thresholds. If used, the first-guess priority is set to '-1'.",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--break.buddy_eve",
                  help="break the loop if the number of flagged observations in the last iretation (by considering al the test) is euqual to or less than this value.",
                  type="numeric",
                  default=0)
#.............................................................................. 
# Buddy-check
p <- add_argument(p, "--dr.buddy",
                  help="perform each test within a (2*dr)-by-(2*dr) square-box around each observation [m] (default is 3000m). This is a numeric vector with the same dimension of the vector specifying the thresholds",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-dB")
p <- add_argument(p, "--i.buddy",
                  help="number of iterations",
                  type="integer",
                  default=1,
                  short="-iB")
p <- add_argument(p, "--thr.buddy",
                  help="threshold(s) used to perform the quality checks. This is a numeric vector with the same dimension of the vector specifying the sizes of the square boxes. flag observation if: abs( obs - mean(buddies) ) / st_dev(buddies) > threshold. Default is 3.", 
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-thB")
p <- add_argument(p, "--sdmin.buddy",
                  help="minimum allowed value(s) for the standard deviation. This is a numeric vector with the same dimension of the vector specifying the sizes of the square boxes.",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--n.buddy",
                  help="consider an arbitrary observation, how many neighbouring observations do we need to call them 'buddies' and perform the check? We need a number greater than 'n'. This is a numeric vector with the same dimension of the vector specifying the sizes of the square boxes. Default is set to 5.",
                  default=NA,
                  nargs=Inf,
                  type="integer",
                  short="-nB")
p <- add_argument(p, "--dz.buddy",
                  help="do not perform the check if at least one of the elevation differences between an observation and its buddies is equal to or greater than the threshold(s) 'dz'. This is a numeric vector with the same dimension of the vector specifying the sizes of the square boxes. Default is 30 m.",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-zB")
p <- add_argument(p, "--prio.buddy",
                  help="specify priorities. This is a numeric vector, with the dimension of the number of providers. One positive value for each provider. The smaller the value, the greater the priority. Priorities are used only in the first round of the check, when each observation is compared only against other observations having a priority equal to or grater than its own.",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-prB")
p <- add_argument(p, "--usefg.buddy",
                  help="should first-guess values be used in the test? 1=yes, otherwise=no. This is a numeric vector, with the same dimension of the vector specifying the sizes of the square boxes. If used, the first-guess priority is set to '-1'.",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--break.buddy",
                  help="break the loop if the number of flagged observations in the last iretation (by considering al the test) is euqual to or less than this value.",
                  type="numeric",
                  default=0)
p <- add_argument(p, "--transf.buddy",
                  help="apply Box-Cox transformation",
                  flag=T)
#.............................................................................. 
# isolated stations
p <- add_argument(p, "--dr.isol",
                  help="check for the number of observation in a dr-by-dr square-box around each observation [m]",
                  type="numeric",
                  default=25000,
                  short="-dI")
p <- add_argument(p, "--n.isol",
                  help="threshold (number of neighbouring observations) for the identification of isolated observations.",
                  type="integer",
                  default=10,
                  short="-nI")
#.............................................................................. 
# spatial consistency test
p <- add_argument(p, "--grid.sct",
                  help="nrow ncol (i.e. number_of_rows number_of_columns). SCT is performed independently over several boxes. The regular nrow-by-ncol grid is used to define those rectangular boxes where the SCT is performed.",
                  type="integer",
                  nargs=2,
                  default=c(20,20),
                  short="-gS")
p <- add_argument(p, "--i.sct",
                  help="number of SCT iterations",
                  type="integer",
                  default=1,
                  short="-iS")
p <- add_argument(p, "--n.sct",
                  help="minimum number of stations in a box to run SCT",
                  type="integer",default=50,short="-nS")
p <- add_argument(p, "--dz.sct",
                  help="minimum range of elevation in a box to run SCT [m]",
                  type="numeric",
                  default=30,
                  short="-zS")
p <- add_argument(p, "--DhorMin.sct",
                  help=paste("OI, minimum allowed value for the horizontal de-correlation",
                  "lenght (of the background error correlation) [m]"),
                  type="numeric",
                  default=10000,
                  short="-hS")
p <- add_argument(p, "--Dver.sct",
                  help="OI, vertical de-correlation lenght  (of the background error correlation) [m]",
                  type="numeric",
                  default=200,
                  short="-vS")
p <- add_argument(p, "--eps2.sct",
                  help="OI, ratio between observation error variance and background error variance. eps2.sct is a vector of positive values (not NAs). If eps2.sct has the same length of the number of input files, then a provider dependent eps2 will be used in the SCT. Otherwise, the value of eps2.sct[1] will be used for all providers and any other eps2.sct[2:n] value will be ignored",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-eS")
p <- add_argument(p, "--thr.sct",
                  help="SCT threshold. flag observation if: (obs-Cross_Validation_pred)^2/(varObs+varCVpred) > thr.sct. thr.sct is a vector of positive values (not NAs). If thr.sct has the same length of the number of input files, then a provider dependent threshold will be used in the SCT. Otherwise, the value of thr.sct[1] will be used for all providers and any other thr.sct[2:n] value will be ignored ",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-tS")
p <- add_argument(p, "--thrpos.sct",
                  help="SCT threshold. flag observation if: (obs-Cross_Validation_pred)^2/(varObs+varCVpred) > thrpos.sct AND (obs-Cross_Validation_pred)>=0. thrpos.sct is a vector of positive values (not NAs). If thrpos.sct has the same length of the number of input files, then a provider dependent threshold will be used in the SCT. Otherwise, the value of thrpos.sct[1] will be used for all providers and any other thrpos.sct[2:n] value will be ignored ",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-tpS")
p <- add_argument(p, "--thrneg.sct",
                  help="SCT threshold. flag observation if: (obs-Cross_Validation_pred)^2/(varObs+varCVpred) > thrneg.sct AND (obs-Cross_Validation_pred)<0.  thrneg.sct is a vector of positive values (not NAs). If thrneg.sct has the same length of the number of input files, then a provider dependent threshold will be used in the SCT. Otherwise, the value of thrneg.sct[1] will be used for all providers and any other thrneg.sct[2:n] value will be ignored ",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-tnS")
p <- add_argument(p, "--laf.sct",
                  help="use land area fraction in the OI correlation function (0-100%)",
                  flag=T,
                  short="-lS")
p <- add_argument(p, "--lafmin.sct",
                  help="land area fraction influence in the OI correlation function",
                  type="numeric",
                  default=0.5,
                  short="-lmS")
p <- add_argument(p, "--fast.sct",
                  help="faster spatial consistency test. Allow for flagging more than one observation simulataneously. Station by station mode, some more shortcuts are taken to speed up the elaboration",
                  flag=T,
                  short="-fS")
p <- add_argument(p, "--smartbox.sct",
                  help="use smart boxes in the spatial consistency test for temperature",
                  flag=T)
p <- add_argument(p, "--stn_by_stn.sct",
                  help="spatial consistency test, station by station mode",
                  flag=T)
p <- add_argument(p, "--corr.sct",
                  help="correlation function to use (''gaussian'',''soar''",
                  type="character",
                  default="gaussian")
p <- add_argument(p, "--box_o_nearest_halfwidth.sct",
                  help="half-width of the square box used to select the nearest observations",
                  type="numeric",
                  default=100000)
p <- add_argument(p, "--pmax.sct",
                  help="maximum number of observations to use in the neighbourhood of each observations",
                  type="integer",
                  default=50)
p <- add_argument(p, "--succ_corr.sct",
                  help="successive correction step (yes/no)",
                  flag=T)
p <- add_argument(p, "--o_errvar_min.sct",
                  help="minimum allowed value for the observation error variance",
                  type="numeric",
                  default=0.001)
p <- add_argument(p, "--o_errvar_max.sct",
                  help="maximum allowed value for the observation error variance",
                  type="numeric",
                  default=4)
p <- add_argument(p, "--xa_errvar_min.sct",
                  help="minimum allowed value for the analysis error variance",
                  type="numeric",
                  default=0.001)
p <- add_argument(p, "--xa_errvar_max.sct",
                  help="maximum allowed value for the analysis error variance",
                  type="numeric",
                  default=4)
p <- add_argument(p, "--fglab.sct",
                  help="method used to create the first-guess (\"linear\",\"Frei\",NA used together with usefg(e).sct )",
                  type="character",
                  default="Frei")
p <- add_argument(p, "--fg_gamma.sct",
                  help="lapse rate value",
                  type="numeric",
                  default=-0.0065)
p <- add_argument(p, "--transf.sct",
                  help="apply Box-Cox transformation before SCT (\"stn_by_stn.sct\")",
                  flag=T)
p <- add_argument(p, "--break.sct",
                  help="break the loop if the number of flagged observations in the last iretation (by considering al the test) is euqual to or less than this value.",
                  type="numeric",
                  default=0)
#.............................................................................. 
# observation representativeness
p <- add_argument(p, "--mean.corep",
                  help="average coefficient for the observation representativeness. mean.corep is a vector of positive values (not NAs). If mean.corep has the same length of the number of input files, then a provider dependent value will be used. Otherwise, the value of mean.corep[1] will be used for all providers and any other mean.corep[2:n] value will be ignored",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-avC")
p <- add_argument(p, "--min.corep",
     help="minimum value for the coefficient for the observation representativeness. If min.corep has the same length of the number of input files, then a provider dependent value will be used. Otherwise, the value of min.corep[1] will be used for all providers and any other min.corep[2:n] value will be ignored",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-mnC")
p <- add_argument(p, "--max.corep",
     help="maximum value for the coefficient for the observation representativeness. If max.corep has the same length of the number of input files, then a provider dependent value will be used. Otherwise, the value of max.corep[1] will be used for all providers and any other max.corep[2:n] value will be ignored",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-mxC")
p <- add_argument(p, "--const.corep",
     help="value assigned to the coefficient for the observation representativeness. If const.corep has the same length of the number of input files, then a provider dependent value will be used. Otherwise, the value of const.corep[1] will be used for all providers and any other const.corep[2:n] value will be ignored. If specified, const.corep has priority over the other corep parameters.",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-coC")
#.............................................................................. 
# laf
p <- add_argument(p, "--laf.file",
                  help="land area fraction file (netCDF in kilometric coordinates)",
                  type="character",
                  default=NULL,
                  short="-lfS")
p <- add_argument(p, "--proj4laf",
                  help="proj4 string for the laf",
                  type="character",
                  default=proj4_where_dqc_is_done_default,
                  short="-pl")
p <- add_argument(p, "--laf.varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default="land_area_fraction",
                  short="-lfv")
p <- add_argument(p, "--laf.topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the laf upside down",
                  flag=T,
                  short="-lftd")
p <- add_argument(p, "--laf.ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-lfnd")
p <- add_argument(p, "--laf.tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-lfti")
p <- add_argument(p, "--laf.dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=c("x","y","time"),
                  short="-lfna",
                  nargs=Inf)
p <- add_argument(p, "--laf.proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert",
                  short="-lfp4v")
p <- add_argument(p, "--laf.proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4",
                  short="-lfp4a")
p <- add_argument(p, "--laf.x_as_var.varname",
                  help="easting coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--laf.y_as_var.varname",
                  help="northing coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--laf.xy_as_var.ndim",
                  help="easting/northing coordinates, number of dimensions",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--laf.xy_as_var.tpos",
                  help="easting/northing coordinates, position of time dimension",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--laf.xy_as_var.dimnames",
                  help="easting/northing coordinates, dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
#.............................................................................. 
# check elevation against dem
p <- add_argument(p, "--dem",
     help="check elevation against digital elevation model (dem)",
                  flag=T,
                  short="-dm")
p <- add_argument(p, "--dz.dem",
     help="maximum allowed deviation between observation and dem elevations [m]",
                  type="numeric",
                  default=500,
                  short="-zD")
p <- add_argument(p, "--dem.fill",
                  help="fill missing elevation with data from dem",
                  flag=T,
                  short="-df")
p <- add_argument(p, "--dem.file",
     help="land area fraction file (netCDF in kilometric coordinates)",
                  type="character",
                  default=NULL,
                  short="-dmf")
p <- add_argument(p, "--proj4dem",
                  help="proj4 string for the dem",
                  type="character",
                  default=proj4_where_dqc_is_done_default,
                  short="-pd")
p <- add_argument(p, "--dem.varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default="altitude",
                  short="-dmv")
p <- add_argument(p, "--dem.topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the dem upside down",
                  flag=T,
                  short="-dmtd")
p <- add_argument(p, "--dem.ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-dmnd")
p <- add_argument(p, "--dem.tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-dmti")
p <- add_argument(p, "--dem.dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=c("x","y","time"),
                  short="-dmna",
                  nargs=Inf)
p <- add_argument(p, "--dem.proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert",
                  short="-dmp4v")
p <- add_argument(p, "--dem.proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4",
                  short="-dmp4a")
p <- add_argument(p, "--dem.x_as_var.varname",
                  help="easting coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--dem.y_as_var.varname",
                  help="northing coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--dem.xy_as_var.ndim",
                  help="easting/northing coordinates, number of dimensions",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--dem.xy_as_var.tpos",
                  help="easting/northing coordinates, position of time dimension",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--dem.xy_as_var.dimnames",
                  help="easting/northing coordinates, dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
#.............................................................................. 
# precipitation and temperature cross-check
p <- add_argument(p, "--ccrrt",
           help="precipitation (in-situ) and temperature (field) cross-check",
                  flag=T,
                  short="-ccrrtf")
p <- add_argument(p, "--ccrrt.tmin",
                  help="temperature thresholds (vector, negative values start with \"_\" (e.g. _2=-2)",
                  type="character",
                  default=NA,
                  nargs=Inf,
                  short="-ccrrtt")
p <- add_argument(p,"--ccrrt.filesetup",
                  help="predefined setup to read gridded fields: dem, t2m. available options: meps",
                  type="character",
                  default=NULL,
                  short="-ccrrtfs")
p <- add_argument(p,"--ccrrt.filemeps",
                  help="meps netCDF file",
                  type="character",
                  default=NULL,
                  short="-ccrrtfm")
#.............................................................................. 
# cool check
p <- add_argument(p, "--cool",
                  help="cool (Check fOr hOLes in the field) test",
                  flag=T)
p <- add_argument(p, "--i.cool",
                  help="number of cool-test iterations",
                  type="integer",
                  default=1)
p <- add_argument(p, "--thres.cool",
                  help="numeric vector with the thresholds used to define events (same units of the specified variable). A threshold transforms an observation into a binary event: observation is less than the threshold OR observations is greater or equal to it.",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--condition.cool",
                  help="character vector specifying the conditions to apply at each of the thresholds (\"lt\"=less than; \"le\"=less or equal than; \"gt\"=greater than; \"ge\"=greater or equal than).",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--n.cool",
                  help="minimum acceptable number of observations producing a ''hole'' in the field (specified as a function of the provider and the threshold). If a clump of connected cells originates from a small number of observations, then it cannot be properly resolved by the observational network. As a result, the observations causing those small-scale events are assumed to be affected by large representativeness errors and flagged as ''suspect''. Format: array (ntot[thres1],nprid1[thres1],nprid2[thres1],...,ntot[thres2],nprid1[thres2],nprid2[thres2],...)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--grid_res.cool",
                  help="resolution of the grid used to compute the field (same units as ).",
                  type="integer",
                  default=1000)
p <- add_argument(p, "--dh_max.cool",
                  help="gridpoints having the nearest observation more than dh_max units apart are set to NAs.",
                  type="integer",
                  default=100000)
p <- add_argument(p, "--break.cool",
                  help="break the loop if the number of flagged observations in the last iretation (by considering al the test) is euqual to or less than this value.",
                  type="numeric",
                  default=0)
#.............................................................................. 
# blacklist
# specified by triple/pairs of numbers: either (lat,lon,IDprovider) OR (index,IDprovider)
p <- add_argument(p, "--blacklist.lat",
                  help="observation blacklist (latitude)",
                  type="numeric",default=NA,nargs=Inf,short="-bla")
p <- add_argument(p, "--blacklist.lon",
                  help="observation blacklist (longitude)",
                  type="numeric",default=NA,nargs=Inf,short="-blo")
p <- add_argument(p, "--blacklist.fll",
                  help="observation blacklist (ID provider)",
                  type="numeric",default=NA,nargs=Inf,short="-bfll")
p <- add_argument(p, "--blacklist.idx",
                  help="observation blacklist (position in input file)",
                  type="numeric",default=NA,nargs=Inf,short="-bix")
p <- add_argument(p, "--blacklist.fidx",
                  help="observation blacklist (ID provider)",
                  type="numeric",default=NA,nargs=Inf,short="-bfix")
#.............................................................................. 
# keep (keep them)
# specified by triple/pairs of numbers: either (lat,lon,IDprovider) OR (index,IDprovider)
p <- add_argument(p, "--keeplist.lat",
                  help="observation keeplist (latitude)",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-kla")
p <- add_argument(p, "--keeplist.lon",
                  help="observation keeplist (longitude)",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-klo")
p <- add_argument(p, "--keeplist.fll",
                  help="observation keeplist (ID provider)",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-kfll")
p <- add_argument(p, "--keeplist.idx",
                  help="observation keeplist (position in input file)",
                  type="numeric",
                  default=NA,
                  nargs=Inf, 
                  short="-kix")
p <- add_argument(p, "--keeplist.fidx",
                  help="observation keeplist (ID provider)",
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-kfix")
#.............................................................................. 
# radar-derived precipitation as output
p <- add_argument(p, "--radarout",
                  help="include the radar-derived precipitation as output.",
                  flag=T,
                  short="-rado")
p <- add_argument(p, "--radarout.prid",
                  help="provider identifier for the radar data",
                  type="numeric",
                  default=100,
                  short="-radop")
p <- add_argument(p, "--radarout.aggfact",
                  help="aggregation factor for the radar-derived precipitation",
                  type="numeric",
                  default=3,
                  short="-radop")
p <- add_argument(p, "--radarout.aggfun",
                  help="aggregation function for the radar-derived precipitation",
                  type="character",
                  default="mean")
p <- add_argument(p, "--radarout.corep",
                  help="coefficient for the representativeness of radar-derived precipitation",
                  type="numeric",
                  default=1)
#.............................................................................. 
# doit flags
comstr<-" Decide if the test should be applied to all, none or only to a selection of observations based on the provider. Possible values are 0, 1, 2. It is possible to specify either one global value or one value for each provider. Legend: 1 => the observations will be used in the elaboration and they will be tested; 0 => the observations will not be used and they will not be tested; 2 => the observations will be used but they will not be tested."
p <- add_argument(p, "--doit.buddy",
                  help=paste("customize the buddy-test application.",comstr),
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-dob")
p <- add_argument(p, "--doit.buddy_eve",
                  help=paste("customize the buddy_eve-test application.",comstr),
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-dobe")
p <- add_argument(p, "--doit.sct",
                  help=paste("customize the application of SCT.",comstr),
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-dos")
p <- add_argument(p, "--doit.clim",
                  help=paste("customize the application of the climatological check.",comstr),
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-doc")
p <- add_argument(p, "--doit.dem",
                  help=paste("customize the application of the test of observation elevation against the digital elevation model.",comstr),
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-dod")
p <- add_argument(p, "--doit.isol",
                  help=paste("customize the application of the isolation test.",comstr),
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-doi")
p <- add_argument(p, "--doit.fg",
 help=paste("customize the application of the check against a deterministic first-guess field.",comstr),
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-dofg")
p <- add_argument(p, "--doit.fge",
 help=paste("customize the application of the check against an ensemble of first-guess fields.",comstr),
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-dofge")
p <- add_argument(p, "--doit.cool",
                  help=paste("customize the cool check application.",comstr),
                  type="numeric",
                  default=NA,
                  nargs=Inf,
                  short="-dopud")
#.............................................................................. 
# precipitation correction for the wind-induced undercatch
p <- add_argument(p, "--rr.wcor",
              help=" precipitation correction for the wind-induced undercatch",
                  flag=T)
p <- add_argument(p,"--rr.wcor.filesetup",
                  help="predefined setup to read gridded fields: dem, t2m, wspeed. available options: meps",
                  type="character",
                  default=NULL,
                  short="-rrwf")
p <- add_argument(p,"--rr.wcor.filemeps",
                  help="meps netCDF file",
                  type="character",
                  default=NULL,
                  short="-rrwfm")
# parameter 
p <- add_argument(p, "--rr.wcor.par",
                  help=paste("Parameter used for correcting wind-induced loss",
                              " of solid precipitation (Wolff et al., 2015)"),
                  type="numeric",
                  default=c(4.24,1.81,0.18,0.99,0.66,1.07,0.18,0.11,2.35,0.12),
                  nargs=10,
                  short="-rrwpar")
# temp 
p <- add_argument(p,"--t2m.file",
                  help="air temperature netCDF file",
                  type="character",
                  default=NULL,
                  short="-rrwt")
p <- add_argument(p, "--t2m.offset",
                  help="air temperature offset",
                  type="character",
                  default="0",
                  short="-rrwto")
p <- add_argument(p, "--t2m.cfact",
                  help="air temperature correction factor",
                  type="character",
                  default="1",
                  short="-rrwtc")
p <- add_argument(p, "--t2m.negoffset",
                  help="offset sign (1=neg, 0=pos)",
                  type="character",
                  default="0",
                  short="-rrwtos")
p <- add_argument(p, "--t2m.negcfact",
                  help="correction factor sign (1=neg, 0=pos)",
                  type="character",
                  default="0",
                  short="-rrwton")
p <- add_argument(p, "--proj4t2m",
                  help="proj4 string for the air temperature file",
                  type="character",
                  default=proj4_where_dqc_is_done_default,
                  short="-rrwtp")
p <- add_argument(p, "--t2m.varname",
                  help="air temperature variable name in the netCDF file",
                  type="character",
                  default=NULL,
                  short="-rrwtv")
p <- add_argument(p, "--t2m.topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the file upside down",
                  flag=T,
                  short="-rrwtt")
p <- add_argument(p, "--t2m.ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwtn")
p <- add_argument(p, "--t2m.tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwtti")
p <- add_argument(p, "--t2m.epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwtei")
p <- add_argument(p, "--t2m.dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  short="-rrwtna",
                  nargs=Inf)
p <- add_argument(p, "--t2m.proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert",
                  short="-rrwtp4v")
p <- add_argument(p, "--t2m.proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4",
                  short="-rrwtp4a")
p <- add_argument(p, "--t2m.t",
                  help="timestamp to read in the air temperature netCDF file (YYYYMMDDHH00)",
                  type="character",
                  default=NA,
                  short="-rrwttt")
p <- add_argument(p, "--t2m.e",
                  help="ensemble member to read in the air temperature netCDF file",
                  type="numeric",
                  default=NA,
                  short="-rrwtee")
p <- add_argument(p, "--t2m.x_as_var.varname",
                  help="easting coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--t2m.y_as_var.varname",
                  help="northing coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--t2m.xy_as_var.ndim",
                  help="easting/northing coordinates, number of dimensions",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--t2m.xy_as_var.tpos",
                  help="easting/northing coordinates, position of time dimension",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--t2m.xy_as_var.dimnames",
                  help="easting/northing coordinates, dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
# dem for temperature adjustments
p <- add_argument(p,"--t2m.demfile",
                  help="dem file associated to the first-guess or background file",
                  type="character",
                  default=NULL,
                  short="-rrwdf")
p <- add_argument(p, "--t2m.demoffset",
                  help="offset",
                  type="character",
                  default="0",
                  short="-rrwdoff")
p <- add_argument(p, "--t2m.demcfact",
                  help="correction factor",
                  type="character",
                  default="1",
                  short="-rrwdcf")
p <- add_argument(p, "--t2m.demnegoffset",
                  help="offset sign (1=neg, 0=pos)",
                  type="numeric",
                  default=0,
                  short="-rrwdnoff")
p <- add_argument(p, "--t2m.demnegcfact",
                  help="correction factor sign (1=neg, 0=pos)",
                  type="numeric",
                  default=0,
                  short="-rrwdncf")
p <- add_argument(p, "--t2m.demt",
                  help="timestamp to read in the netCDF file (YYYYMMDDHH00)",
                  type="character",
                  default=NA,
                  short="-rrwdt")
p <- add_argument(p, "--t2m.demvarname",
                  help="variable name in the netCDF file (dem associated to the first-guess)",
                  type="character",
                  default="none",
                  short="-rrwdv")
p <- add_argument(p, "--t2m.demtopdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the field upside down",
                  flag=T,
                  short="-rrwdtd")
p <- add_argument(p, "--t2m.demndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwdnd")
p <- add_argument(p, "--t2m.demepos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwdei")
p <- add_argument(p, "--t2m.demtpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwdti")
p <- add_argument(p, "--t2m.deme",
                  help="ensemble member to read in the netCDF file",
                  type="numeric",
                  default=NA,
                  short="-rrwdee")
p <- add_argument(p, "--t2m.demdimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  short="-rrwdna",
                  nargs=Inf)
# wind
p <- add_argument(p,"--wind.file",
                  help="air temperature netCDF file",
                  type="character",
                  default=NULL,
                  short="-rrww")
p <- add_argument(p, "--proj4wind",
                  help="proj4 string for the air temperature file",
                  type="character",
                  default=proj4_where_dqc_is_done_default,
                  short="-rrwwp")
p <- add_argument(p, "--windspeed.varname",
                  help="air temperature variable name in the netCDF file",
                  type="character",
                  default=NULL,
                  short="-rrwwv")
p <- add_argument(p, "--u.varname",
                  help="air temperature variable name in the netCDF file",
                  type="character",
                  default=NULL,
                  short="-rrwwv")
p <- add_argument(p, "--v.varname",
                  help="air temperature variable name in the netCDF file",
                  type="character",
                  default=NULL,
                  short="-rrwwv")
p <- add_argument(p, "--wind.topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the file upside down",
                  flag=T,
                  short="-rrwwt")
p <- add_argument(p, "--wind.ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwwn")
p <- add_argument(p, "--wind.tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwwti")
p <- add_argument(p, "--wind.epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwwei")
p <- add_argument(p, "--wind.dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  short="-rrwwna",
                  nargs=Inf)
p <- add_argument(p, "--wind.proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert",
                  short="-rrwwp4v")
p <- add_argument(p, "--wind.proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4",
                  short="-rrwwp4a")
p <- add_argument(p, "--wind.t",
                  help="timestamp to read in the air temperature netCDF file (YYYYMMDDHH00)",
                  type="character",
                  default=NA,
                  short="-rrwwtt")
p <- add_argument(p, "--wind.e",
                  help="ensemble member to read in the air temperature netCDF file",
                  type="numeric",
                  default=NA,
                  short="-rrwwee")
p <- add_argument(p, "--wind.x_as_var.varname",
                  help="easting coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--wind.y_as_var.varname",
                  help="northing coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--wind.xy_as_var.ndim",
                  help="easting/northing coordinates, number of dimensions",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--wind.xy_as_var.tpos",
                  help="easting/northing coordinates, position of time dimension",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--wind.xy_as_var.dimnames",
                  help="easting/northing coordinates, dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
#.............................................................................. 
# first-guess or background file / deterministic
p <- add_argument(p, "--fg",
        help="check against a deteministic first-guess (fg) field on a regular grid",
                  flag=T)
p <- add_argument(p, "--thr.fg",
     help="maximum allowed deviation between observation and fg (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrpos.fg",
     help="maximum allowed deviation between observation and fg (if obs>fg) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrneg.fg",
     help="maximum allowed deviation between observation and fg (if obs<fg) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--fg_minval.fg",
     help="do the 'fg' test only when first-guess values are greater than or equal to these thresholds (provider dependent)",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--fg_maxval.fg",
     help="do the 'fg' test only when first-guess values are less than or equal to these thresholds (provider dependent)",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--obs_minval.fg",
     help="do the 'fg' test only when observed values are greater than or equal to these thresholds (provider dependent)",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--obs_maxval.fg",
     help="do the 'fg' test only when observed values are less than or equal to these thresholds (provider dependent)",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrperc.fg",
     help="maximum allowed deviation between observation and fg (as %, e.g. 0.1=10%) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrposperc.fg",
     help="maximum allowed deviation between observation and fg (if obs>fg, as %, e.g. 0.1=10%) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrnegperc.fg",
     help="maximum allowed deviation between observation and fg (if obs<fg, as %, e.g. 0.1=10%) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--fg_minval_perc.fg",
     help="do the 'fg' test (%) only when first-guess values are greater than or equal to these thresholds (provider dependent)",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--fg_maxval_perc.fg",
     help="do the 'fg' test (%) only when first-guess values are less than or equal to these thresholds (provider dependent)",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--obs_minval_perc.fg",
     help="do the 'fg' test (%) only when observed values are greater than or equal to these thresholds (provider dependent)",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--obs_maxval_perc.fg",
     help="do the 'fg' test (%) only when observed values are less than or equal to these thresholds (provider dependent)",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--fg.dodqc",
                  help="check the first-guess field for weird values",
                  type="logical",
                  default=T)
p <- add_argument(p,"--fg.type",
                  help="file type for the first-guess (e.g., meps)",
                  type="character",
                  default=NULL,
                  short="-fgty")
p <- add_argument(p,"--fg.file",
                  help="first-guess or background file",
                  type="character",
                  default=NULL,
                  short="-fgf")
p <- add_argument(p, "--fg.offset",
                  help="offset",
                  type="numeric",
                  default=0,
                  short="-fgoff")
p <- add_argument(p, "--fg.cfact",
                  help="correction factor",
                  type="numeric",
                  default=1,
                  short="-fgcf")
p <- add_argument(p, "--fg.negoffset",
                  help="offset sign (1=neg, 0=pos)",
                  type="numeric",
                  default=0,
                  short="-fgnoff")
p <- add_argument(p, "--fg.negcfact",
                  help="correction factor sign (1=neg, 0=pos)",
                  type="numeric",
                  default=0,
                  short="-fgncf")
p <- add_argument(p, "--proj4fg",
                  help="proj4 string for the first-guess file",
                  type="character",
                  default=proj4_where_dqc_is_done_default,
                  short="-pfg")
p <- add_argument(p, "--usefg.sct",
         help="use the first-guess field provided as the SCT-background",
                  flag=T)
p <- add_argument(p, "--usefg.cool",
         help="use the first-guess field provided in the cool test",
                  flag=T)
p <- add_argument(p, "--fg.varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default="land_area_fraction",
                  short="-fgv")
p <- add_argument(p, "--fg.topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the fg upside down",
                  flag=T,
                  short="-fgtd")
p <- add_argument(p, "--fg.ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgnd")
p <- add_argument(p, "--fg.tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgti")
p <- add_argument(p, "--fg.epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgti")
p <- add_argument(p, "--fg.dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  short="-fgna",
                  nargs=Inf)
p <- add_argument(p, "--fg.proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert",
                  short="-fgp4v")
p <- add_argument(p, "--fg.proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4",
                  short="-fgp4a")
p <- add_argument(p, "--fg.t",
                  help="timestamp to read from the netCDF file (YYYYMMDDHH00)",
                  type="character",
                  default=NA,
                  short="-fgtt")
p <- add_argument(p, "--fg.e",
                  help="ensemble member to read in the netCDF file",
                  type="numeric",
                  default=NA,
                  short="-fgee")
p <- add_argument(p, "--fg.acc",
                  help="first-guess field is accumulated",
                  flag=T,
                  short="-fgacc")
p <- add_argument(p, "--fg.x_as_var.varname",
                  help="easting coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--fg.y_as_var.varname",
                  help="northing coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--fg.xy_as_var.ndim",
                  help="easting/northing coordinates, number of dimensions",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--fg.xy_as_var.tpos",
                  help="easting/northing coordinates, position of time dimension",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--fg.xy_as_var.dimnames",
                  help="easting/northing coordinates, dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p,"--fg.demfile",
                  help="dem file associated to the first-guess or background file",
                  type="character",
                  default=NULL,
                  short="-fgdf")
p <- add_argument(p, "--fg.demoffset",
                  help="offset",
                  type="character",
                  default="0",
                  short="-fgdoff")
p <- add_argument(p, "--fg.demcfact",
                  help="correction factor",
                  type="character",
                  default="1",
                  short="-fgdcf")
p <- add_argument(p, "--fg.demnegoffset",
                  help="offset sign (1=neg, 0=pos)",
                  type="character",
                  default="0",
                  short="-fgdnoff")
p <- add_argument(p, "--fg.demnegcfact",
                  help="correction factor sign (1=neg, 0=pos)",
                  type="character",
                  default="0",
                  short="-fgdncf")
p <- add_argument(p, "--fg.demt",
                  help="timestamp to read in the netCDF file (YYYYMMDDHH00)",
                  type="character",
                  default=NA,
                  short="-fgdt")
p <- add_argument(p, "--fg.demvarname",
                  help="variable name in the netCDF file (dem associated to the first-guess)",
                  type="character",
                  default="none",
                  short="-fgdv")
p <- add_argument(p, "--fg.demtopdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the field upside down",
                  flag=T,
                  short="-fgdtd")
p <- add_argument(p, "--fg.demndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgdnd")
p <- add_argument(p, "--fg.demepos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgdei")
p <- add_argument(p, "--fg.demtpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgdti")
p <- add_argument(p, "--fg.deme",
                  help="ensemble member to read in the netCDF file",
                  type="numeric",
                  default=NA,
                  short="-fgdee")
p <- add_argument(p, "--fg.demdimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  short="-fgdna",
                  nargs=Inf)
#.............................................................................. 
# first-guess or background file / ensemble
p <- add_argument(p, "--fge",
        help="check against an ensemble of first-guess (fge) fields on a regular grid",
                  flag=T)
p <- add_argument(p, "--thr.fge",
     help="maximum allowed deviation between observation and fg (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrpos.fge",
     help="maximum allowed deviation between observation and fg (if obs>fg) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrneg.fge",
     help="maximum allowed deviation between observation and fg (if obs<fg) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--perc.fge_minval",
     help="do the prec.fg test only for values greater than the specified threshold (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrperc.fge",
     help="maximum allowed deviation between observation and fg (as %, e.g. 0.1=10%) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrposperc.fge",
     help="maximum allowed deviation between observation and fg (if obs>fg, as %, e.g. 0.1=10%) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrnegperc.fge",
     help="maximum allowed deviation between observation and fg (if obs<fg, as %, e.g. 0.1=10%) (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--throut.fge",
     help="observation is an outlier if abs(obs-mean_ens)/sd_ens>threshold (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrposout.fge",
     help="same as throut.fge, used only if obs>fg (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--thrnegout.fge",
     help="same as throut.fge, used only if obs<fg (provider dependent)",
                  type="numeric",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p, "--sdmin.fge",
                  help="ensemble standard deviation, minimum value",
                  type="numeric",
                  default=NULL,
                  short="-fgesdmn")
p <- add_argument(p,"--fge.type",
                  help="file type for the ensemble first-guess (e.g., meps)",
                  type="character",
                  default=NULL,
                  short="-fgety")
p <- add_argument(p, "--fge.offset",
                  help="offset",
                  type="numeric",
                  default=0,
                  short="-fgedoff")
p <- add_argument(p, "--fge.cfact",
                  help="correction factor",
                  type="numeric",
                  default=1,
                  short="-fgedcf")
p <- add_argument(p, "--fge.negoffset",
                  help="offset sign (1=neg, 0=pos)",
                  type="numeric",
                  default=0,
                  short="-fgednoff")
p <- add_argument(p, "--fge.negcfact",
                  help="correction factor sign (1=neg, 0=pos)",
                  type="numeric",
                  default=0,
                  short="-fgedncf")
p <- add_argument(p,"--fge.file",
                  help="file with the ensemble members",
                  type="character",
                  default=NULL,
                  short="-fgef")
p <- add_argument(p, "--proj4fge",
                  help="proj4 string for the first-guess file",
                  type="character",
                  default=proj4_where_dqc_is_done_default,
                  short="-pfge")
p <- add_argument(p, "--usefge.sct",
         help="use the ensemble mean as the SCT-background",
                  flag=T)
p <- add_argument(p, "--fge.varname",
                  help="variable name in the netCDF file",
                  type="character",
                  default="land_area_fraction",
                  short="-fgev")
p <- add_argument(p, "--fge.topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the fge upside down",
                  flag=T,
                  short="-fgetd")
p <- add_argument(p, "--fge.acc",
                  help="ensemble first-guess field is accumulated",
                  flag=T,
                  short="-fgacc")
p <- add_argument(p, "--fge.ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgend")
p <- add_argument(p, "--fge.tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgeti")
p <- add_argument(p, "--fge.epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgeti")
p <- add_argument(p, "--fge.dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  short="-fgena",
                  nargs=Inf)
p <- add_argument(p, "--fge.proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert",
                  short="-fgep4v")
p <- add_argument(p, "--fge.proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4",
                  short="-fgep4a")
p <- add_argument(p, "--fge.t",
                  help="timestamp to read in the netCDF file (YYYYMMDDHH00)",
                  type="character",
                  default=NA,
                  short="-fgett")
p <- add_argument(p, "--fge.e",
                  help="ensemble member to read in the netCDF file",
                  type="numeric",
                  default=NA,
                  short="-fgeee")
p <- add_argument(p, "--fge.x_as_var.varname",
                  help="easting coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--fge.y_as_var.varname",
                  help="northing coordinate, variable name (used when proj4 is not specified)",
                  type="character",
                  default=NA)
p <- add_argument(p, "--fge.xy_as_var.ndim",
                  help="easting/northing coordinates, number of dimensions",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--fge.xy_as_var.tpos",
                  help="easting/northing coordinates, position of time dimension",
                  type="numeric",
                  default=NA)
p <- add_argument(p, "--fge.xy_as_var.dimnames",
                  help="easting/northing coordinates, dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  nargs=Inf)
p <- add_argument(p,"--fge.demfile",
                  help="dem file associated to the first-guess or background file",
                  type="character",
                  default=NULL,
                  short="-fgedf")
p <- add_argument(p, "--fge.demoffset",
                  help="offset",
                  type="character",
                  default="0",
                  short="-fgedoff")
p <- add_argument(p, "--fge.demcfact",
                  help="correction factor",
                  type="character",
                  default="1",
                  short="-fgedcf")
p <- add_argument(p, "--fge.demnegoffset",
                  help="offset sign (1=neg, 0=pos)",
                  type="character",
                  default="0",
                  short="-fgednoff")
p <- add_argument(p, "--fge.demnegcfact",
                  help="correction factor sign (1=neg, 0=pos)",
                  type="character",
                  default="0",
                  short="-fgedncf")
p <- add_argument(p, "--fge.demt",
                  help="timestamp to read in the netCDF file (YYYYMMDDHH00)",
                  type="character",
                  default=NA,
                  short="-fgedt")
p <- add_argument(p, "--fge.demvarname",
                  help="variable name in the netCDF file (dem associated to the first-guess)",
                  type="character",
                  default="none",
                  short="-fgedv")
p <- add_argument(p, "--fge.demtopdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the field upside down",
                  flag=T,
                  short="-fgedtd")
p <- add_argument(p, "--fge.demndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgednd")
p <- add_argument(p, "--fge.demepos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgedei")
p <- add_argument(p, "--fge.demtpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-fgedti")
p <- add_argument(p, "--fge.deme",
                  help="ensemble member to read in the netCDF file",
                  type="numeric",
                  default=NA,
                  short="-fgedee")
p <- add_argument(p, "--fge.demdimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  short="-fgedna",
                  nargs=Inf)
#.............................................................................. 
# Timestamp valid for all the netcdf files
p <- add_argument(p, "--timestamp",
                  help="timestamp, valid for all the netCDF file (YYYYMMDDHH00)",
                  type="character",
                  default=NA,
                  short="-t")
#.............................................................................. 
# run on several cores 
p <- add_argument(p, "--cores",
                  help="set the number of cores for parallel runs. Rpackage \"parallel\" required. 0 stands for \"use detectCores\". Default do not use it.",
                  type="numeric",
                  default=NA)
#.............................................................................. 
# PARSE arguments
argv <- parse_args(p)
#
#-----------------------------------------------------------------------------
# read configuration file
if (!is.na(argv$config.file)) {
  if (file.exists(argv$config.file)) {
    source(argv$config.file)
    argv_tmp<-append(argv,conf)
    names_argv_tmp<-names(argv_tmp)
    argv_def<-list()
    names_argv_def<-integer(0)
    k<-0
    for (i in 1:length(argv_tmp)) {
      if (names_argv_tmp[i] %in% names_argv_def) next
      k<-k+1
      j<-which(names_argv_tmp==names_argv_tmp[i])
      argv_def[[k]]<-argv_tmp[[j[length(j)]]]
      names_argv_def<-c(names_argv_def,names_argv_tmp[i])
    }
    names(argv_def)<-names_argv_def
    rm(argv_tmp,names_argv_tmp,names_argv_def)
    rm(argv)
    argv<-argv_def
    rm(argv_def)
  } else {
    print("WARNING: config file not found")
    print(argv$config.file)
  }
}

