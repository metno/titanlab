# CHECKS on input arguments
if (!file.exists(argv$input)) 
  boom(paste("ERROR: input file not found",argv$input))
# more than one input file
if (any(!is.na(argv$input.files))) {
  for (j in 1:length(argv$input.files)) {
    if (!file.exists(argv$input.files[j])) 
      boom(paste("ERROR: input file not found",argv$input.files[j]))
  }
  argv$input.files<-c(argv$input,argv$input.files)
} else {
  argv$input.files<-argv$input
}
nfin<-length(argv$input.files)
# check consistency between number of files and provider ids
if (any(is.na(argv$prid))) {
  argv$prid<-1:nfin
} else {
  if (length(argv$prid)!=nfin) 
    boom("ERROR: number of provider identifier is different from the number of input files")
}
#................................................................................
# set input offsets and correction factors
argv$input.offset<-strings_to_numbers(strings=argv$input.offset, 
                                              default=0,
                                              strings_dim=nfin,
                                              neg=argv$input.negoffset)
argv$input.cfact<-strings_to_numbers(strings=argv$input.cfact,
                                             default=1,
                                             strings_dim=nfin,
                                             neg=argv$input.negcfact)
#t2m
t2m.offset<-strings_to_numbers(strings=argv$t2m.offset,default=0,
                                       neg=argv$t2m.negoffset)
t2m.cfact<-strings_to_numbers(strings=argv$t2m.cfact,default=0,
                                      neg=argv$t2m.negcfact)
t2m.demoffset<-strings_to_numbers(strings=argv$t2m.demoffset,default=0,
                                          neg=argv$t2m.demnegoffset)
t2m.demcfact<-strings_to_numbers(strings=argv$t2m.demcfact,default=0,
                                         neg=argv$t2m.demnegcfact)
#fg
fg.offset<-strings_to_numbers(strings=argv$fg.offset,default=0,
                                      neg=argv$fg.negoffset)
fg.cfact<-strings_to_numbers(strings=argv$fg.cfact,default=0,
                                     neg=argv$fg.negcfact)
fg.demoffset<-strings_to_numbers(strings=argv$fg.demoffset,default=0,
                                         neg=argv$fg.demnegoffset)
fg.demcfact<-strings_to_numbers(strings=argv$fg.demcfact,default=0,
                                        neg=argv$fg.demnegcfact)
argv$fg_minval.fg<-strings_to_numbers(strings=argv$fg_minval.fg,
                                              strings_dim=nfin)
argv$fg_maxval.fg<-strings_to_numbers(strings=argv$fg_maxval.fg,
                                              strings_dim=nfin)
argv$obs_minval.fg<-strings_to_numbers(strings=argv$obs_minval.fg,
                                               strings_dim=nfin)
argv$obs_maxval.fg<-strings_to_numbers(strings=argv$obs_maxval.fg,
                                               strings_dim=nfin)
argv$fg_minval_perc.fg<-strings_to_numbers(strings=argv$fg_minval_perc.fg,
                                                   strings_dim=nfin)
argv$fg_maxval_perc.fg<-strings_to_numbers(strings=argv$fg_maxval_perc.fg,
                                                   strings_dim=nfin)
argv$obs_minval_perc.fg<-strings_to_numbers(strings=argv$obs_minval_perc.fg,
                                                    strings_dim=nfin)
argv$obs_maxval_perc.fg<-strings_to_numbers(strings=argv$obs_maxval_perc.fg,
                                                    strings_dim=nfin)
# fge
fge.offset<-strings_to_numbers(strings=argv$fge.offset,default=0,
                                       neg=argv$fge.negoffset)
fge.cfact<-strings_to_numbers(strings=argv$fge.cfact,default=0,
                                      neg=argv$fge.negcfact)
fge.demoffset<-strings_to_numbers(strings=argv$fge.demoffset,default=0,
                                          neg=argv$fge.demnegoffset)
fge.demcfact<-strings_to_numbers(strings=argv$fge.demcfact,default=0,
                                         neg=argv$fge.demnegcfact)
#................................................................................
# check variable
if (!(argv$variable %in% c("T","RH","RR","SD"))) 
  boom("variable must be one of T, RH, RR, SD")
# set proj4 variables (proj4from and proj4to are obsolete)
if (argv$proj4from!=argv$proj4_input_obsfiles) {
  if (argv$proj4_input_obsfiles==proj4_input_obsfiles_default & 
      argv$proj4from!=proj4_input_obsfiles_default)
    argv$proj4_input_obsfiles<-argv$proj4from
}
if (argv$proj4to!=argv$proj4_where_dqc_is_done) {
  if (argv$proj4_where_dqc_is_done==proj4_where_dqc_is_done_default & 
      argv$proj4to!=proj4_where_dqc_is_done_default)
    argv$proj4_where_dqc_is_done<-argv$proj4to
}
#................................................................................
# set variables to customize output
if (argv$varname.lat.out!=argv$varname.y.out) {
  if (argv$varname.y.out==varname.y.out_default & 
      argv$varname.lat.out!=varname.y.out_default)
    argv$varname.y.out<-argv$varname.lat.out
}
if (argv$varname.lon.out!=argv$varname.x.out) {
  if (argv$varname.x.out==varname.x.out_default & 
      argv$varname.lon.out!=varname.x.out_default)
    argv$varname.x.out<-argv$varname.lon.out
}
if (argv$latlon.dig.out!=argv$xy.dig.out) {
  if (argv$xy.dig.out==xy.dig.out_default & 
      argv$latlon.dig.out!=xy.dig.out_default)
    argv$xy.dig.out<-argv$latlon.dig.out
}
#................................................................................
# set the input arguments according to user specification
if (argv$variable=="RR") {
  argv$transf.buddy<-T
  argv$transf.sct<-T
}
if (!is.na(argv$fg.type)) {
  if (argv$fg.type=="meps") {
    if (argv$variable=="T") {
      argv$fg.epos<-5
      argv$fg.e<-0
      argv$fg.varname<-"air_temperature_2m"
      argv$fg.ndim<-5 
      argv$fg.tpos<-3
      if (any(is.na(argv$fg.dimnames))) {
        argv$fg.dimnames<-c("x","y","time","height1","ensemble_member")
      } else {
        argv$fg.ndim<-length(argv$fg.dimnames)
      }
      argv$proj4fg<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
      argv$fg.offset<-273.15
      argv$fg.negoffset<-1
      argv$fg.demvarname<-"surface_geopotential" 
      argv$fg.demndim<-5
      argv$fg.demtpos<-3
      argv$fg.demepos<-5
      argv$fg.deme<-0
      argv$fg.demdimnames<-c("x","y","time","height0","ensemble_member")
      # divide geopotential by g=9.80665. This calculates geopotential height (above mean sea level)
      argv$fg.demcfact<-0.0980665 
      argv$fg.topdown<-TRUE
      argv$fg.demtopdown<-TRUE
    } else if (argv$variable=="RR") {
      argv$fg.epos<-5
      argv$fg.e<-0
      argv$fg.varname<-"precipitation_amount_acc"
      argv$fg.ndim<-5 
      argv$fg.tpos<-3
      if (any(is.na(argv$fg.dimnames))) {
        argv$fg.dimnames<-c("x","y","time","height0","ensemble_member")
      } else {
        argv$fg.ndim<-length(argv$fg.dimnames)
      }
      argv$proj4fg<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
      argv$fg.acc<-TRUE
      argv$fg.topdown<-TRUE
    } else if (argv$variable=="RH") {
      argv$fg.epos<-5
      argv$fg.e<-0
      argv$fg.varname<-"relative_humidity_2m"
      argv$fg.ndim<-5 
      argv$fg.tpos<-3
      if (any(is.na(argv$fg.dimnames))) {
        argv$fg.dimnames<-c("x","y","time","height1","ensemble_member")
      } else {
        argv$fg.ndim<-length(argv$fg.dimnames)
      }
      argv$proj4fg<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
      argv$fg.cfact<-100.
      argv$fg.topdown<-TRUE
    } else {
      boom("ERROR in --fg.type, combination of type/variable not available")
    } 
  } else if (argv$fg.type=="radar") {
    if (argv$variable=="RR") {
      argv$fg.epos<-NA
      argv$fg.e<-NULL
      argv$fg.varname<-"lwe_precipitation_rate"
      argv$fg.ndim<-3 
      argv$fg.tpos<-3
      if (any(is.na(argv$fg.dimnames))) {
        argv$fg.dimnames<-c("Xc","Yc","time")
      } else {
        argv$fg.ndim<-length(argv$fg.dimnames)
      }
      argv$proj4fg<-"+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
      argv$fg.topdown<-FALSE
    } else {
      boom("ERROR in --fg.type, combination of type/variable not available")
    }
  } else if (argv$fg.type=="surfex_T") {
    if (argv$variable=="RH") {
      argv$fg.epos<-NA
      argv$fg.e<-NULL
      argv$fg.varname<-"relative_humidity_2m"
      argv$fg.ndim<-3 
      argv$fg.tpos<-3
      argv$fg.dimnames<-c("x","y","time")
      argv$proj4fg<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
      argv$fg.topdown<-TRUE
    } else if (argv$variable=="T") {
      argv$fg.epos<-NA
      argv$fg.e<-NULL
      argv$fg.varname<-"air_temperature_2m"
      argv$fg.ndim<-3 
      argv$fg.tpos<-3
      argv$fg.dimnames<-c("x","y","time")
      argv$proj4fg<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
      argv$fg.topdown<-TRUE
    } else if (argv$variable=="SD") {
      argv$fg.epos<-NA
      argv$fg.e<-NULL
      argv$fg.varname<-"surface_snow_thickness"
      argv$fg.ndim<-3 
      argv$fg.tpos<-3
      argv$fg.dimnames<-c("x","y","time")
      argv$proj4fg<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
      argv$fg.topdown<-TRUE
    } else {
      boom("ERROR in --fg.type, combination of type/variable not available")
    }
  } else {
    boom("ERROR in --fg.type, type not recognized")
  }
}
if (!is.na(argv$fge.type)) {
  if (argv$fge.type=="meps") {
    if (argv$variable=="T") {
      argv$fge.epos<-5
      argv$fge.varname<-"air_temperature_2m"
      argv$fge.ndim<-5 
      argv$fge.tpos<-3
      if (is.na(argv$fge.dimnames)) {
        argv$fge.dimnames<-c("x","y","time","height1","ensemble_member")
      } else {
        argv$fge.ndim<-length(argv$fge.dimnames)
      }
      argv$proj4fge<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
      argv$fge.offset<-273.15
      argv$fge.negoffset<-1
      argv$fge.demvarname<-"surface_geopotential" 
      argv$fge.demndim<-5
      argv$fge.demtpos<-3
      argv$fge.demepos<-5
      argv$fge.deme<-0
      if (is.na(argv$fge.demdimnames)) {
        argv$fge.demdimnames<-c("x","y","time","height0","ensemble_member")
      } else {
        argv$fge.demndim<-length(argv$fge.demdimnames)
      }
      # divide geopotential by g=9.80665. This calculates geopotential height (above mean sea level)
      argv$fge.demcfact<-0.0980665 
      argv$fge.topdown<-TRUE
      argv$fge.demtopdown<-TRUE
    } else if (argv$variable=="RR") {
      argv$fge.epos<-5
      argv$fge.varname<-"precipitation_amount_acc"
      argv$fge.ndim<-5 
      argv$fge.tpos<-3
      if (is.na(argv$fge.dimnames)) {
        argv$fge.dimnames<-c("x","y","time","height0","ensemble_member")
      } else {
        argv$fge.ndim<-length(argv$fge.dimnames)
      }
      argv$proj4fge<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
      argv$fge.acc<-TRUE
      argv$fge.topdown<-TRUE
    } else if (argv$variable=="RH") {
      argv$fge.epos<-5
      argv$fge.varname<-"relative_humidity_2m"
      argv$fge.ndim<-5 
      argv$fge.tpos<-3
      if (is.na(argv$fge.dimnames)) {
        argv$fge.dimnames<-c("x","y","time","height1","ensemble_member")
      } else {
        argv$fge.ndim<-length(argv$fge.dimnames)
      }
      argv$proj4fge<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
      argv$fge.cfact<-100.
      argv$fge.topdown<-TRUE
    } else {
      boom("ERROR in --fge.type, combination of type/variable not available")
    } 
  } else {
    boom("ERROR in --fge.type, type not recognized")
  }
}
# shortcut for meps file in the precip correction for wind undercatch
if (!is.na(argv$rr.wcor.filesetup)) {
  if (argv$rr.wcor.filesetup=="meps") {
    argv$t2m.file<-argv$rr.wcor.filemeps
    argv$t2m.epos<-5
    argv$t2m.e<-0
    argv$t2m.varname<-"air_temperature_2m"
    argv$t2m.ndim<-5 
    argv$t2m.tpos<-3
    argv$t2m.dimnames<-c("x","y","time","height1","ensemble_member")
    argv$proj4t2m<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
    argv$t2m.offset<-273.15
    argv$t2m.negoffset<-1
    argv$t2m.demfile<-argv$rr.wcor.filemeps
    argv$t2m.demvarname<-"surface_geopotential" 
    argv$t2m.demndim<-5
    argv$t2m.demtpos<-3
    argv$t2m.demepos<-5
    argv$t2m.deme<-0
    argv$t2m.demdimnames<-c("x","y","time","height0","ensemble_member")
    # divide geopotential by g=9.80665. This calculates geopotential height (above mean sea level)
    argv$t2m.demcfact<-0.0980665 
    argv$t2m.topdown<-TRUE
    argv$t2m.demtopdown<-TRUE
    argv$wind.file<-argv$rr.wcor.filemeps
    argv$wind.epos<-5
    argv$wind.e<-0
    argv$u.varname<-"x_wind_10m" 
    argv$v.varname<-"y_wind_10m" 
    argv$wind.ndim<-5 
    argv$wind.tpos<-3
    argv$wind.dimnames<-c("x","y","time","height3","ensemble_member")
    argv$proj4wind<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
    argv$wind.topdown<-TRUE
  } else {
    boom("ERROR --rr.wcor.filesetup argument not recognized")
  }
}
# shortcut for meps file in the precip-temp cross-check
if (!is.na(argv$ccrrt.filesetup)) {
  if (argv$ccrrt.filesetup=="meps") {
    argv$t2m.file<-argv$ccrrt.filemeps
    argv$t2m.epos<-5
    argv$t2m.e<-0
    argv$t2m.varname<-"air_temperature_2m"
    argv$t2m.ndim<-5 
    argv$t2m.tpos<-3
    argv$t2m.dimnames<-c("x","y","time","height1","ensemble_member")
    argv$proj4t2m<-"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
    argv$t2m.offset<-273.15
    argv$t2m.negoffset<-1
    argv$t2m.demfile<-argv$ccrrt.filemeps
    argv$t2m.demvarname<-"surface_geopotential" 
    argv$t2m.demndim<-5
    argv$t2m.demtpos<-3
    argv$t2m.demepos<-5
    argv$t2m.deme<-0
    argv$t2m.demdimnames<-c("x","y","time","height0","ensemble_member")
    # divide geopotential by g=9.80665. This calculates geopotential height (above mean sea level)
    argv$t2m.demcfact<-0.0980665 
    argv$t2m.topdown<-TRUE
    argv$t2m.demtopdown<-TRUE
  } else {
    boom("ERROR --ccrrt.filesetup argument not recognized")
  }
}
# check external files
if (argv$dem | argv$dem.fill) {
  if (!file.exists(argv$dem.file)) 
    boom(paste("ERROR: dem file not found",argv$dem.file))
}
if (argv$laf.sct) {
  if (!file.exists(argv$laf.file)) 
    boom(paste("ERROR: laf file not found",argv$laf.file))
}
if (!is.na(argv$fg.file)) {
  if (!file.exists(argv$fg.file)) 
    boom(paste("ERROR: first-guess file not found",argv$fg.file))
}
# input column names
if (any(is.na(argv$separator))) 
  argv$separator<-rep(";",nfin)
if (length(argv$separator)==0) 
  argv$separator<-rep(";",nfin)
if (length(argv$separator)!=nfin) 
  argv$separator<-rep(argv$separator[1],nfin)
for (i in 1:length(argv$separator)) if (argv$separator[i]=="comma")  argv$separator[i]<-","
if (any(is.na(argv$varname.lat))) 
  argv$varname.lat<-rep("lat",nfin)
if (length(argv$varname.lat)==0) 
  argv$varname.lat<-rep("lat",nfin)
if (length(argv$varname.lat)!=nfin) 
  argv$varname.lat<-rep(argv$varname.lat[1],nfin)
if (any(is.na(argv$varname.lon))) 
  argv$varname.lon<-rep("lon",nfin)
if (length(argv$varname.lon)==0) 
  argv$varname.lon<-rep("lon",nfin)
if (length(argv$varname.lon)!=nfin) 
  argv$varname.lon<-rep(argv$varname.lon[1],nfin)
if (any(is.na(argv$varname.elev))) 
  argv$varname.elev<-rep("elev",nfin)
if (length(argv$varname.elev)==0) 
  argv$varname.elev<-rep("elev",nfin)
if (length(argv$varname.elev)!=nfin) 
  argv$varname.elev<-rep(argv$varname.elev[1],nfin)
if (any(is.na(argv$varname.value))) 
  argv$varname.value<-rep("value",nfin)
if (length(argv$varname.value)==0) 
  argv$varname.value<-rep("value",nfin)
if (length(argv$varname.value)!=nfin) 
  argv$varname.value<-rep(argv$varname.value[1],nfin)
# check for spatial conversion flag
if (!argv$spatconv) 
  boom(paste("ERROR: \"--spatconv\" (-c) option must be used on the command line. Default is: input is in lat-lon coordinates; some DQC tests takes place in kilometric coordinates specified by the user; output is in lat-lon coordinates"))
# load netcdf library, if needed
if (argv$laf.sct | argv$dem | argv$dem.fill |
    !is.na(argv$fg.file) | !is.na(argv$fge.file) |
    !is.na(argv$t2m.file))
  suppressPackageStartupMessages(library("ncdf4")) 
# first-guess file
if (!is.na(argv$fg.file)) {
  if ( argv$variable=="T" &
       !file.exists(argv$fg.demfile)) 
    boom("ERROR: for temperature, a digital elevation model must be specified together with a first-guess file (det)")
} else if (argv$fg) {
  boom("ERROR no first-guess file provided for the first-guess test")
} else if (argv$usefg.sct) {
  boom("ERROR: SCT requested with the background derived from a first-guess field that is not being provided as input")
}
#
if (!is.na(argv$month.clim) & (argv$month.clim<1 | argv$month.clim>12)) {
  boom(paste("ERROR: month number is wrong. month number=",argv$month.clim))
} else if (!is.na(argv$month.clim) & 
           (length(which(!is.na(argv$vmin.clim)))!=12 | 
            length(which(!is.na(argv$vmax.clim)))!=12) ) {
  boom("ERROR: climatological check, vmin.clim and/or vmax.clim vectors must have 12 arguments")
}
# blacklist
if (any(!is.na(argv$blacklist.lat)) | 
    any(!is.na(argv$blacklist.lon)) |
    any(!is.na(argv$blacklist.fll)) ) {
  if ( (length(argv$blacklist.lat)!=length(argv$blacklist.lon))  |
       (length(argv$blacklist.lat)!=length(argv$blacklist.fll))  |
       (any(is.na(argv$blacklist.fll))) | 
       (any(is.na(argv$blacklist.lat))) | 
       (any(is.na(argv$blacklist.lon))) ) {
    boom(paste("ERROR in the blacklist definition, must have same number of lat,lon,IDprovider points. lat number=",argv$blacklist.lat,". lon number=",argv$blacklist.lon,". ID provider number=",argv$blacklist.fll))
  }
}
if (any(!is.na(argv$blacklist.idx)) | 
    any(!is.na(argv$blacklist.fidx)) ) {
  if ( (length(argv$blacklist.idx)!=length(argv$blacklist.fidx))  |
       (any(is.na(argv$blacklist.idx))) | 
       (any(is.na(argv$blacklist.fidx))) ) {
    print("ERROR in the blacklist definition, must have same number of index and IDprovider points")
    print(paste("index number=",argv$blacklist.idx))
    print(paste("ID provider number=",argv$blacklist.fidx))
    boom()
  }
}
# keeplist
if (any(!is.na(argv$keeplist.lat)) | 
    any(!is.na(argv$keeplist.lon)) |
    any(!is.na(argv$keeplist.fll)) ) {
  if ( (length(argv$keeplist.lat)!=length(argv$keeplist.lon))  |
       (length(argv$keeplist.lat)!=length(argv$keeplist.fll))  |
       (any(is.na(argv$keeplist.fll))) | 
       (any(is.na(argv$keeplist.lat))) | 
       (any(is.na(argv$keeplist.lon))) ) {
    print("ERROR in the keeplist definition, must have same number of lat,lon,IDprovider points")
    print(paste("lat number=",argv$keeplist.lat))
    print(paste("lon number=",argv$keeplist.lon))
    print(paste("ID provider number=",argv$keeplist.fll))
    boom()
  }
}
if (any(!is.na(argv$keeplist.idx)) | 
    any(!is.na(argv$keeplist.fidx)) ) {
  if ( (length(argv$keeplist.idx)!=length(argv$keeplist.fidx))  |
       (any(is.na(argv$keeplist.idx))) | 
       (any(is.na(argv$keeplist.fidx))) ) {
    print("ERROR in the keeplist definition, must have same number of index and IDprovider points")
    print(paste("index number=",argv$keeplist.idx))
    print(paste("ID provider number=",argv$keeplist.fidx))
    boom()
  }
}
#
# observation representativeness
if ( (any(is.na(argv$mean.corep)) | 
      any(is.na(argv$min.corep))  | 
      any(is.na(argv$max.corep)) ) & 
      any(is.na(argv$const.corep)) ) {
  print("++WARNING")
  print("parameters related to the coefficient of observation representativeness are not properly specified")
  print("--mean.corep --min.corep and --max.corep or --const.corep should be specified")
  print("Because they are not specified, it is assumed that the coefficient of observation representativeness is not considered an interesting output. As a  consequence, the corep parameters are set to default values (min.corep=0.9 mean.corep=1 max.corep=1.1")
  argv$min.corep<-0.9
  argv$mean.corep<-1
  argv$max.corep<-1.1
}
if (length(argv$min.corep)!=nfin) 
  argv$min.corep<-rep(argv$min.corep[1],length=nfin)
if (length(argv$mean.corep)!=nfin) 
  argv$mean.corep<-rep(argv$mean.corep[1],length=nfin)
if (length(argv$max.corep)!=nfin) 
  argv$max.corep<-rep(argv$max.corep[1],length=nfin)
if (length(argv$const.corep)!=nfin) 
  argv$const.corep<-rep(argv$const.corep[1],length=nfin)
#
# precip and temperature crosscheck
argv$ccrrt.tmin<-strings_to_numbers(strings=argv$ccrrt.tmin,
                                            strings_dim=nfin)
#
# fg
if (argv$fg) {
  if (length(argv$thrpos.fg)!=nfin) 
    argv$thrpos.fg<-rep(argv$thrpos.fg[1],length=nfin)
  if (length(argv$thrneg.fg)!=nfin)
    argv$thrneg.fg<-rep(argv$thrneg.fg[1],length=nfin)
  if (length(argv$thr.fg)!=nfin)
    argv$thr.fg<-rep(argv$thr.fg[1],length=nfin)
  if (length(argv$thrposperc.fg)!=nfin) 
    argv$thrposperc.fg<-rep(argv$thrposperc.fg[1],length=nfin)
  if (length(argv$thrnegperc.fg)!=nfin)
    argv$thrnegperc.fg<-rep(argv$thrnegperc.fg[1],length=nfin)
  if (length(argv$thrperc.fg)!=nfin)
    argv$thrperc.fg<-rep(argv$thrperc.fg[1],length=nfin)
  if ( !any(!is.na(c(argv$thrpos.fg,argv$thrneg.fg,argv$thr.fg,
                     argv$thrposperc.fg,argv$thrnegperc.fg,argv$thrperc.fg))))
    boom("Error in specification of fg-thresholds")
}
#
# fge
if (argv$fge) {
  if (length(argv$thrpos.fge)!=nfin) 
    argv$thrpos.fge<-rep(argv$thrpos.fge[1],length=nfin)
  if (length(argv$thrneg.fge)!=nfin)
    argv$thrneg.fge<-rep(argv$thrneg.fge[1],length=nfin)
  if (length(argv$thr.fge)!=nfin) 
    argv$thr.fge<-rep(argv$thr.fge[1],length=nfin)
  if (length(argv$thrposperc.fge)!=nfin) 
    argv$thrposperc.fge<-rep(argv$thrposperc.fge[1],length=nfin)
  if (length(argv$thrnegperc.fge)!=nfin)
    argv$thrnegperc.fge<-rep(argv$thrnegperc.fge[1],length=nfin)
  if (length(argv$thrperc.fge)!=nfin) 
    argv$thrperc.fge<-rep(argv$thrperc.fge[1],length=nfin)
  if (length(argv$perc.fge_minval)!=nfin) 
    argv$perc.fge_minval<-rep(argv$perc.fge_minval[1],length=nfin)
  if (length(argv$thrposout.fge)!=nfin) 
    argv$thrposout.fge<-rep(argv$thrposout.fge[1],length=nfin)
  if (length(argv$thrnegout.fge)!=nfin)
    argv$thrnegout.fge<-rep(argv$thrnegout.fge[1],length=nfin)
  if (length(argv$throut.fge)!=nfin) 
    argv$throut.fge<-rep(argv$throut.fge[1],length=nfin)
  if ( !any(!is.na(argv$thrpos.fge)) &
       !any(!is.na(argv$thrneg.fge)) &
       !any(!is.na(argv$thr.fge)) &
       !any(!is.na(argv$thrposperc.fge)) &
       !any(!is.na(argv$thrnegperc.fge)) &
       !any(!is.na(argv$thrperc.fge)) &
       !any(!is.na(argv$perc.fge_minval)) &
       !any(!is.na(argv$thrposout.fge)) &
       !any(!is.na(argv$thrnegout.fge)) &
       !any(!is.na(argv$throut.fge)) ) {
    boom("Error in specification of fge-thresholds")
  }
}
#
# SCT
# if defined, thrpos.sct and thrneg.sct have the priority on thr.sct
if ( (any(is.na(argv$thrpos.sct)) & any(!is.na(argv$thrpos.sct))) |
     (any(is.na(argv$thrneg.sct)) & any(!is.na(argv$thrneg.sct))) ) {
  print("SCT thresholds for positive and negative deviations are not properly specified")
  print(paste("threshold(s) when (Obs-CVpred) <0 (thrneg.sct)",argv$thrneg.sct))
  print(paste("threshold(s) when (Obs-CVpred)>=0 (thrpos.sct)",argv$thrpos.sct))
  boom()
}
if (length(argv$thrpos.sct)!=nfin) {
  argv$thrpos.sct<-rep(argv$thrpos.sct[1],length=nfin)
}
if (length(argv$thrneg.sct)!=nfin) {
  argv$thrneg.sct<-rep(argv$thrneg.sct[1],length=nfin)
}
if ( any(is.na(argv$thr.sct)) & 
     is.na(argv$thrneg.sct[1]) ) {
  print("++ WARNING")
  print("thr.sct should be specified and it must not contain NAs")
  print(" because either it has not been specified or it has been set to NA, ")
  print(" then TITAN will use the default value of 16")
  argv$thr.sct<-vector();argv$thr.sct[1]<-16
}
if (length(argv$thr.sct)!=nfin) 
  argv$thr.sct<-rep(argv$thr.sct[1],length=nfin)
#
# eps2
if (any(is.na(argv$eps2.sct))) {
  print("++ WARNING")
  print("eps2.sct should be specified and it must not contain NAs")
  print(" because either it has not been specified or it has been set to NA, ")
  print(" then TITAN will use the default value of 0.5")
  argv$eps2.sct<-vector();argv$eps2.sct[1]<-0.5
}
if (length(argv$eps2.sct)!=nfin) 
  argv$eps2.sct<-rep(argv$eps2.sct[1],length=nfin)
#
# cool test
if (argv$cool) {
  if (any(is.na(argv$thres.cool))) {
    print("++ WARNING")
    print("COOL test has NAs among the specified thresholds")
    print(" TITAN will use only the default threshold of 0.1")
    argv$thres.cool<-vector();argv$thres.cool[1]<-0.1
  }
  if ( any( is.na(argv$condition.cool) | 
       !(argv$condition.cool %in% c("lt","le","gt","ge")) )) {
    print("++ WARNING")
    print("COOL test has NAs and/or not allowed strings among the specified conditions")
    print(" TITAN will use only the default condition \"lt\"")
    argv$condition.cool<-vector();argv$condition.cool[1]<-"lt"
  }
  if (length(argv$condition.cool)!=length(argv$thres.cool)) {
    print("++ WARNING")
    print("COOL test, different lengths for vectors thres.cool and condition.cool")
    print(" TITAN will use the default condition \"lt\" for all thresholds")
    argv$condition.cool<-vector(mode="character",length=length(argv$thres.cool))
    argv$condition.cool[]<-"lt"
  }
  n.cool<-array(data=NA,dim=c(length(argv$thres.cool),(nfin+1))) 
  for (i in 1:length(argv$thres.cool)) {
    for (j in 1:(nfin+1)) { 
      n.cool[i,j]<-argv$n.cool[((i-1)*(nfin+1)+j)]
    }
  }
  if (any(is.na(n.cool))) {
    print("++ ERROR")
    print("COOL test. something wrong in the specification of the n.cool argument")
    print(n.cool)
    boom()
  }
}
#
# doit flags
if (any(is.na(argv$doit.buddy))) argv$doit.buddy<-rep(1,length=nfin)
if (any(is.na(argv$doit.buddy_eve))) argv$doit.buddy_eve<-rep(1,length=nfin)
if (any(is.na(argv$doit.sct))) argv$doit.sct<-rep(1,length=nfin)
if (any(is.na(argv$doit.clim))) argv$doit.clim<-rep(1,length=nfin)
if (any(is.na(argv$doit.dem))) argv$doit.dem<-rep(1,length=nfin)
if (any(is.na(argv$doit.isol))) argv$doit.isol<-rep(1,length=nfin)
if (any(is.na(argv$doit.fg))) argv$doit.fg<-rep(1,length=nfin)
if (any(is.na(argv$doit.fge))) argv$doit.fge<-rep(1,length=nfin)
if (any(is.na(argv$doit.cool))) argv$doit.cool<-rep(1,length=nfin)
if (any(!(argv$doit.buddy %in% c(0,1,2)))) 
  boom("doit.buddy must contain only 0,1,2")
if (any(!(argv$doit.buddy_eve %in% c(0,1,2))))
  boom("doit.buddy_eve must contain only 0,1,2")
if (any(!(argv$doit.sct %in% c(0,1,2))))
  boom("doit.sct must contain only 0,1,2")
if (any(!(argv$doit.clim %in% c(0,1,2))))
  boom("doit.clim must contain only 0,1,2")
if (any(!(argv$doit.dem %in% c(0,1,2))))
  boom("doit.dem must contain only 0,1,2")
if (any(!(argv$doit.isol %in% c(0,1,2))))
  boom("doit.isol must contain only 0,1,2")
if (any(!(argv$doit.fg %in% c(0,1,2))))
  boom("doit.fg must contain only 0,1,2")
if (any(!(argv$doit.cool %in% c(0,1,2))))
  boom("doit.cool must contain only 0,1,2")
#
# set the thresholds for the plausibility check
if (!is.na(argv$tmin) & is.na(argv$vmin)) argv$vmin<-argv$tmin
if (!is.na(argv$tmax) & is.na(argv$vmax)) argv$vmax<-argv$tmax
if (any(is.na(argv$vmin.clim)) & !any(is.na(argv$tmin.clim))) 
  argv$vmin.clim<-argv$tmin.clim
if (any(is.na(argv$vmax.clim)) & !any(is.na(argv$tmax.clim))) 
  argv$vmax.clim<-argv$tmax.clim
argv$vmin<-strings_to_numbers(strings=argv$vmin,neg=argv$vminsign)
argv$vmax<-strings_to_numbers(strings=argv$vmax,neg=argv$vmaxsign)
argv$vmin.clim<-strings_to_numbers(strings=argv$vmin.clim,
                                        strings_dim=12, neg=argv$vminsign.clim)
argv$vmax.clim<-strings_to_numbers(strings=argv$vmax.clim,
                                        strings_dim=12, neg=argv$vmaxsign.clim)
# buddy priorities
if (is.null(argv$prio.buddy)) argv$prio.buddy<-rep(-1,length=nfin)
if (any(is.na(argv$prio.buddy))) argv$prio.buddy<-rep(-1,length=nfin)
if (length(argv$prio.buddy)!=nfin) argv$prio.buddy<-rep(-1,length=nfin)
if (is.null(argv$prio.buddy_eve)) argv$prio.buddy_eve<-rep(-1,length=nfin)
if (any(is.na(argv$prio.buddy_eve))) argv$prio.buddy_eve<-rep(-1,length=nfin)
if (length(argv$prio.buddy_eve)!=nfin) argv$prio.buddy_eve<-rep(-1,length=nfin)
# SCT with smart boxes
if (argv$smartbox.sct & argv$variable == "T") {
  if (!file.exists(file.path(argv$titan_path,"sct","sct_smart_boxes.so")))
    boom(paste("ERROR: file not found.",argv$titan_path,"sct","sct_smart_boxes.so"))
  dyn.load(file.path(argv$titan_path,"sct","sct_smart_boxes.so"))
}
# buddy checks
if (!any(!is.na(argv$dr.buddy))) argv$dr.buddy<-3000
if (length(argv$thr.buddy)!=length(argv$dr.buddy))
  argv$thr.buddy<-rep(0.05,length=argv$dr.buddy)
if (any(is.na(argv$n.buddy)))
  argv$n.buddy<-rep(5,length=argv$dr.buddy)
if (any(is.na(argv$dz.buddy)))
  argv$dz.buddy<-rep(10000,length=argv$dr.buddy)
# buddy_eve checks
if (argv$buddy_eve) {
  if (length(argv$dr.buddy_eve)!=length(argv$thr_eve.buddy_eve))
    argv$dr.buddy_eve<-rep(3000,length=argv$thr_eve.buddy_eve)
  if (length(argv$thr.buddy_eve)!=length(argv$thr_eve.buddy_eve))
    argv$thr.buddy_eve<-rep(0.05,length=argv$thr_eve.buddy_eve)
  if (any(is.na(argv$n.buddy_eve)))
    argv$n.buddy_eve<-rep(5,length=argv$thr_eve.buddy_eve)
  if (any(is.na(argv$dz.buddy_eve)))
    argv$dz.buddy_eve<-rep(10000,length=argv$thr_eve.buddy_eve)
}
# wind-induced undercatch of precipitation, check consistency of inputs
if (argv$rr.wcor & argv$variable!="RR") 
  boom("ERROR: wind-induced correction for undercatch is implemented for precipitation only")
# proj4s
if (argv$dem | argv$dem.fill) {
  if (argv$proj4dem=="" & argv$dem.proj4_var=="" & argv$dem.proj4_att=="" ) {
    dem.xy_as_vars<-T
    proj4dem<-NULL
    proj4dem_from_nc<-NULL
  } else {
    dem.xy_as_vars<-F
    proj4dem<-argv$proj4dem
    proj4dem_from_nc<-list(var=argv$dem.proj4_var, att=argv$dem.proj4_att)
  }
}
if (!is.na(argv$t2m.file)) {
  if (argv$proj4t2m=="" & argv$t2m.proj4_var=="" & argv$t2m.proj4_att=="" ) {
    t2m.xy_as_vars<-T
    proj4t2m<-NULL
    proj4t2m_from_nc<-NULL
  } else {
    t2m.xy_as_vars<-F
    proj4t2m<-argv$proj4t2m
    proj4t2m_from_nc<-list(var=argv$t2m.proj4_var, att=argv$t2m.proj4_att)
  }
}
#if (!is.na(argv$wind.file)) {
#  if (argv$proj4wind=="" & argv$wind.proj4_var=="" & argv$wind.proj4_att=="" ) {
#    wind.xy_as_vars<-T
#    proj4wind<-NULL
#    proj4wind_from_nc<-NULL
#  } else {
#    wind.xy_as_vars<-F
#    proj4wind<-argv$proj4wind
#    proj4wind_from_nc<-list(var=argv$wind.proj4_var, att=argv$wind.proj4_att)
#  }
#}
if (!is.na(argv$fg.file)) {
  if (argv$proj4fg=="" & argv$fg.proj4_var=="" & argv$fg.proj4_att=="" ) {
    fg.xy_as_vars<-T
    proj4fg<-NULL
    proj4fg_from_nc<-NULL
  } else {
    fg.xy_as_vars<-F
    proj4fg<-argv$proj4fg
    proj4fg_from_nc<-list(var=argv$fg.proj4_var, att=argv$fg.proj4_att)
  }
}
if (!is.na(argv$fge.file)) {
  if (argv$proj4fge=="" & argv$fge.proj4_var=="" & argv$fge.proj4_att=="" ) {
    fge.xy_as_vars<-T
    proj4fge<-NULL
    proj4fge_from_nc<-NULL
  } else {
    fge.xy_as_vars<-F
    proj4fge<-argv$proj4fge
    proj4fge_from_nc<-list(var=argv$fge.proj4_var, att=argv$fge.proj4_att)
  }
}
# set the timestamp
if (!is.na(argv$timestamp)) {
  if (is.na(argv$fg.t)) argv$fg.t<-argv$timestamp
  if (is.na(argv$fge.t)) argv$fge.t<-argv$timestamp
  if (is.na(argv$wind.t)) argv$wind.t<-argv$timestamp
  if (is.na(argv$t2m.t)) argv$t2m.t<-argv$timestamp
}

