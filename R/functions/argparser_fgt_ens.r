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
