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
