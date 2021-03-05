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
