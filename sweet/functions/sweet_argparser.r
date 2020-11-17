#+
argparser<-function() {
#------------------------------------------------------------------------------
p <- arg_parser("sweet")
#------------------------------------------------------------------------------
p<- add_argument(p, "--ffin_obs",
                 help="full file name for the observation input file",
                 type="character",
                 default="in.txt")
p<- add_argument(p, "--ffout",
                 help="full file name for the output file",
                 type="character",
                 default="out.txt")
p<- add_argument(p, "--config_file",
                 help="full file name for the configuration file",
                 type="character",
                 default=NA)
p <- add_argument(p, "--cores",
                  help="set the number of cores for parallel runs. Rpackage \"parallel\" required. 0 stands for \"use detectCores\". Default do not use it.",
                  type="numeric",
                  default=NA)
#..............................................................................
p<- add_argument(p, "--ffout_gridtype",
                 help="output grid type",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varname",
                 help="output varname(s)",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varlongname",
                 help="attribute of each variable with the long-varname(s)",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varstandardname",
                 help="attribute of each variable with the standard varname(s)",
                 type="character",
                 default="none")
p<- add_argument(p, "--ffout_varversion",
                 help="attribute of each variable with the var version(s)",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_varunit",
                 help="attribute of each variable with the var unit(s)",
                 type="character",
                 default="")
p <- add_argument(p, "--ffout_cell_methods",
                  help="attribute of each variable with the aggregation methods (e.g. ''time: sum'')",
                  type="character",
                  default=NA,
                  nargs=Inf)
p<- add_argument(p, "--ffout_times_unit",
                 help="output var version",
                 type="character",
                 default="H")
p<- add_argument(p, "--ffout_reference",
                 help="global attribute with dataset reference",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_proj4",
                 help="output ''proj4'' strings",
                 type="character",
                 default="")
p <- add_argument(p, "--ffout_lonlat",
                  help="logical, lon lat in the output",
                  flag=T)
p <- add_argument(p, "--ffout_diground",
                  help="rounding digits",
                  type="numeric",
                  default=2)
p<- add_argument(p, "--ffout_summary",
                 help="output var unit",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_sourcestring",
                 help="output",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_title",
                 help="output",
                 type="character",
                 default="")
p<- add_argument(p, "--ffout_comment",
                 help="output",
                 type="character",
                 default="")
#..............................................................................
p <- add_argument(p, "--sweet_t",
                  help="simulate temperature data",
                  flag=T)
p <- add_argument(p, "--t_lscale",
                  help="temperature simulations. Length scale (same units as the spatial coordinates)",
                  type="numeric",
                  default=50)
p <- add_argument(p, "--t_t0_mean",
                  help="temperature simulations. Frei's vertical profile parameter \"T0\": mean temperature at the z=0m surface (Celsius degrees)",
                  type="numeric",
                  default=20)
p <- add_argument(p, "--t_t0_stdev",
                  help="temperature simulations. \"T0\" standard deviation (Celsius degrees). Used to scale the covariance matrix in the spatial simulations.",
                  type="numeric",
                  default=5)
p <- add_argument(p, "--t_gamma_mean",
                  help="temperature simulations. Frei's vertical profile parameter \"gamma\": temperature lapse rate (Celsius degrees / meter)",
                  type="numeric",
                  default=-0.0065)
p <- add_argument(p, "--t_gamma_stdev",
                  help="temperature simulations. \"gamma\" standard deviation (Celsius degrees / meter). Used to scale the covariance matrix in the spatial simulations.",
                  type="numeric",
                  default=0.00001)
p <- add_argument(p, "--t_a_mean",
                  help="temperature simulations. Frei's vertical profile parameter \"a\": strenght of temperature inversion (Celsius degrees)",
                  type="numeric",
                  default=10)
p <- add_argument(p, "--t_a_stdev",
                  help="temperature simulations. \"a\" standard deviation (Celsius degrees). Used to scale the covariance matrix in the spatial simulations.",
                  type="numeric",
                  default=2)
p <- add_argument(p, "--t_a_min",
                  help="temperature simulations. Frei's vertical profile parameter \"a\": strenght of temperature inversion (Celsius degrees)",
                  type="numeric",
                  default=-10)
p <- add_argument(p, "--t_a_max",
                  help="temperature simulations. Frei's vertical profile parameter \"a\": strenght of temperature inversion (Celsius degrees)",
                  type="numeric",
                  default=10)
p <- add_argument(p, "--t_h0_mean",
                  help="temperature simulations. Frei's vertical profile parameter \"h0\": height above sea level where the temperature inversion begins (metres above sea level). Can be negative.",
                  type="numeric",
                  default=500)
p <- add_argument(p, "--t_h0_stdev",
                  help="temperature simulations. \"h0\" standard deviation (metres above sea level). Used to scale the covariance matrix in the spatial simulations.",
                  type="numeric",
                  default=10)
p <- add_argument(p, "--t_h1i_mean",
                  help="temperature simulations. Frei's vertical profile parameter \"h1i\": height spanned by the temperature inversion (metres). Only positive values allowed",
                  type="numeric",
                  default=100)
p <- add_argument(p, "--t_h1i_stdev",
                  help="temperature simulations. \"h1i\" standard deviation (metres). Used to scale the covariance matrix in the spatial simulations.",
                  type="numeric",
                  default=10)
#..............................................................................
p <- add_argument(p, "--sweet_rr",
                  help="simulate precipitation data",
                  flag=T)
p <- add_argument(p, "--nsamples",
                  help="number of samples",
                  type="numeric",
                  default=1)
p <- add_argument(p, "--rr_lscale",
                  help="precipitation simulations. Length scale (same units as the spatial coordinates)",
                  type="numeric",
                  default=50)
p <- add_argument(p, "--rr_gamma_shape",
                  help="precipitation simulations. Gaussian anamorphosis. Gamma distribution parameter \"shape\"",
                  type="numeric",
                  default=0.2)
p <- add_argument(p, "--rr_gamma_rate",
                  help="precipitation simulations. Gaussian anamorphosis. Gamma distribution parameter \"rate\"",
                  type="numeric",
                  default=0.1)
#..............................................................................
p <- add_argument(p, "--add_ges_unif",
                  help="add gross measurement errors (GEs) to data. Draw random values from a uniform distribution.",
                  flag=T)
p <- add_argument(p, "--ges_unif_fract",
                  help="fraction of GES with respect to all available points (0-1)",
                  type="numeric",
                  default=0.01)
p <- add_argument(p, "--ges_unif_min",
                  help="minimum value for observations with GE.",
                  type="numeric",
                  default=-100)
p <- add_argument(p, "--ges_unif_max",
                  help="maximum value for observations with GE.",
                  type="numeric",
                  default=100)
#..............................................................................
p <- add_argument(p, "--add_ges_repr",
                  help="add gross measurement errors (GEs) to data. Small-scale spatially correlated noise.",
                  flag=T)
p <- add_argument(p, "--ges_repr_stdev",
                  help="standard deviation of the covariance matrix used for the simulation of the gaussian random field. Same value for all the diagonal elements.",
                  type="numeric",
                  default=1)
p <- add_argument(p, "--ges_repr_lscale",
                  help="length scale of the covariance matrix used for the simulation of the gaussian random field (same units as the spatial coordinates)",
                  type="numeric",
                  default=1)
#..............................................................................
p <- add_argument(p, "--gridded",
                  help="create fields on a grid",
                  flag=T)
p <- add_argument(p, "--gridded_extent",
                  help="xmin xmax ymin ymax",
                  type="numeric",
                  nargs=Inf,
                  default=c(-621692.2, -249192.2, -501572, 928))
p <- add_argument(p, "--extent",
                  help="xmin xmax ymin ymax",
                  type="numeric",
                  nargs=Inf,
                  default=c(61.9, 62.3, 9.8, 10.2))
p <- add_argument(p, "--extent_small",
                  help="xmin xmax ymin ymax",
                  type="numeric",
                  nargs=Inf,
                  default=c(61.9, 62.3, 9.8, 10.2))
p <- add_argument(p, "--gridded_res",
                  help="dx dy",
                  type="numeric",
                  nargs=Inf,
                  default=c(12500,12500))
p <- add_argument(p, "--gridded_crs",
                  help="coordinate reference system",
                  type="character",
                  default="+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06")
#..............................................................................
p <- add_argument(p, "--verbose",
                  help="verbose mode",
                  flag=T)
p <- add_argument(p, "--debug",
                  help="debug mode",
                  flag=T)
#..............................................................................
argv <- parse_args(p)
# Command line arguments, parameter defaults - @@END@@
#-----------------------------------------------------------------------------
# read configuration file
if (!is.na(argv$config_file)) {
  if (file.exists(argv$config_file)) {
    source(argv$config_file)
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
    print(argv$config_file)
  }
}
#
return(argv)
}
