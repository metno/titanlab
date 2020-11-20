#+
synsct_argparser <- function() {
# Command line arguments
p <- arg_parser("synsct_rr")
#------------------------------------------------------------------------------
p<- add_argument(p, "--ffin_fields",
                 help="full file name for the input file with the fields (nc)",
                 type="character",
                 default="in.nc")
p<- add_argument(p, "--ffin_obs",
                 help="full file name for the input file with the observations (txt)",
                 type="character",
                 default="in.txt")
p<- add_argument(p, "--ffin_sim",
                 help="full file name for the input file with the simulation of temperature (dat)",
                 type="character",
                 default="in.dat")
p<- add_argument(p, "--ffout",
                 help="full file name for the output file",
                 type="character",
                 default="out.dat")
p<- add_argument(p, "--config_file",
                 help="full file name for the configuration file",
                 type="character",
                 default=NA)
#..............................................................................
p<- add_argument(p, "--background_elab_type",
                 help="background elaboration type",
                 type="character",
                 default="mean_outer_circle")
p<- add_argument(p, "--undef",
                 help="undefined value",
                 type="numeric",
                 default=-9999)
p<- add_argument(p, "--boxcox_lambda",
                 help="Box-Cox transformation parameter",
                 type="numeric",
                 default=NA)
p<- add_argument(p, "--tpos_score",
                 help="sct threshold when obs > cv-analysis",
                 type="numeric",
                 default=3)
p<- add_argument(p, "--tneg_score",
                 help="sct threshold when obs < cv-analysis",
                 type="numeric",
                 default=3)
p<- add_argument(p, "--t_sod",
                 help="sct spatial outlier detection threshold",
                 type="numeric",
                 default=2)
p<- add_argument(p, "--eps2",
                 help="relative precision obs/backg",
                 type="numeric",
                 default=0.5)
p<- add_argument(p, "--inner_radius",
                 help="radius of the inner circle",
                 type="numeric",
                 default=30000)
p<- add_argument(p, "--outer_radius",
                 help="radius of the outer circle",
                 type="numeric",
                 default=50000)
p<- add_argument(p, "--kth_closest_obs_horizontal_scale",
                 help="kth_closest_obs_horizontal_scale, what else?",
                 type="integer",
                 default=3)
#..............................................................................
p<- add_argument(p, "--value_min",
                 help="minimum allowed value",
                 type="numeric",
                 default=0)
p<- add_argument(p, "--value_max",
                 help="maximum allowed value",
                 type="numeric",
                 default=350)
p<- add_argument(p, "--sig2o_min",
                 help="minimum allowed value for observation error variance",
                 type="numeric",
                 default=0)
p<- add_argument(p, "--sig2o_max",
                 help="maximum allowed value for observation error variance",
                 type="numeric",
                 default=10)
p<- add_argument(p, "--vertical_scale",
                 help="vertical scale (set it to very high if you do not need it)",
                 type="numeric",
                 default=6000)
p<- add_argument(p, "--num_min",
                 help="minimum number of observations",
                 type="integer",
                 default=3)
p<- add_argument(p, "--num_max",
                 help="minimum number of observations",
                 type="integer",
                 default=20)
p<- add_argument(p, "--num_iterations",
                 help="number of sct iterations",
                 type="integer",
                 default=20)
p<- add_argument(p, "--num_min_prof",
                 help="minimum number of observations needed to compute vert prof",
                 type="integer",
                 default=10)
p<- add_argument(p, "--min_elev_diff",
                 help="minimum elevation difference to compute a complex vertical profile of temperature",
                 type="numeric",
                 default=500)
#..............................................................................
p<- add_argument(p, "--pGE",
                 help="probability of gross measurement error (0-100)",
                 type="numeric",
                 default=NA)
p<- add_argument(p, "--thinobs_perc",
                 help="percentage of observation to remove (0-100)",
                 type="numeric",
                 default=NA)
p<- add_argument(p, "--thinobs_prid",
                 help="provider identifier to remove",
                 type="numeric",
                 default=NA)
p<- add_argument(p, "--a_vertprof_ix",
                 help="which a? 1-41, 1<-a=-20, ..., 41<-a=20",
                 type="numeric",
                 default=1)
p<- add_argument(p, "--synsct_tg_nens",
                 help="number of ensembles",
                 type="numeric",
                 default=10)
p<- add_argument(p, "--synsct_rr_nens",
                 help="number of ensembles",
                 type="numeric",
                 default=10)
#..............................................................................
p<- add_argument(p, "--t_score_eva",
                 help="list of thresholds for evaluation",
                 type="character",
                 nargs=Inf,
                 default=NA)
p<- add_argument(p, "--t_sod_eva",
                 help="list of thresholds for evaluation",
                 type="character",
                 nargs=Inf,
                 default=NA)
#..............................................................................
p <- add_argument(p, "--gridded_extent",
                  help="xmin xmax ymin ymax",
                  type="numeric",
                  nargs=Inf,
                  default=c(-621692.2, -246692.2, -499072, 928))
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
p <- add_argument(p, "--rr_lscale",
                  help="precipitation simulations. Length scale (same units as the spatial coordinates)",
                  type="numeric",
                  default=50)
#..............................................................................
p <- add_argument(p, "--verbose",
                  help="verbose mode",
                  flag=T)
p <- add_argument(p, "--debug",
                  help="debug mode",
                  flag=T)
#..............................................................................
argv <- parse_args(p)
#
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
argv
}
