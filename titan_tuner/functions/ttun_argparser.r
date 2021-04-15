#+
ttun_argparser<-function() {
#------------------------------------------------------------------------------
p <- arg_parser("ttun")
#..............................................................................
p <- add_argument(p, "date1",
                  help="period start date (if \"none\" then date1 and date2 are derived from file)",
                  type="character")

p <- add_argument(p, "--date2",
                  help="period end date",
                  type="character",
                  default="none")

p <- add_argument(p, "--date.format",
                  help="format of the date/time",
                  type="character",
                  default="%Y-%m-%dT%H")

p <- add_argument(p, "--ffin_date.format",
                  help="format of the input date/time",
                  type="character",
                  default="%Y-%m-%dT%H")

p <- add_argument(p, "--ffout_date.format",
                  help="format of the output date/time",
                  type="character",
                  default="%Y-%m-%dT%H")

p <- add_argument(p, "--ffin_date.file",
                  help="input date/time provided in a text file (rows with \"ffin_date.format\")",
                  type="character",
                  default=NA)

p <- add_argument(p, "--name_lat",
                  help="label for latitudes in input file",
                  type="character",
                  default="lat")

p <- add_argument(p, "--name_lon",
                  help="label for longitudes in input file",
                  type="character",
                  default="lon")

p <- add_argument(p, "--name_z",
                  help="label for elevations in input file",
                  type="character",
                  default="elev")

p <- add_argument(p, "--name_val",
                  help="label for values in input file",
                  type="character",
                  default="value")

p <- add_argument(p, "--name_prid",
                  help="label for provider identifiers in input file",
                  type="character",
                  default="prid")

p <- add_argument(p, "--name_cn",
                  help="label for countries in input file",
                  type="character",
                  default="cn")

#..............................................................................

p <- add_argument(p, "--time_step",
                  help="time step",
                  type="numeric",
                  default=1)

p <- add_argument(p, "--time_unit",
                  help="time unit",
                  type="character",
                  default="hours")

p <- add_argument(p, "--time_n_prev",
                  help="number of previous time steps",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--time_n_succ",
                  help="number of successive time steps",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--date_filter_by_month",
                  help="month(s) to consider, within the time period chosen",
                  type="numeric",
                  default=NA,
                  nargs=Inf)

p <- add_argument(p, "--one_timestep_for_file",
                  help="read the first timestep from each file",
                  flag=T)

#..............................................................................
# IO
# input file(s)

p<- add_argument(p, "--ffin_template",
                 help="path to + name (template) of the input observation files",
                 type="character",
                 default="none")

p<- add_argument(p, "--titanlib_path",
                 help="where is the titanlib.R file?",
                 type="character",
                 default=".")

p <- add_argument(p, "--config_file",
                  help="configuration file",
                  type="character",
                  default=NULL)

p <- add_argument(p, "--separator",
                  help="input file. separator",
                  type="character",
                  default=";")

# output file

p<- add_argument(p, "--ffout",
                 help="output file",
                 type="character",
                 default="out.txt")

p<- add_argument(p, "--ffout_png1",
                 help="output file (path+part of the name) for png1. Used only in debug mode. Note that the name is automatically completed with date/time and the extension .png",
                 type="character",
                 default="out_fig1")

p<- add_argument(p, "--ffout_png2",
                 help="output file (path+part of the name) for png1. Used only in debug mode. Note that the name is automatically completed with date/time and the extension .png",
                 type="character",
                 default="out_fig2")

p<- add_argument(p, "--year_string",
                 help="string, placeholder for year",
                 type="character",
                 default="%Y")
p<- add_argument(p, "--month_string",
                 help="string, placeholder for month",
                 type="character",
                 default="%m")
p<- add_argument(p, "--day_string",
                 help="string, placeholder for day",
                 type="character",
                 default="%d")
p<- add_argument(p, "--hour_string",
                 help="string, placeholder for hour",
                 type="character",
                 default="%H")
p<- add_argument(p, "--min_string",
                 help="string, placeholder for minute",
                 type="character",
                 default="%M")
p<- add_argument(p, "--sec_string",
                 help="string, placeholder for second",
                 type="character",
                 default="%S")

#..............................................................................
# 

p<- add_argument(p, "--variable",
                 help="",
                 type="character",
                 default="T")

p<- add_argument(p, "--num_min_prof.sct",
                 help="",
                 type="integer",
                 default=10)

p<- add_argument(p, "--min_elev_diff.sct",
                 help="",
                 type="numeric",
                 default=100)

p<- add_argument(p, "--min_horizontal_scale.sct",
                 help="",
                 type="numeric",
                 default=1000)

p<- add_argument(p, "--max_horizontal_scale.sct",
                 help="",
                 type="numeric",
                 default=100000)

p<- add_argument(p, "--eps2.sct",
                 help="",
                 type="numeric",
                 default=0.5)

p<- add_argument(p, "--num_min_outer.sct",
                 help="minimum number of observations that need to be present in the outer circle",
                 type="integer",
                 default=5)

p<- add_argument(p, "--boxcox.lambda",
                 help="Box-Cox transformation parameter",
                 type="numeric",
                 default=0.5)

p<- add_argument(p, "--PGE",
                 help="prior probability of gross error (0-100)",
                 type="numeric",
                 default=5)

p<- add_argument(p, "--PGE_prid",
                 help="define providers that have the observations perturbed with GE",
                 type="numeric",
                 default=NA,
                 nargs=Inf)

#..............................................................................

p<- add_argument(p, "--ci",
                 help="constraint vector (1st=thr, 2nd=kth, 3rd=inner_radius(m), 4th=outer_radius(m), 5th=num_max_outer, 6th=vertical_scale(m)). The optimized value of a parameter is forced to be greater than the specified value. More details in constrOptim, see ci: constaint vector.",
                 type="numeric",
                 default=c( 0.5, 2, 1000, 1000, 10, 100),
                 nargs=6)

p<- add_argument(p, "--theta0",
                 help="initialization vector (1st=thr, 2nd=kth, 3rd=inner_radius(m), 4th=outer_radius(m), 5th=num_max_outer, 6th=vertical_scale(m)). Initial value for each parameter. More details in constrOptim help.",
                 type="numeric",
                 default=c( 3, 4, 10000, 100000, 50, 250),
                 nargs=6)

p<- add_argument(p, "--theta",
                 help="parameter vector (1st=thr, 2nd=kth, 3rd=inner_radius(m), 4th=outer_radius(m), 5th=num_max_outer, 6th=vertical_scale(m)). No optimization required, just testing.",
                 type="numeric",
                 default=NA,
                 nargs=6)

p<- add_argument(p, "--parscale",
                 help="a vector of scaling values for the parameters (1st=thr, 2nd=kth, 3rd=inner_radius(m), 4th=outer_radius(m), 5th=num_max_outer, 6th=vertical_scale(m)). More details in optim help.",
                 type="numeric",
                 default=c( 1, 10, 10000, 10000, 10, 100),
                 nargs=6)

#..............................................................................

p<- add_argument(p, "--Ta_delta",
                 help="temperature. Range of admissible values is plus/minus delta",
                 type="numeric",
                 default=10)

p<- add_argument(p, "--Tv_delta",
                 help="temperature. Range of valid values is plus/minus delta",
                 type="numeric",
                 default=0.05)

p<- add_argument(p, "--RRa_delta",
                 help="temperature. Range of admissible values is plus/minus delta",
                 type="numeric",
                 default=10)

p<- add_argument(p, "--RRv_delta",
                 help="temperature. Range of valid values is plus/minus delta",
                 type="numeric",
                 default=0.05)

p<- add_argument(p, "--RRa_fact",
                 help="temperature. Range of admissible values is plus/minus delta",
                 type="numeric",
                 default=10)

p<- add_argument(p, "--RRv_fact",
                 help="temperature. Range of valid values is plus/minus delta",
                 type="numeric",
                 default=0.05)



#..............................................................................

p<- add_argument(p, "--ext",
                 help="extension of the domain considered ( lon_min, lon_max, lat_min, lat_max)",
                 type="numeric",
                 default=NA,
                 nargs=Inf)

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
#..............................................................................

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
