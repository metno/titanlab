
argparser_titanlab <- function() {

  p <- arg_parser("test")

# ----------------------------------------------------------------------  

  p <- add_argument(p, "--date1",
                    help = "period start date",
                    type = "character")
  p <- add_argument(p, "--date2",
                    help = "period end date",
                    type = "character")
  p <- add_argument(p, "--time_step",
                    help="time step for time sequence",
                    type="numeric",
                    default=1)
  p <- add_argument(p, "--time_unit",
                    help="time unit for time sequence",
                    type="character",
                    default="hours")

# ----------------------------------------------------------------------  

p <- add_argument(p, "--config_file",
                  help="configuration file",
                  type="character",
                  default=NULL)
p <- add_argument(p, "--config_string",
                  help="short configuration info. USed in name outputfile",
                  type="character",
                  default=NULL)

# ----------------------------------------------------------------------
# output file
  p<- add_argument(p, "--output_file_path",
                   help="output file path",
                   type="character",
                   default="output_file")


# ----------------------------------------------------------------------


p<- add_argument(p, "--input_file_path",
                 help="General path input files",
                 type="character",
                 default="none")

p <- add_argument(p, "--input_varname",
                  help="variable name input file(s). RR/TAM/TAN/TAX",
                  type="character",
                  default=NA)
# ---------------------------------------------------------------

p <- add_argument(p, "--background_values",
                  help="background_values",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--background_elab_type",
                  help="background_elab_type",
                  type="character",
                  default=NA)

# ---------------------------------------------------------------
p <- add_argument(p, "--num_min",
                  help="num_min",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--num_max",
                  help="num_max",
                  type="numeric",
                  default=NA)

# ---------------------------------------------------------------
p <- add_argument(p, "--inner_radius",
                  help="inner_radius",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--outer_radius",
                  help="outer_radius",
                  type="numeric",
                  default=NA)
# ---------------------------------------------------------------

p <- add_argument(p, "--num_iterations",
                  help="num_iterations",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--num_min_prof",
                  help="num_min_prof",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--min_elev_diff",
                  help="min_elev_diff",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--min_horizontal_scale",
                  help="min_horizontal_scale",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--max_horizontal_scale",
                  help="max_horizontal_scale",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--kth_closest_obs_horizontal_scale",
                  help="kth_closest_obs_horizontal_scale",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--vertical_scale",
                  help="vertical_scale",
                  type="numeric",
                  default=NA)


# ---------------------------------------------------------------

p <- add_argument(p, "--tpos_score",
                  help="positive threshold sct",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--tneg_score",
                  help="negative threshold sct",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--t_sod",
                  help="t_sod",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--eps2",
                  help="eps2",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--sig2o_min",
                  help="sig2o_min",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--sig2o_max",
                  help="sig2o_max",
                  type="numeric",
                  default=NA)


# ---------------------------------------------------------------


p <- add_argument(p, "--value_min",
                  help="value_min",
                  type="numeric",
                  default=NA)

p <- add_argument(p, "--value_max",
                  help="value_max",
                  type="numeric",
                  default=NA)


# ---------------------------------------------------------------



p <- add_argument(p, "--debug",
                  help="debugging",
                  type="logical",
                  default=FALSE)



# ---------------------------------------------------------------

argv <- parse_args(p)


# read configuration file
if (!is.na(argv$config_file)) {
  if (file.exists(argv$config_file)) {
    source(argv$config_file)
    argv_tmp <- append(argv, conf) # add list in config file to argv
    names_argv_tmp <- names(argv_tmp)
    values_argv_new <- list() 
    names_argv_new <- integer(0)
    k <- 0
    for (i in 1:length(argv_tmp)) { 
      if (names_argv_tmp[i] %in% names_argv_new) next 
      k <- k + 1
      j <- which(names_argv_tmp == names_argv_tmp[i]) # find index all instances of current element in list argv_tmp  
      values_argv_new[[k]] <- argv_tmp[[j[length(j)]]]  # find the last instances of that variable and give the value of the variable to values_argv_new
      names_argv_new <- c(names_argv_new, names_argv_tmp[i]) # add to new vector for names (then will only have unique)
    }
    names(values_argv_new) <- names_argv_new # distribute the correct variable names to the values
    rm(argv)
    argv <- values_argv_new # updated argv
    rm(values_argv_new, names_argv_new, argv_tmp, names_argv_tmp)
  } else {
    print("WARNING: config file not found")
    print(argv$config_file)
  }
}


# print(argv)
return(argv)
}



