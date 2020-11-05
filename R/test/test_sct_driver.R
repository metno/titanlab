#!/usr/bin/env Rscript

#---------------------------------------------------------------

# titanlib_path <- "/home/cristianl/projects/titanlib/build/extras"
# titanlib_path <- "/home/lineb/projects/titanlib/titanlib/build/extras"

# dyn.load(file.path(titanlib_path, paste("SWIG/R/titanlib", .Platform$dynlib.ext, sep = "")))
# source(  file.path(titanlib_path, "SWIG/R/titanlib.R"))

# ---------------------------------------------------------------

suppressPackageStartupMessages(library("argparser"))
suppressPackageStartupMessages(library("lubridate"))

source("argparser_titanlab.R")

# ---------------------------------------------------------------

t0 <- Sys.time()

# Read command line arguments (and config file)
argv <- argparser_titanlab()

# Create time sequence daily files between date1 and date2
tseq <- seq(ymd(argv$date1), ymd(argv$date2), by = paste(argv$time_step, argv$time_unit))
n_tseq <- length(tseq)

# Loop over time sequence. For each time step: read file, run SCT, write new file
for (t in 1:n_tseq) { 

  print(paste("timestep", t, "/", n_tseq, "elapsed time", round(Sys.time() - t0, 2), attr(Sys.time() - t0, "unit")))
  print(tseq[t])

  curr_file=paste0(argv$input_file_path, format(tseq[t], "%Y"), "/", format(tseq[t], "%m"), "/obs_", argv$input_varname, "_", format(tseq[t], "%Y%m%d"), ".txt")
  print(paste0("Reading file: ", curr_file))
  obs <- read.csv(curr_file, sep=';')
  print(head(obs))

  values <- obs$value
  lats   <- obs$lat 
  lons   <- obs$lon 
  elevs  <- obs$z 

  N <- length(values)

  obs_to_check <- rep(0,N)
  obs_to_check[which(obs$prid == 1)] <- 1 # check all obs with provider id 1
 
  background_values <- rep(1,N) * argv$background_values 
  background_elab_type <- argv$background_elab_type

  num_min <- argv$num_min 
  num_max <- argv$num_max 

  inner_radius <- argv$inner_radius
  outer_radius <- argv$outer_radius
 
  num_iterations <- argv$num_iterations
  num_min_prof   <- argv$num_min_prof
  min_elev_diff  <- argv$min_elev_diff
  min_horizontal_scale <- argv$min_horizontal_scale
  max_horizontal_scale <- argv$max_horizontal_scale
  kth_closest_obs_horizontal_scale <- argv$kth_closest_obs_horizontal_scale
  vertical_scale <- argv$vertical_scale
  
  tpos_score <- rep(1,N) * argv$tpos_score 
  tneg_score <- rep(1,N) * argv$tneg_score
  t_sod      <- rep(1,N) * argv$t_sod
  eps2       <- rep(1,N) * argv$eps2
  value_min  <- argv$value_min
  value_max  <- argv$value_max
  sig2o_min  <- argv$sig2o_min
  sig2o_max  <- argv$sig2o_max

  debug <- argv$debug
  
# Run SCT
#  res <- sct(lats, lons, elevs, values, obs_to_check, background_values, background_elab_type, num_min, num_max, inner_radius, outer_radius, num_iterations, num_min_prof, min_elev_diff, min_horizontal_scale, max_horizontal_scale, kth_closest_obs_horizontal_scale, vertical_scale, value_min, value_max, sig2o_min, sig2o_max, eps2, tpos_score, tneg_score, t_sod, debug)
  
#  print(head(res[[1]]))

  flag_test <- rep(10,N) 
  obs_out <- cbind(obs, flag_test)
  print(head(obs_out))

  write.table(obs_out, file = paste0(argv$output_file_path, "obs_out_", argv$input_varname, "_", format(tseq[t], "%Y%m%d"), "_", argv$config_string,".txt"), sep = ";", row.names = FALSE) # add output path and link to argv
  print(paste0("Writing to file: ", paste0(argv$output_file_path, "obs_out_", argv$input_varname, "_", format(tseq[t],"%Y%m%d"), "_", argv$config_string, ".txt")))
  print("-----------------------------------------------------------------------------------------")

 # rm(curr_file, obs, values, lats, lons, elevs, )

}












