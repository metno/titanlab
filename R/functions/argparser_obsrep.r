  #.............................................................................. 
  #
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
