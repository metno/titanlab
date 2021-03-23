  #.............................................................................. 
  # doit flags

  comstr<-" Decide if the test should be applied to all, none or only to a selection of observations based on the provider. Possible values are 0, 1, 2. It is possible to specify either one global value or one value for each provider. Legend: 1 => the observations will be used in the elaboration and they will be tested; 0 => the observations will not be used and they will not be tested; 2 => the observations will be used but they will not be tested."

  p <- add_argument(p, "--doit.buddy",
                    help=paste("customize the buddy-test application.",comstr),
                    type="numeric",
                    default=NA,
                    nargs=Inf)

  p <- add_argument(p, "--doit.sct",
                    help=paste("customize the application of SCT.",comstr),
                    type="numeric",
                    default=NA,
                    nargs=Inf)

  p <- add_argument(p, "--doit.clim",
                    help=paste("customize the application of the climatological check.",comstr),
                    type="numeric",
                    default=NA,
                    nargs=Inf)

  p <- add_argument(p, "--doit.dem",
                    help=paste("customize the application of the test of observation elevation against the digital elevation model.",comstr),
                    type="numeric",
                    default=NA,
                    nargs=Inf)

  p <- add_argument(p, "--doit.isol",
                    help=paste("customize the application of the isolation test.",comstr),
                    type="numeric",
                    default=NA,
                    nargs=Inf)

  p <- add_argument(p, "--doit.sct_dual",
                    help=paste("customize the cool check application.",comstr),
                    type="numeric",
                    default=NA,
                    nargs=Inf)
