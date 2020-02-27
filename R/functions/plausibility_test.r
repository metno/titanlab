#+ plausibility test
plausibility_test <- function(argv,data,dqcflag){
#==============================================================================

  ix<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) &
            (data$value<argv$vmin | data$value>argv$vmax))
  if (length(ix)>0) dqcflag[ix]<-argv$p.code
  if (argv$verbose | argv$debug) {
    print(paste0("plausibility test (",argv$p.code,")"))
    print(paste("min/max thresholds =",argv$vmin,argv$vmax))
    print(paste("# <min=",length(which(dqcflag==argv$p.code &
                                       data$value<argv$vmin))))
    print(paste("# >max=",length(which(dqcflag==argv$p.code &
                                     data$value>argv$vmax))))
    print(paste("# suspect observations=",length(which(dqcflag==argv$p.code &
                                                      !is.na(dqcflag)))))
    print("+---------------------------------+")
  }
  if (argv$debug)
    save.image(file.path(argv$debug.dir,"dqcres_plausibility.RData"))
  #
  return(dqcflag)
}
