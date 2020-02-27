#+
climatological_check <- function( argv, data, dqcflag ) {
#==============================================================================
# set doit vector
  doit<-vector(length=ndata,mode="numeric")
  doit[]<-NA
  for (f in 1:nfin)
    doit[data$prid==argv$prid[f]]<-argv$doit.clim[f]
  # apply the test on all the observations except blacklist/keeplist 
  ix<-which(is.na(dqcflag))
  if (length(ix)>0) {
    # flag only observations that are suspect and have doit==1
    sus<-which( (data$value[ix]<argv$vmin.clim[argv$month.clim] |
                 data$value[ix]>argv$vmax.clim[argv$month.clim]) &
                 doit[ix]==1)
    # set dqcflag
    if (length(sus)>0) dqcflag[ix[sus]]<-argv$clim.code
  } else {
    print("no valid observations left, no climatological check")
  }
  if (argv$verbose | argv$debug) {
    print(paste("climatological test (month=",argv$month.clim,")",sep=""))
    print(paste("# suspect observations=",length(which(dqcflag==argv$clim.code))))
    print("+---------------------------------+")
  }
  rm(doit)
  if (argv$debug)
    save.image(file.path(argv$debug.dir,"dqcres_monthclim.RData"))
  #
  return(dqcflag)
}
