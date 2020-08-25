#+ Isolation test
isolation_test <- function( doit, argv, data, dqcflag, x, y){
#==============================================================================
  # set doit vector
  doit<-vector(length=ndata,mode="numeric")
  doit[]<-NA
  for (f in 1:nfin) doit[data$prid==argv$prid[f]]<-argv$doit.iso[f]
  #
  ix<-which(is.na(dqcflag) & doit!=0)
  if (length(ix)>0) {
    # define global 1D vector used in nstat (1D for fast access)
    xtot<-x[ix]
    ytot<-y[ix]
    xy<-cbind(xtot,ytot)
    isolation_check( ytot, xtot, argv$n.isol, argv$dr.isol)
    ns<-apply(xy,FUN=nstat,MARGIN=1,drmin=argv$dr.isol)
    sus<-which(ns<argv$n.isol & doit[ix]==1)
    # set dqcflag
    if (length(sus)>0) dqcflag[ix[sus]]<-argv$isol.code
  } else {
    print("no valid observations left, no check for isolated observations")
  }
  rm(doit)
  if (argv$verbose | argv$debug) {
    print(paste("# isolated observations=",length(which(dqcflag==argv$isol.code))))
    print("+---------------------------------+")
  }
  if (argv$debug) 
    save.image(file.path(argv$debug.dir,"dqcres_iso.RData")) 
  #
  return(dqcflag)
}
