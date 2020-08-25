#+
climatological_check <- function( argv, ndata, data, dqcflag ) {
#==============================================================================
# set doit vector
  doit<-vector(length=ndata,mode="numeric")
  doit[]<-NA
  for (f in 1:nfin)
    doit[data$prid==argv$prid[f]]<-argv$doit.clim[f]
  # apply the test on all the observations except blacklist/keeplist 
  ix <- which( is.na( dqcflag))
  if ( length(ix) > 0) {
    # flag only observations that are suspect and have doit==1
    sus <- which( ( data$value[ix] < argv$vmin.clim[argv$month.clim] |
                    data$value[ix] > argv$vmax.clim[argv$month.clim]) &
                    doit[ix]==1)
    # set dqcflag
    if ( length( sus) > 0) dqcflag[ix[sus]]<-argv$clim.code
  } else {
    cat( "no valid observations left, no climatological check\n")
  }
  cat( paste( "Climatological test (month=",argv$month.clim,")","\n",sep=""))
  cat( paste( " min/max thresholds",argv$vmin.clim[argv$month.clim],argv$vmax.clim[argv$month.clim],"\n"))
  cat( paste( " # suspect observations=",length(which(dqcflag==argv$clim.code)),"\n"))
  cat( "+---------------------------------+\n")
  #
  return(dqcflag)
}
