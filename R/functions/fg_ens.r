#+ check against first-guess (ensemble)
fg_ens <- function( argv,
                    data,
                    dqcflag,
                    fge.mu,
                    fge.sd){
#==============================================================================
  cat( paste0("first-guess check ens (",argv$fge.code,")\n"))
  # set doit vector
  doit<-vector(length=ndata,mode="numeric"); doit[]<-NA
  thrvec<-vector(length=ndata,mode="numeric"); thrvec[]<-NA
  thrposvec<-vector(length=ndata,mode="numeric"); thrposvec[]<-NA
  thrnegvec<-vector(length=ndata,mode="numeric"); thrnegvec[]<-NA
  thrpercvec<-vector(length=ndata,mode="numeric"); thrpercvec[]<-NA
  perc_minvalvec<-vector(length=ndata,mode="numeric"); perc_minvalvec[]<-NA
  thrpospercvec<-vector(length=ndata,mode="numeric"); thrpospercvec[]<-NA
  thrnegpercvec<-vector(length=ndata,mode="numeric"); thrnegpercvec[]<-NA
  throutvec<-vector(length=ndata,mode="numeric"); throutvec[]<-NA
  thrposoutvec<-vector(length=ndata,mode="numeric"); thrposoutvec[]<-NA
  thrnegoutvec<-vector(length=ndata,mode="numeric"); thrnegoutvec[]<-NA
  for (f in 1:nfin) {
    if (!any(data$prid==argv$prid[f])) next
    aux<-which(data$prid==argv$prid[f])
    doit[aux]<-argv$doit.fge[f]
    thrvec[aux]<-argv$thr.fge[f]
    thrposvec[aux]<-argv$thrpos.fge[f]
    thrnegvec[aux]<-argv$thrneg.fge[f]
    perc_minvalvec[aux]<-argv$perc.fge_minval[f]
    thrpercvec[aux]<-argv$thrperc.fge[f]
    thrpospercvec[aux]<-argv$thrposperc.fge[f]
    thrnegpercvec[aux]<-argv$thrnegperc.fge[f]
    throutvec[aux]<-argv$throut.fge[f]
    thrposoutvec[aux]<-argv$thrposout.fge[f]
    thrnegoutvec[aux]<-argv$thrnegout.fge[f]
    rm(aux)
  }
  # use only (probably) good observations
  ix <- which( is.na(dqcflag) & doit!=0)
  if (length(ix)>0) {
    dev      <- data$value - fge.mu
    devperc  <- dev / fge.mu
    devout   <- dev / fge.sd
    flag_sus <- rep( F, ndata)
    flag_to_check <- is.na(dqcflag) & doit==1 &
                     !is.na(data$value) &
                     !is.nan(data$value) &
                     is.finite(data$value) &
                     !is.na(fge.mu) & !is.nan(fge.mu) & is.finite(fge.mu)
    if (any(!is.na(thrvec)))
      flag_sus <- flag_sus |
       (!is.na(thrvec) & flag_to_check & abs(dev)>thrvec)
    if (any(!is.na(thrposvec)))
      flag_sus<-flag_sus |
       (!is.na(thrposvec) & flag_to_check & dev>thrposvec)
    if (any(!is.na(thrnegvec)))
      flag_sus<-flag_sus |
       (!is.na(thrnegvec) & flag_to_check & dev<0 & abs(dev)>thrnegvec)
    flag_to_check<-flag_to_check & fge.mu>=perc_minvalvec
    if (any(!is.na(thrpercvec)))
      flag_sus<-flag_sus |
       (!is.na(thrpercvec) & flag_to_check & abs(devperc)>thrpercvec)
    if (any(!is.na(thrpospercvec)))
      flag_sus<-flag_sus |
       (!is.na(thrpospercvec) & flag_to_check &
        dev>0 & abs(devperc)>thrpospercvec)
    if (any(!is.na(thrnegpercvec)))
      flag_sus<-flag_sus |
       (!is.na(thrnegpercvec) & flag_to_check &
        dev<0 & abs(devperc)>thrnegpercvec)
    flag_to_check<-flag_to_check &
                   !is.na(fge.sd) & !is.nan(fge.sd) & is.finite(fge.sd)
    if (any(!is.na(throutvec)))
      flag_sus<-flag_sus |
       (!is.na(throutvec) & flag_to_check & abs(devout)>throutvec)
    if (any(!is.na(thrposoutvec)))
      flag_sus<-flag_sus |
       (!is.na(thrposoutvec) & flag_to_check &
        dev>0 & abs(devout)>thrposoutvec)
    if (any(!is.na(thrnegoutvec)))
      flag_sus<-flag_sus |
       (!is.na(thrnegoutvec) & flag_to_check &
        dev<0 & abs(devout)>thrnegoutvec)
    ix_sus<-which(flag_sus)
    # set dqcflag
    if (length(ix_sus)>0) dqcflag[ix_sus]<-argv$fge.code
  }  else {
    cat( "no valid observations left, no first-guess check\n")
  }
  cat( paste( "# observations that fail the first-guess check (ens)=",
              length(which(dqcflag==argv$fge.code)),"\n"))
  print("+---------------------------------+\n")
  #
  return(dqcflag)
}
