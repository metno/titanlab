#+ check against first-guess (deterministic)
fg_det <- function(argv,data,dqcflag){
#==============================================================================
  if (!any(!is.na(fg))) {
    if (argv$verbose | argv$debug) print("first guess is not defined (all NAs)")
  } else {
    if (argv$verbose | argv$debug)
      print(paste0("first-guess check det (",argv$fg.code,")"))
    # set doit vector
    doit<-vector(length=ndata,mode="numeric"); doit[]<-NA
    thrvec<-vector(length=ndata,mode="numeric"); thrvec[]<-NA
    thrposvec<-vector(length=ndata,mode="numeric"); thrposvec[]<-NA
    thrnegvec<-vector(length=ndata,mode="numeric"); thrnegvec[]<-NA
    thrpercvec<-vector(length=ndata,mode="numeric"); thrpercvec[]<-NA
    thrpospercvec<-vector(length=ndata,mode="numeric"); thrpospercvec[]<-NA
    thrnegpercvec<-vector(length=ndata,mode="numeric"); thrnegpercvec[]<-NA
    fg_minval<-vector(length=ndata,mode="numeric"); fg_minval[]<-NA
    fg_maxval<-vector(length=ndata,mode="numeric"); fg_maxval[]<-NA
    obs_minval<-vector(length=ndata,mode="numeric"); obs_minval[]<-NA
    obs_maxval<-vector(length=ndata,mode="numeric"); obs_maxval[]<-NA
    fg_minval_perc<-vector(length=ndata,mode="numeric"); fg_minval_perc[]<-NA
    fg_maxval_perc<-vector(length=ndata,mode="numeric"); fg_maxval_perc[]<-NA
    obs_minval_perc<-vector(length=ndata,mode="numeric"); obs_minval_perc[]<-NA
    obs_maxval_perc<-vector(length=ndata,mode="numeric"); obs_maxval_perc[]<-NA
    fg_range<-range(fg,na.rm=T)
    obs_range<-range(data$value,na.rm=T)
    for (f in 1:nfin) {
      if (!any(data$prid==argv$prid[f])) next
      aux<-which(data$prid==argv$prid[f])
      doit[aux]<-argv$doit.fg[f]
      thrvec[aux]<-argv$thr.fg[f]
      thrposvec[aux]<-argv$thrpos.fg[f]
      thrnegvec[aux]<-argv$thrneg.fg[f]
      thrpercvec[aux]<-argv$thrperc.fg[f]
      thrpospercvec[aux]<-argv$thrposperc.fg[f]
      thrnegpercvec[aux]<-argv$thrnegperc.fg[f]
      fg_minval[aux]<-ifelse(is.na(argv$fg_minval.fg[f]),fg_range[1],
                                                         argv$fg_minval.fg[f])
      fg_maxval[aux]<-ifelse(is.na(argv$fg_maxval.fg[f]),fg_range[2],
                                                         argv$fg_maxval.fg[f])
      obs_minval[aux]<-ifelse(is.na(argv$obs_minval.fg[f]),obs_range[1],
                                                           argv$obs_minval.fg[f])
      obs_maxval[aux]<-ifelse(is.na(argv$obs_maxval.fg[f]),obs_range[2],
                                                          argv$obs_maxval.fg[f])
      fg_minval_perc[aux]<-ifelse(is.na(argv$fg_minval_perc.fg[f]),fg_range[1],
                                                       argv$fg_minval_perc.fg[f])
      fg_maxval_perc[aux]<-ifelse(is.na(argv$fg_maxval_perc.fg[f]),fg_range[2],
                                                       argv$fg_maxval_perc.fg[f])
      obs_minval_perc[aux]<-ifelse(is.na(argv$obs_minval_perc.fg[f]),obs_range[1],
                                                      argv$obs_minval_perc.fg[f])
      obs_maxval_perc[aux]<-ifelse(is.na(argv$obs_maxval_perc.fg[f]),obs_range[2],
                                                       argv$obs_maxval_perc.fg[f])
      rm(aux)
    }
    # use only (probably) good observations
    ix<-which(is.na(dqcflag) & doit!=0)
    if (length(ix)>0) {
      dev<-data$value-fg
      devperc<-dev/fg
      flag_sus<-rep(F,ndata)
      flag_to_check_basic<-is.na(dqcflag) & doit==1 &
                     !is.na(data$value) &
                     !is.nan(data$value) &
                     is.finite(data$value) &
                     !is.na(fg) & !is.nan(fg) & is.finite(fg)
      flag_to_check<-flag_to_check_basic &
                     data$value>=obs_minval & data$value<=obs_maxval &
                     fg>=fg_minval & fg<=fg_maxval
      # additive model
      if (any(!is.na(thrvec)))
        flag_sus<-flag_sus |
         (!is.na(thrvec) & flag_to_check & abs(dev)>thrvec)
      if (any(!is.na(thrposvec)))
        flag_sus<-flag_sus |
         (!is.na(thrposvec) & flag_to_check & dev>thrposvec)
      if (any(!is.na(thrnegvec)))
        flag_sus<-flag_sus |
         (!is.na(thrnegvec) & flag_to_check & dev<0 & abs(dev)>thrnegvec)
      # multiplicative model
      flag_to_check<-flag_to_check_basic &
                     data$value>=obs_minval_perc & data$value<=obs_maxval_perc &
                     fg>=fg_minval_perc & fg<=fg_maxval_perc
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
      ix_sus<-which(flag_sus)
      rm(flag_sus,flag_to_check,flag_to_check_basic,dev,devperc)
      rm(doit,thrvec,thrposvec,thrnegvec,thrpercvec)
      rm(thrpospercvec,thrnegpercvec)
      # set dqcflag
      if (length(ix_sus)>0) dqcflag[ix_sus]<-argv$fg.code
      rm(ix_sus)
    }  else {
      print("no valid observations left, no first-guess check")
    }
    if (argv$verbose | argv$debug) {
      print(paste("# observations that fail the first-guess check (det)=",
                  length(which(dqcflag==argv$fg.code))))
      print("+---------------------------------+")
    }
    if (argv$debug)
      save.image(file.path(argv$debug.dir,"dqcres_fg.RData"))
    if (exists("doit")) rm(doit)
    if (exists("thrvec")) rm(thrvec)
    if (exists("thrposvec")) rm(thrposvec)
    if (exists("thrnegvec")) rm(thrnegvec)
    if (exists("obs_minval_perc")) rm(obs_minval_perc)
    if (exists("obs_maxval_perc")) rm(obs_maxval_perc)
    if (exists("fg_minval_perc")) rm(fg_minval_perc)
    if (exists("fg_maxval_perc")) rm(fg_maxval_perc)
    if (exists("obs_minval")) rm(obs_minval)
    if (exists("obs_maxval")) rm(obs_maxval)
    if (exists("fg_minval")) rm(fg_minval)
    if (exists("fg_maxval")) rm(fg_maxval)
    if (exists("thrpercvec")) rm(thrpercvec)
    if (exists("thrpospercvec")) rm(thrpospercvec)
    if (exists("thrnegpercvec")) rm(thrnegpercvec)
  }
  #
  return(dqcflag)
}


