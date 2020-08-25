#+ plausibility test
plausibility_test <- function( argv, data, dqcflag){
# use range_check from titanlib
#==============================================================================
  if ( length( ix <- which( (is.na(dqcflag) | dqcflag==argv$keep.code) & 
       range_check( values=data$value, min=argv$vmin, max=argv$vmax)[[2]]))>0) 
    dqcflag[ix]<-argv$p.code
  # verbose
  cat(paste("plausibility test (",argv$p.code,")\n"))
  cat(paste(" min/max thresholds =",argv$vmin,argv$vmax,"\n"))
  cat(paste(" # <min=", length( which( dqcflag==argv$p.code &
                                       data$value<argv$vmin)),"\n"))
  cat(paste(" # >max=", length( which( dqcflag==argv$p.code &
                                       data$value>argv$vmax)),"\n"))
  cat(paste(" # suspect observations=",length(which(dqcflag==argv$p.code &
                                                    !is.na(dqcflag))),"\n"))
  cat("+---------------------------------+\n")
  #
  return(dqcflag)
}
