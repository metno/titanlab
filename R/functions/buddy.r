#+ buddy check 
buddy <- function( argv, 
                   ndata, 
                   data,
                   dqcflag){
#------------------------------------------------------------------------------
# NOTE: priorities work like this: the larger the value, the lower the priority

#==============================================================================
  cat( paste0( "buddy-check (",argv$buddy.code,")\n"))
  cat( paste0( "priorities ",toString(argv$prio.buddy),"\n"))
  nsus <- vector( mode="numeric", length=length(argv$dr.buddy))
  # set doit/prio vectors
  doit <- vector( length=ndata, mode="numeric"); doit[]<-NA
  prio <- vector( length=ndata, mode="numeric"); prio[]<-NA
  for (f in 1:nfin) {
    aux <- which( data$prid == argv$prid[f])
    if ( length(aux) == 0) next
    doit[aux] <- argv$doit.buddy[f]
    prio[aux] <- argv$prio.buddy[f]
  }
  prio_unique <- sort( unique( prio, na.rm=T), decreasing=T)
  rm( aux)

  if (argv$transf.buddy) 
    data$value <- boxcox( x=data$value, lambda=argv$boxcox.lambda)

  # test
  for (i in 1:argv$i.buddy) {
    nsus[]<-0
    for (j in 1:length(argv$dr.buddy)) {
      for (k in 1:length(prio_unique)) {
        # use only (probably) good observations with doit!=0
        flag_aux<-( (is.na(dqcflag) | dqcflag==argv$keep.code) &
                    !is.na(x) & !is.na(y) & !is.na(data$value) &
                    doit!=0 & !is.na(prio))
        if (argv$variable == "T") flag_aux <- flag_aux & !is.na(z)
        ix <- which( flag_aux); rm( flag_aux)
        t0a <- Sys.time()
        obsToCheck_n<-length(ix)
        if (obsToCheck_n>0) {
          # define global 1D vector used in statSpat (1D for fast access)
          obsToCheck_lon <- data$lon[ix]
          obsToCheck_lat <- data$lat[ix]
          obsToCheck_z   <- as.numeric( z[ix])
          obsToCheck_val <- data$value[ix]
          obsToCheck_chk <- rep( 0, obsToCheck_n)
          # check only those observations with priorities geq than this
          obsToCheck_chk[prio[ix]>=prio_unique[k]] <- 1
  
          elev_gradient <- ifelse(argv$variable=="T", argv$gamma.standard, 0) 

          flag <- buddy_check( points = Points( obsToCheck_lat, 
                                                obsToCheck_lon, 
                                                obsToCheck_z),
                               obsToCheck_val,
                               argv$dr.buddy[j],
                               argv$n.buddy[j],
                               argv$thr.buddy[j],
                               argv$dz.buddy[j],
                               elev_gradient,
                               argv$sdmin.buddy,
                               100,
                               obsToCheck_chk)
          # suspect if: 
          sus<-which( flag[1:obsToCheck_n] == 1 &
                      is.na(dqcflag[ix]) &
                      doit[ix]==1 )

          # set dqcflag
          if (length(sus)>0) dqcflag[ix[sus]] <- argv$buddy.code

        } else {
          cat( "no valid observations left, no buddy check\n")
        }
        nsus[j]<-ifelse(exists("sus"),length(sus),0)
        t1a<-Sys.time()
        str<-" (#TOT "
        for (f in 1:nfin) {
          if (f>1) str<-paste0(str,"; ")
          aux<-which(dqcflag==argv$buddy.code & data$prid==argv$prid[f])
          str<-paste0(str,"prid",argv$prid[f],"=",length(aux))
          rm(aux)
        }
        str<-paste0(str,")")
        cat( paste0( "iteration=",i,
                      "/test=",j,
                      "/prio>=",prio_unique[k],
                      "/dqc param: thres=",argv$thr.buddy[j],",",
                      "rad=",argv$dr.buddy[j],",",
                      "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit"),"\n"))
        cat( paste0(nsus[j]," new suspect observations",str,"\n"))
        rm(str)
      } # end for k
    } # end for j
    if ( sum(nsus) <= argv$break.buddy) break
  }  # end for i
  cat("+---------------------------------+\n")
  #
  return(dqcflag)
}
