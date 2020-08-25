#+ buddy check 
buddy <- function( argv, 
                   ndata, 
                   data,
                   rfg,
                   rfgdem, 
                   dqcflag){
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
  rm( aux)
  # if needed, prepare fg for buddy check
  # if argv$usefg.buddy are not all NAs and at least one of them == 1
  fg_x    <- integer(0)
  fg_y    <- integer(0)
  fg_lat  <- integer(0)
  fg_lon  <- integer(0)
  fg_z    <- integer(0)
  fg_val  <- integer(0)
  fg_prio <- integer(0)
  nfg_val <- 0
  if ( any( !is.na( argv$usefg.buddy_eve)) & 
       any( argv$usefg.buddy_eve == 1)) {
    t0a <- Sys.time()
    if ( is.null(rfg)) boom("ERROR in buddy_eve (1)")
    dfg <- getValues( rfg)
    if ( !is.null( rfgdem)) { dfgdem <- getValues( rfgdem) } else
                            { dfgdem <- rep( 0, length( dfg)) }
    # get coordinates into CRS 
    ixx<-which(!is.na(dfg) & !is.na(dfgdem))
    if (length(ixx)>0) {
      fgxy<-as.data.frame(xyFromCell(rfg,ixx))
      names(fgxy)<-c("x","y")
      coordinates(fgxy)<-c("x","y")
      proj4string(fgxy)<-CRS(argv$proj4fg)
      fgxy_transf<-as.data.frame(spTransform(fgxy,CRS=argv$proj4_where_dqc_is_done))
      fg_x<-fgxy_transf[,1]
      fg_y<-fgxy_transf[,2]
      fgll_transf <- as.data.frame(spTransform(fgxy,CRS=argv$proj4_longlat))
      fg_lon  <- fgll_transf[,1]
      fg_lat  <- fgll_transf[,2]
      fg_z<-dfgdem[ixx]
      fg_val<-dfg[ixx]
      fg_prio<-rep(-1,length(ixx))
      rm( fgxy, fgxy_transf, fgll_transf)
    }
    # thinning
    ix<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) &
               !is.na(x) & !is.na(y) & !is.na(z) & !is.na(prio) &
               !is.na(data$value) & doit!=0 )
    #+ function
    thin_fg_for_buddy<-function(i, dr) {
      if (any(abs(obsToCheck_x-fg_x[i])<dr & abs(obsToCheck_y-fg_y[i])<dr)) {
        return(1)
      } else {
        return(0)
      }
    }
    obsToCheck_x <- x[ix]
    obsToCheck_y <- y[ix]
    dr_max <- max( argv$dr.buddy_eve[ argv$usefg.buddy_eve==1 & !is.na(argv$usefg.buddy_eve)])
    if (!is.na(argv$cores)) {
      thin_fg <- mcmapply( thin_fg_for_buddy,
                           1:length(fg_x),
                           mc.cores = argv$cores,
                           SIMPLIFY = T,
                           dr       = dr_max)
    # no-multicores
    } else {
      thin_fg <- mapply( thin_fg_for_buddy,
                         1:length(fg_x),
                         SIMPLIFY = T,
                         dr       = dr_max)
    }
    ixx<-which(thin_fg>0)
    if (length(ixx)>0) {
      fg_x    <- fg_x[ixx]
      fg_y    <- fg_y[ixx]
      fg_lon  <- fg_lon[ixx]
      fg_lat  <- fg_lat[ixx]
      fg_z    <- fg_z[ixx]
      fg_val  <- fg_val[ixx]
      fg_prio <- fg_prio[ixx]
    }
    rm(ix,obsToCheck_x,obsToCheck_y,thin_fg)
    rm(dfg,ixx,dr_max)
    nfg_val<-length(fg_val)
    t1a<-Sys.time()
    cat( paste0("use first-guess values, number of values is =",nfg_val,
                 "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit"),"\n"))
  } # end prepare fg for the test
  # test
  for (i in 1:argv$i.buddy) {
    priority<-ifelse((i==1 & any(prio!=(-1))),T,F)
    nsus[]<-0
    for (j in 1:length(argv$dr.buddy)) {
      # use only (probably) good observations with doit!=0
      flag_aux<-( (is.na(dqcflag) | dqcflag==argv$keep.code) &
                  !is.na(x) & !is.na(y) & !is.na(data$value) &
                  doit!=0)
      if (priority) flag_aux<-flag_aux & !is.na(prio)
      if (argv$variable=="T") flag_aux<-flag_aux & !is.na(z)
      ix<-which(flag_aux); rm(flag_aux)
      t0a<-Sys.time()
      obsToCheck_n<-length(ix)
      if (obsToCheck_n>0) {
        # define global 1D vector used in statSpat (1D for fast access)
        obsToCheck_lon <- data$lon[ix]
        obsToCheck_lat <- data$lat[ix]
        obsToCheck_z   <- as.numeric( z[ix])
        obsToCheck_val <- data$value[ix]
        obsToCheck_chk <- rep( 1, obsToCheck_n)
        if ( argv$usefg.buddy[j]==1 & !is.na(argv$usefg.buddy[j])) {
          obsToCheck_lon <- c( obsToCheck_lon, fg_lon)
          obsToCheck_lat <- c( obsToCheck_lat, fg_lat)
          obsToCheck_z   <- as.numeric( c( obsToCheck_z, fg_z))
          obsToCheck_val <- c( obsToCheck_val, fg_val)
          obsToCheck_chk <- c( obsToCheck_chk, rep( 0, length( fg_val)))
        }
        if (argv$transf.buddy) 
          obsToCheck_val<-boxcox(x=obsToCheck_val,lambda=argv$boxcox.lambda)

        elev_gradient <- ifelse(argv$variable=="T", argv$gamma.standard, 0) 
        flag <- buddy_check( obsToCheck_lat,
                             obsToCheck_lon,
                             obsToCheck_z,
                             obsToCheck_val,
                             argv$dr.buddy[j],
                             argv$n.buddy[j],
                             argv$thr.buddy[j],
                             argv$dz.buddy[j],
                             elev_gradient,
                             argv$sdmin.buddy,
                             1,
                             obsToCheck_chk)
        if (flag[[1]]) {
          # suspect if: 
          sus<-which( flag[[2]][1:obsToCheck_n] &
                      is.na(dqcflag[ix]) &
                      doit[ix]==1 )
          # set dqcflag
          if (length(sus)>0) dqcflag[ix[sus]]<-argv$buddy.code
        }
      } else {
        print("no valid observations left, no buddy check")
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
      if (i==1) {
        print(paste0("iteration=",i,
                     "/test=",j,
                     "/dqc param: thres=",argv$thr.buddy[j],
                     "rad=",argv$dr.buddy[j],
                     "usefg=",argv$usefg.buddy[j],
                     "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
      } else {
        print(paste0("iteration=",i,
                     "/test=",j,
                     "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
      }
      print(paste0(nsus[j]," new suspect observations",str))
      rm(str)
    } # end for j
    if (!priority & sum(nsus)<=argv$break.buddy) break
  }  # end for i
  cat("+---------------------------------+\n")
  #
  return(dqcflag)
}
