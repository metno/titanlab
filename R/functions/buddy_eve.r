#+ buddy check event based
buddy_eve <- function( argv, 
                       ndata, 
                       data,
                       rfg,
                       rfgdem, 
                       dqcflag){
#==============================================================================
  cat( paste0("buddy_eve-check (",argv$buddy_eve.code,")\n"))
  cat( paste0("priorities ",toString(argv$prio.buddy_eve),"\n"))
  proj4_longlat <- "+proj=longlat +datum=WGS84"
  nsus <- vector( mode="numeric", length=length(argv$thr_eve.buddy_eve))
  # set doit/prio vectors
  doit <- vector( length=ndata, mode="numeric"); doit[] <- NA
  prio <- vector( length=ndata, mode="numeric"); prio[] <- NA
  for (f in 1:nfin) {
    aux <- which( data$prid == argv$prid[f])
    if ( length(aux) == 0) next
    doit[aux] <- argv$doit.buddy_eve[f]
    prio[aux] <- argv$prio.buddy_eve[f]
  }
  rm( aux)
  # if needed, prepare fg for buddy check
  # if argv$usefg.buddy_eve are not all NAs and at least one of them == 1
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
    ixx <- which( !is.na( dfg) & !is.na( dfgdem))
    if ( length( ixx) > 0) {
      fgxy<-as.data.frame(xyFromCell(rfg,ixx))
      names(fgxy)<-c("x","y")
      coordinates(fgxy)<-c("x","y")
      proj4string(fgxy)<-CRS(argv$proj4fg)
      fgxy_transf <- as.data.frame(spTransform(fgxy,CRS=argv$proj4_where_dqc_is_done))
      fg_x <- fgxy_transf[,1]
      fg_y <- fgxy_transf[,2]
      fgll_transf <- as.data.frame(spTransform(fgxy,CRS=argv$proj4_longlat))
      fg_lon  <- fgll_transf[,1]
      fg_lat  <- fgll_transf[,2]
      fg_z    <- dfgdem[ixx]
      fg_val  <- dfg[ixx]
      fg_prio <- rep(-1,length(ixx))
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
    rm( ix, obsToCheck_x, obsToCheck_y, thin_fg)
    rm( dfg, ixx, dr_max)
    nfg_val <- length( fg_val)
    t1a <- Sys.time()
    cat( paste0( "use first-guess values, number of values is =",nfg_val,
                 "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit"),"\n"))
  } # end prepare fg for the test
  # test
  for (i in 1:argv$i.buddy_eve) {
    priority <- ifelse( (i==1 & any(prio!=(-1))), T, F)
    nsus[] <- 0
    for (j in 1:length(argv$thr_eve.buddy_eve)) {
      # use only (probably) good observations with doit!=0
      flag_aux<-( (is.na(dqcflag) | dqcflag==argv$keep.code) &
                  !is.na(x) & !is.na(y) & !is.na(data$value) &
                  doit!=0)
      if (priority) flag_aux <- flag_aux & !is.na(prio)
      if (argv$variable=="T") flag_aux <- flag_aux & !is.na(z)
      ix  <- which(flag_aux); rm(flag_aux)
      t0a <- Sys.time()
      obsToCheck_n <- length(ix)
      if (obsToCheck_n > 0) {
        obsToCheck_lon  <- data$lon[ix]
        obsToCheck_lat  <- data$lat[ix]
        obsToCheck_z    <- as.numeric( z[ix])
        obsToCheck_prio <- as.numeric( prio[ix])
        obsToCheck_val  <- data$value[ix]
        obsToCheck_chk  <- rep(1,obsToCheck_n)
        # define global 1D vector used in statSpat (1D for fast access)
        if (argv$usefg.buddy_eve[j]==1 & !is.na(argv$usefg.buddy_eve[j])) {
          obsToCheck_lon    <- c( obsToCheck_lon, fg_lon)
          obsToCheck_lat    <- c( obsToCheck_lat, fg_lat)
          obsToCheck_z    <- as.numeric( c(    obsToCheck_z, fg_z))
          obsToCheck_prio <- as.numeric( c( obsToCheck_prio, fg_prio))
          obsToCheck_val  <- c( obsToCheck_val, fg_val)
          obsToCheck_chk  <- c( obsToCheck_chk, rep(0,length(fg_val)))
        }
        elev_gradient <- ifelse(argv$variable=="T", argv$gamma.standard, 0)
        flag <- buddy_event_check( obsToCheck_lat,
                                   obsToCheck_lon,
                                   obsToCheck_z,
                                   obsToCheck_val,
                                   argv$dr.buddy_eve[j],
                                   argv$n.buddy_eve[j],
                                   argv$thr_eve.buddy_eve[j],
                                   argv$thr.buddy_eve[j],
                                   argv$dz.buddy_eve[j],
                                   elev_gradient,
                                   1,
                                   obsToCheck_chk)
        sus <- which( is.na(dqcflag[ix]) & doit[ix]==1 & flag)                          
        # set dqcflag
        if ( length( sus) > 0) dqcflag[ix[sus]] <- argv$buddy_eve.code
      } else {
        cat( "no valid observations left, no buddy_eve check\n")
      }
      nsus[j]<-ifelse(exists("sus"),length(sus),0)
      t1a<-Sys.time()
      str<-" (#TOT "
      for (f in 1:nfin) {
        if (f>1) str<-paste0(str,"; ")
        aux<-which(dqcflag==argv$buddy_eve.code & data$prid==argv$prid[f])
        str<-paste0(str,"prid",argv$prid[f],"=",length(aux))
        rm(aux)
      }
      str<-paste0(str,")")
      if (i==1) {
        cat( paste0( "iteration=",i,
                     "/test=",j,
                     "/event=yes if value less than ",argv$thr_eve.buddy_eve[j],
                     "/dqc param: thres=",argv$thr.buddy_eve[j],
                     "rad=",argv$dr.buddy_eve[j],
                     "usefg=",argv$usefg.buddy_eve[j],
                     "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit"),"\n"))
      } else {
        cat( paste0( "iteration=",i,
                     "/test=",j,
                     "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit"),"\n"))
      }
      cat( paste0(nsus[j]," new suspect observations",str))
    } # end for j
    if (!priority & sum(nsus)<=argv$break.buddy_eve) break
  }  # end for i
  cat("+---------------------------------+\n")
  #
  return(dqcflag)
}
