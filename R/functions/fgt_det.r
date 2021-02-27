#+ check against first-guess (deterministic)
fgt_det <- function( argv, 
                     ndata, 
                     data,
                     x,
                     y,
                     z, 
                     rfg, 
                     rfgdem,
                     dqcflag){
#==============================================================================
  require(RANN)
  cat( paste0( "fgt-det-check (code=", argv$fg.code, ")\n"))
  nsus <- vector( mode="numeric", length=length(argv$inner_radius.fg))
  # set doit/prio vectors
  doit   <- vector( length=ndata, mode="numeric"); doit[]<-NA
  prio <- vector( length=ndata, mode="numeric"); prio[]<-NA
  for (f in 1:nfin) {
    ix <- which( data$prid == argv$prid[f])
    if ( length(ix) == 0) next
    doit[ix] <- argv$doit.fg[f]
    prio[ix] <- argv$prio.fg[f]
  }
  prio_unique <- sort( unique( prio, na.rm=T), decreasing=T)
  rm( ix)

  # prepare vectors of valid and admissible values
  if (argv$variable == "T") {
    values_mina <- data$value - 20
    values_maxa <- data$value + 20
    values_minv <- data$value - 1
    values_maxv <- data$value + 1
  } else if (argv$variable == "RR") {
    values_mina <- pmin( pmax( data$value - 10, 0), pmax( data$value - 0.5 * data$value, 0))
    values_maxa <- pmax( data$value + 10, data$value + 0.5 * data$value)
    values_minv <- pmin( pmax( data$value - 1, 0), pmax( data$value - 0.1 * data$value, 0))
    values_maxv <- pmax( data$value + 1, data$value + 0.1 * data$value)
  }

  # prepare fg
  fg_x    <- integer(0)
  fg_y    <- integer(0)
  fg_lat  <- integer(0)
  fg_lon  <- integer(0)
  fg_z    <- integer(0)
  fg_val  <- integer(0)
  nfg_val <- 0
  t0a <- Sys.time()
  if ( is.null(rfg)) boom("ERROR in fgt_det (1)")
  dfg <- getValues( rfg)
  if ( !is.null( rfgdem)) { dfgdem <- getValues( rfgdem) } else
                          { dfgdem <- rep( 0, length( dfg)) }

  # data transformation
  if (argv$transf.fg) {
    values_mina <- boxcox( x=values_mina, lambda=argv$boxcox.lambda)
    values_maxa <- boxcox( x=values_maxa, lambda=argv$boxcox.lambda)
    values_minv <- boxcox( x=values_minv, lambda=argv$boxcox.lambda)
    values_maxv <- boxcox( x=values_maxv, lambda=argv$boxcox.lambda)
    data$value <- boxcox( x=data$value, lambda=argv$boxcox.lambda)
    dfg <- boxcox( x=dfg, lambda=argv$boxcox.lambda)
  }

  # get coordinates into CRS 
  ixx <- which( !is.na(dfg) & !is.na(dfgdem))
  if ( length( ixx) > 0) {
    fgxy         <- as.data.frame( xyFromCell( rfg, ixx))
    names( fgxy) <- c( "x", "y")
    coordinates( fgxy) <- c( "x", "y")
    proj4string( fgxy) <- CRS( argv$proj4fg)
    fgxy_transf <- as.data.frame( spTransform( fgxy, CRS=argv$proj4_where_dqc_is_done))
    fg_x        <- fgxy_transf[,1]
    fg_y        <- fgxy_transf[,2]
    fgll_transf <- as.data.frame( spTransform( fgxy, CRS="+proj=longlat +datum=WGS84"))
    fg_lon      <- fgll_transf[,1]
    fg_lat      <- fgll_transf[,2]
    fg_z        <- dfgdem[ixx]
    fg_val      <- dfg[ixx]
    rm( fgxy, fgxy_transf, fgll_transf)
  }

  # functions
  spatagg <- function(i) { val <- fg_val[nnix[i,]]; return( c( mean( val), sd( val))) }
  demspatagg <- function(i) { val <- fg_z[nnix[i,]]; mean( val) }

  # test
  for (i in 1:argv$i.fg) {
    nsus[]<-0
    for (j in 1:length(argv$inner_radius.fg)) {
      tpos <- vector( length=ndata, mode="numeric"); tpos[]<-NA
      tneg <- vector( length=ndata, mode="numeric"); tneg[]<-NA
      for (f in 1:nfin) {
        ix <- which( data$prid == argv$prid[f])
        if ( length(ix) == 0) next
        tpos[ix] <- argv$tpos.fg[(j-1)*nfin+f]
        tneg[ix] <- argv$tneg.fg[(j-1)*nfin+f]
      }
      prio_unique <- sort( unique( prio, na.rm=T), decreasing=T)
      rm( ix)

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
          obsToCheck_mina <- values_mina[ix]
          obsToCheck_maxa <- values_maxa[ix]
          obsToCheck_minv <- values_minv[ix]
          obsToCheck_maxv <- values_maxv[ix]
          obsToCheck_tpos <- tpos[ix]
          obsToCheck_tneg <- tneg[ix]
          obsToCheck_chk <- rep( 0, obsToCheck_n)
          # check only those observations with priorities geq than this
          obsToCheck_chk[prio[ix]>=prio_unique[k]] <- 1
 
          nn2 <- nn2( cbind( fg_x, fg_y), 
                      query = cbind( x[ix], y[ix]), 
                      k = argv$num_max_outer.fg[i], 
                      searchtype = "radius", radius = )
          nnix <- nn2[[1]]
          if (!is.na(argv$cores)) {
            fgstat_at_opoint <- t( mcmapply( spatagg,
                                   1:length(x),
                                   mc.cores = argv$cores,
                                   SIMPLIFY = T))
          # no-multicores
          } else {
            fgstat_at_opoint <- t( mapply( spatagg,
                                 1:length(x),
                                 SIMPLIFY = T))
          }

          background_values <- fgstat_at_opoint[,1]
          background_uncertainties <- fgstat_at_opoint[,2]

          if ( !is.null( rfgdem)) {
            if (!is.na(argv$cores)) {
              demstat_at_opoint <- t( mcmapply( demspatagg,
                                     1:length(x),
                                     mc.cores = argv$cores,
                                     SIMPLIFY = T))
            # no-multicores
            } else {
              demstat_at_opoint <- t( mapply( demspatagg,
                                   1:length(x),
                                   SIMPLIFY = T))
            }
            background_values <- background_values + argv$gamma.standard * ( z - demstat_at_opoint)
          }
  
          background_elab_type <- "External"
          debug <- FALSE
          num_min_prof <- -999
          min_elev_diff <- -999
          res <- fgt( points = Points( obsToCheck_lat, 
                                       obsToCheck_lon, 
                                       obsToCheck_z),
                      obsToCheck_val,
                      obsToCheck_chk,
                      background_values,
                      background_uncertainties,
                      background_elab_type,
                      argv$num_min_outer.fg[j],
                      argv$num_max_outer.fg[j],
                      argv$inner_radius.fg[j],
                      argv$outer_radius.fg[j],
                      100,
                      num_min_prof,
                      min_elev_diff,
                      obsToCheck_mina,
                      obsToCheck_maxa,
                      obsToCheck_minv,
                      obsToCheck_maxv,
                      obsToCheck_tpos,
                      obsToCheck_tneg,
                      debug)
 
          flag <- res[[1]]

          # suspect if: 
          sus<-which( flag[1:obsToCheck_n] == 1 &
                      is.na(dqcflag[ix]) &
                      doit[ix]==1 )

          # set dqcflag
          if (length(sus)>0) dqcflag[ix[sus]] <- argv$fg.code

        } else {
          cat( "no valid observations left, no FGT deterministic check\n")
        }
        nsus[j]<-ifelse(exists("sus"),length(sus),0)
        t1a<-Sys.time()
        str<-" (#TOT "
        for (f in 1:nfin) {
          if (f>1) str<-paste0(str,"; ")
          aux<-which(dqcflag==argv$fg.code & data$prid==argv$prid[f])
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

