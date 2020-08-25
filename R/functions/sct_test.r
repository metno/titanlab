#+ SCT - Spatial Consistency Test
sct_test <- function( argv, 
                      ndata, 
                      data,
                      dqcflag,
                      x,
                      y,
                      z,
                      laf){
#==============================================================================
  cat( paste0( "SCT (", argv$sct.code, ")\n"))
  nprev <- 0
  # set doit vector
  doit     <- vector( length=ndata, mode="numeric"); doit[]<-NA
  eps2.sct <- vector( length=ndata, mode="numeric"); eps2.sct[]<-NA
  t2       <- vector( length=ndata, mode="numeric"); t2[]<-NA
  t2pos    <- vector( length=ndata, mode="numeric"); t2pos[]<-NA
  t2neg    <- vector( length=ndata, mode="numeric"); t2neg[]<-NA
  t2sod    <- vector( length=ndata, mode="numeric"); t2sod[]<-NA
  for (f in 1:nfin) {
    if ( !any( data$prid == argv$prid[f])) next
    aux <- which( data$prid==argv$prid[f])
    doit[aux]     <- argv$doit.sct[f]
    eps2.sct[aux] <- argv$eps2.sct[f]
    t2[aux]       <- argv$thr.sct[f]
    t2pos[aux]    <- argv$thrpos.sct[f]
    t2neg[aux]    <- argv$thrneg.sct[f]
    t2sod[aux]    <- argv$thrsod.sct[f]
    rm(aux)
  }
  if (argv$transf.sct) {
    yo_sct<-boxcox(x=data$value,lambda=argv$boxcox.lambda)
  } else {
    yo_sct<-data$value
  }
  b_sct <- yo_sct; b_sct[] <- -9999.
  if (argv$usefge.sct) {
    argv$background_elab_type.sct <- "external"
    if (argv$transf.sct) {
      b_sct<-boxcox(x=fge.mu,lambda=argv$boxcox.lambda)
    } else {
      b_sct<-fge.mu
    }
  } else if (argv$usefg.sct) {
    argv$background_elab_type.sct <- "external"
    if (argv$transf.sct) {
      b_sct<-boxcox(x=fg,lambda=argv$boxcox.lambda)
    } else {
      b_sct<-fg
    }
  }
  #
  ix<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) & doit!=0 )  
  t0a<-Sys.time()
#ivec titanlib::sct(const vec& lats,
#        const vec& lons,
#        const vec& elevs,
#        const vec& values,
#        // determine if we have too many or too few observations
#        // (too many means we can reduce the distance, too few mean isolation problem and cannot flag?)
#        int num_min,
#        int num_max,
#        // first find everything close to the point that we are testing (maxdist)
#        float inner_radius,
#        float outer_radius,
#        int num_iterations,
#        int num_min_prof,
#        float min_elev_diff,
#        float min_horizontal_scale,
#        float max_horizontal_scale,
#        float vertical_scale,
#        const vec& pos,
#        const vec& neg,
#        const vec& eps2,
#        vec& prob_gross_error,
#        vec& an_inc,
#        vec& an_res,
#        vec& cv_res,
#        vec& innov,
#        vec& chi,
#        vec& rep) {
  res <- sct( data$lat[ix], data$lon[ix], z[ix], 
              yo_sct[ix],
              doit[ix], 
              b_sct[ix],
              argv$background_elab_type.sct,
              argv$pmin.sct,
              argv$pmax.sct,
              argv$inner_radius.sct, 
              argv$outer_radius.sct,
              argv$i.sct,
              argv$pmin_Frei_vert_prof.sct, 
              argv$dz.sct, 
              argv$DhorMin.sct,
              argv$DhorMax.sct, 
              argv$DhorKth.sct,
              argv$Dver.sct,
              eps2.sct[ix], 
              t2pos[ix],
              t2neg[ix],
              t2sod[ix] )
  if ( any( res[[1]] == 1 ))
    dqcflag[ix][ res[[1]] == 1 ] <- argv$sct.code
  t1a<-Sys.time()
  str<-"("
  for (f in 1:nfin) {
    if ( f > 1) str <- paste0( str, "; ")
    str <- paste( str, "prid", argv$prid[f], "=", 
            length( which( dqcflag==argv$sct.code & data$prid==argv$prid[f])))
  }
  str <- paste0( str, ")")
  cat( paste( "SCT/time", round(t1a-t0a,1), attr(t1a-t0a,"unit"), "\n"))
  cat( paste( "# suspect observations=", length( which( dqcflag==argv$sct.code)), str))
save(file="tmp.rdata", argv, y,x,z,yo_sct,b_sct,ix,res,dqcflag,data)
q()
  #
  # coefficient of observation representativeness
  #-----------------------------------------------------------------------------
  # corep has been set by function sct to the observation error variance
  if ( !any( is.na( argv$const.corep))) {
    for (f in 1:nfin) {
      if ( any( data$prid[ix] == argv$prid[f])) {
        ip <- which( data$prid[ix] == argv$prid[f])
        if ( length(ip) > 0) corep[ix[ip]] <- argv$const.corep[f]
      } else {
        print( paste( "provider", argv$prid[f],
        ": no valid data found to compute the coefficient of representativeness"))
      }
    }
  } else {
    corep     <- dqcflag; corep[] <- NA
    corep[ix] <- res[[2]]
    ix <- which( !is.na(corep) & 
                 (is.na(dqcflag) | dqcflag==argv$keep.code))
    if ( length( ix) > 0) {
      qmn    <- 0.25
      qmx    <- 0.75
      qav    <- 0.5
      acorep <- abs( corep[ix])
      # ecdf(x)(x) here should give us something similar to rank(x)/length(x)
      qcorep <- ecdf(acorep)(acorep)
      if ( any( qcorep < qmn)) qcorep[ which( qcorep < qmn)] <- qmn
      if ( any( qcorep > qmx)) qcorep[ which( qcorep > qmx)] <- qmx
      for (f in 1:nfin) {
        if ( any( data$prid[ix] == argv$prid[f])) {
          ip <- which( data$prid[ix] == argv$prid[f] & qcorep <= qav)
          if ( length( ip) > 0)
            corep[ix[ip]] <- argv$min.corep[f] +
                             ( argv$mean.corep[f] - argv$min.corep[f]) * 
                             ( qcorep[ip] - qmn) / ( qav - qmn)
          ip <- which( data$prid[ix] == argv$prid[f] & qcorep > qav)
          if ( length( ip) > 0)
            corep[ix[ip]] <- argv$mean.corep[f] +
                             ( argv$max.corep[f] - argv$mean.corep[f]) * 
                             ( qcorep[ip] - qav) / ( qmx - qav)
        } else {
          print( paste("provider", argv$prid[f],
          ": no valid data found to compute the coefficient of representativeness"))
        }
      }
    } else {
      print("no valid first guess for the observation error variance found")
    }
  }
  #
  return(dqcflag)
}
