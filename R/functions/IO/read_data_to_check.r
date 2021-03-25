#+ read input data (metadata and observations)
read_data_to_check <- function( argv)
#==============================================================================
# input arguments:
# argv. list with the input arguments (command line and/or config file)
# nfin. number of input files
# 
# output values:
# list of variables
# data. data frame: lat, lon, elev, value, prid
# dqcflag. numeric vector. data quality control flags
# z. numeric vector. elevation
# sctpog. numeric vector. spatial consistency test, probability of gross-error
# corep. numeric vector. coefficient of representativeness
# varidx. auxiliary variable used to write output
# varidx.opt. auxiliary variable used to write output
# dataopt. auxiliary variable used to write output
# extent. numeric vector. lonmin, lonmax, latmin, latmax
#==============================================================================
{

  nfin <- length( argv$input.files)

  first <- T

  # loop over input files (i.e. observation providers)
  for (f in 1:nfin) {

    datain <- read.table( file   = argv$input.files[f],
                          header = T,
                          sep    = argv$separator[f],
                          stringsAsFactors = F,
                          strip.white      = T)

    # if elev is not present then create a fake one
    varidxtmp <- match( argv$varname.elev[f], names(datain))
    if (is.na(varidxtmp)) {
      argv$varname.elev[f] <- "elev"
      if (argv$elev_not_used) {
        datain$elev <- rep(  0, length = length( datain[[1]]))
      } else {
        datain$elev <- rep( NA, length = length( datain[[1]]))
      }
    }
    rm(varidxtmp)

    # varidx is used also in the output session
    varidxtmp<-match( c( argv$varname.lat[f],
                         argv$varname.lon[f],
                         argv$varname.elev[f],
                         argv$varname.value[f]),
                      names(datain) )
    if (any(is.na(varidxtmp))) {
      print("ERROR in the specification of the variable names")
      print(paste(" latitude=",argv$varname.lat[f]))
      print(paste("longitude=",argv$varname.lon[f]))
      print(paste("elevation=",argv$varname.elev[f]))
      print(paste("    value=",argv$varname.value[f]))
      print("header of input file:")
      print(argv$input.files[f])
      print(names(datain))
  #    boom()
      next
    }

    # build up the temporary data structure
    datatmp <- data.frame( datain[,varidxtmp])

    names(datatmp) <- c( "lat", "lon", "elev", "value")

    datatmp$lat <- suppressWarnings( as.numeric( datatmp$lat))
    datatmp$lon <- suppressWarnings( as.numeric( datatmp$lon))

    if (argv$elev_not_used) {
      datatmp$elev <- rep( 0, length(datatmp$lon))
    } else {
      datatmp$elev <- suppressWarnings( as.numeric( datatmp$elev))
    }
    auxz <- suppressWarnings( as.numeric( datatmp$elev))

    datatmp$value <- suppressWarnings(
      argv$input.offset[f] + argv$input.cfact[f] * as.numeric(datatmp$value))

    ndatatmp <- length( datatmp$lat)

    if (ndatatmp==0) next

    # set provider id
    datatmp$prid <- as.numeric( rep(argv$prid[f], ndatatmp))
    aux <- rep( NA, length=ndatatmp)
    # blacklist
    if ( any(!is.na(argv$blacklist.idx)) & 
         any(argv$blacklist.fidx==argv$prid[f])) {
      aux[argv$blacklist.idx[which(argv$blacklist.fidx==argv$prid[f])]] <- argv$code.black
    }
    if (any(!is.na(argv$blacklist.lat)) & 
        any(argv$blacklist.fll==argv$prid[f])) {
      out<-apply(cbind(argv$blacklist.lon[which(argv$blacklist.fll==argv$prid[f])],
                       argv$blacklist.lat[which(argv$blacklist.fll==argv$prid[f])])
                 ,FUN=setCode_lonlat,MARGIN=1,code=argv$code.black)
      rm(out)
    }
    # keep-list
    if (any(!is.na(argv$keeplist.idx)) & 
        any(argv$keeplist.fidx==argv$prid[f])) {
      aux[argv$keeplist.idx[which(argv$keeplist.fidx==argv$prid[f])]]<-argv$code.keep  
    }
    if (any(!is.na(argv$keeplist.lat)) & 
        any(argv$keeplist.fll==argv$prid[f])) {
      out<-apply(cbind(argv$keeplist.lon[which(argv$keeplist.fll==argv$prid[f])],
                       argv$keeplist.lat[which(argv$keeplist.fll==argv$prid[f])])
                 ,FUN=setCode_lonlat,MARGIN=1,code=argv$code.keep)
      rm(out)
    }

    # ensure no duplicates
    if (argv$no_duplicates) {
      is_duplicate<-function(i) { dup_diff_datasources<-F
                                  if (exists("data",mode="data.frame"))
                                    dup_diff_datasources<-
                                     any( (abs(datatmp$lat[i]-data$lat) < argv$dup.match_tol_x |
                                           datatmp$lat[i]==data$lat) & 
                                          (abs(datatmp$lon[i]-data$lon) < argv$dup.match_tol_x |
                                           datatmp$lon[i]==data$lon) &
                                          (abs(datatmp$elev[i]-data$elev) < argv$dup.match_tol_z |
                                           datatmp$elev[i]==data$elev) )
                                  dup_same_datasource<-F
                                  if (i<ndatatmp)
                                    dup_same_datasource<-
                                     any( (abs(datatmp$lat[i]-datatmp$lat[(i+1):ndatatmp]) < argv$dup.match_tol_x |
                                           datatmp$lat[i]==datatmp$lat[(i+1):ndatatmp]) & 
                                          (abs(datatmp$lon[i]-datatmp$lon[(i+1):ndatatmp]) < argv$dup.match_tol_x |
                                           datatmp$lon[i]==datatmp$lon[(i+1):ndatatmp]) &
                                          (abs(datatmp$elev[i]-datatmp$elev[(i+1):ndatatmp]) < argv$dup.match_tol_z |
                                           datatmp$elev[i]==datatmp$elev[(i+1):ndatatmp]) ) 
                                  return(dup_diff_datasources | dup_same_datasource) }
      if (!is.na(argv$cores)) {
        dup<-mcmapply(is_duplicate,
                      1:ndatatmp,
                      mc.cores=argv$cores,
                      SIMPLIFY=T)
      # no-multicores
      } else {
        dup<-mapply(is_duplicate,
                    1:ndatatmp,
                    SIMPLIFY=T)
      }
      ix_nodup<-which(!dup)
    } else {
      ix_nodup<-1:ndatatmp
    }
   
    # create and update the definitve data structure 
    ndatatmp<-length(ix_nodup)
    
    if ( ndatatmp > 0) {

      # datatmp$ lat lon elev value prid
      datatmp <- data.frame( lat   = suppressWarnings( as.numeric( datatmp$lat[ix_nodup])),
                             lon   = suppressWarnings( as.numeric( datatmp$lon[ix_nodup])),
                             elev  = suppressWarnings( as.numeric( datatmp$elev[ix_nodup])),
                             value = suppressWarnings( as.numeric( datatmp$value[ix_nodup])),
                             prid  = suppressWarnings( as.numeric( datatmp$prid[ix_nodup])))
      if ( first) {
        varidx  <- varidxtmp
        data    <- datatmp
        z       <- auxz[ix_nodup]
        dqcflag <- aux[ix_nodup]
        sctpog  <- rep(NA,length=ndatatmp)
        corep   <- rep(NA,length=ndatatmp)
        if ( any( !is.na( argv$varname.opt))) {
          # varidx.opt is used in the output session
          varidx.opt <- match( argv$varname.opt, names(datain))
          dataopt    <- as.data.frame( array(data=NA,
                                             dim=c(ndatatmp,length(argv$varname.opt))))
          names(dataopt) <- argv$varname.opt
          if ( any( !is.na( varidx.opt)))
            dataopt <- datain[ix_nodup,varidx.opt[which(!is.na(varidx.opt))],drop=F]
        }
        first <- F

      } else {

        data    <- rbind( data, datatmp)
        dqcflag <- c(  dqcflag, aux[ix_nodup])
        z       <- c(        z, auxz[ix_nodup])
        sctpog  <- c( sctpog, rep( NA, length=ndatatmp))
        corep   <- c( corep,  rep( NA, length=ndatatmp))

        # dataopt are auxiliary data that are not used in titanlib
        if ( any( !is.na( argv$varname.opt)) ) {
          varidx.opt.check <- match( argv$varname.opt, names(datain))
          if ( any(!is.na(varidx.opt.check) & is.na(varidx.opt)) ) {
            ixopt <- which( !is.na( varidx.opt.check) & is.na( varidx.opt))
            for (iopt in ixopt) {
              if ( varidx.opt.check[iopt] %in% varidx.opt) {
                varidx.opt[iopt] <- max(varidx.opt,na.rm=T)+1
              } else { 
                varidx.opt[iopt] <- varidx.opt.check[iopt]
              }
            }
            rm(ixopt,iopt)
          }
          dataopttmp <- as.data.frame( array(data=NA,
                                       dim=c(ndatatmp,length(argv$varname.opt))))
          names(dataopttmp)<-argv$varname.opt
          if (any(!is.na(varidx.opt.check)))
            dataopttmp<-datain[ix_nodup,varidx.opt.check[which(!is.na(varidx.opt.check))],
                               drop=F]
          dataopt<-rbind(dataopt,
                         dataopttmp)
          rm(dataopttmp)
        }
      }
    } # create and update the definitve data structure

    if (exists("ix_nodup")) rm(ix_nodup)
    if (exists("varidxtmp")) rm(varidxtmp)

  } # END loop over input files (i.e. observation providers)

  rm( datatmp, datain, auxz, aux)

  ndata<-length(data$lat)
  if (ndata==0) {
    print("input file is empty")
    quit(status=0)
  }
  #
  # set domain extent for SCT and metadata tests
  argv$lonmin<-strings_to_numbers(strings=argv$lonmin)
  argv$lonmax<-strings_to_numbers(strings=argv$lonmax)
  argv$latmin<-strings_to_numbers(strings=argv$latmin)
  argv$latmax<-strings_to_numbers(strings=argv$latmax)
  if (argv$dqc_inbox_only) {
    extent_lonmin<-argv$lonmin
    extent_lonmax<-argv$lonmax
    extent_latmin<-argv$latmin
    extent_latmax<-argv$latmax
  } else {
    extent_lonmin<-min(data$lon,na.rm=T)
    extent_lonmax<-max(data$lon,na.rm=T)
    extent_latmin<-min(data$lat,na.rm=T)
    extent_latmax<-max(data$lat,na.rm=T)
  }

  if (argv$verbose | argv$debug) {
    print(paste("number of observations=",ndata))
    if (any(!is.na(argv$blacklist.idx)) | any(!is.na(argv$blacklist.lat)))
      print(paste("number of blacklisted observations=",
            length(which(dqcflag==argv$code.black))) )
    if (any(!is.na(argv$keeplist.idx)) | any(!is.na(argv$keeplist.lat)))
      print(paste("number of keeplisted  observations=",
            length(which(dqcflag==argv$code.keep))) )
    if (nfin>1) {
      for (f in 1:nfin) { 
        print(paste("  number of observations provider",argv$prid[f],"=",
              length(which(data$prid==argv$prid[f]))))
        if (any(!is.na(argv$blacklist.idx)) | any(!is.na(argv$blacklist.lat)))
          print(paste("  number of blacklisted observations provider",
                argv$prid[f],"=",
                length(which(data$prid==argv$prid[f] & dqcflag==argv$code.black))) )
        if (any(!is.na(argv$keeplist.idx)) | any(!is.na(argv$keeplist.lat)))
          print(paste("  number of keeplisted  observations provider",
                argv$prid[f],"=",
                length(which(data$prid==argv$prid[f] & dqcflag==argv$code.keep))) )
      }
    }
    print(paste("extension of the domain considered (xmin,xmax,ymin,ymax)=",
                 round(extent_lonmin,6),",",round(extent_lonmax,6),",",
                 round(extent_latmin,6),",",round(extent_latmax,6)))
    print("+---------------------------------+")
  }
  #
  if (!exists("varidx.opt")) varidx.opt <- NULL
  if (!exists("dataopt"))    dataopt    <- NULL
  return( list( data    = data, 
                dqcflag = as.integer( dqcflag),
                z       = as.numeric( z),
                sctpog  = as.numeric( sctpog),
                corep   = as.numeric( corep),
                varidx  = varidx,
                varidx.opt  = varidx.opt,
                dataopt = dataopt,
                extent  = c( extent_lonmin, extent_lonmax, 
                             extent_latmin, extent_latmax)))
}
