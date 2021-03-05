#+ Read first-guess fields (ensemble case)
read_fge <- function( argv, extent) {
#==============================================================================

  if (argv$verbose) cat("Read ensemble first-guess\n")
  t0a<-Sys.time()

  # initializations

  fge.offset <- strings_to_numbers( strings = argv$fge.offset,default=0,
                                    neg     = argv$fge.negoffset)
  fge.cfact  <- strings_to_numbers( strings = argv$fge.cfact,default=0,
                                    neg     = argv$fge.negcfact)
  fge.demoffset <- strings_to_numbers( strings = argv$fge.demoffset,default=0,
                                       neg     = argv$fge.demnegoffset)
  fge.demcfact  <- strings_to_numbers( strings = argv$fge.demcfact,default=0,
                                       neg     = argv$fge.demnegcfact)
  rfge    <- NULL
  rfgedem <- NULL
  if (argv$proj4fge == "" & argv$fge.proj4_var == "" & argv$fge.proj4_att == "" ) {
    fge.xy_as_vars   <- T
    proj4fge         <- NULL
    proj4fge_from_nc <- NULL
  } else {
    fge.xy_as_vars   <- F
    proj4fge         <- argv$proj4fge
    proj4fge_from_nc <- list( var=argv$fge.proj4_var, att=argv$fge.proj4_att)
  }

  # temperature: adjust for elevation differences
  if ( !is.null( argv$fge.demfile)) {
    if ( file.exists( argv$fge.demfile)) {
      res <- get_data_from_ncfile( nc.file=argv$fge.demfile,
                                   nc.varname=argv$fge.demvarname,
                                   nc.t=argv$fge.demt,
                                   nc.e=argv$fge.deme,
                                   topdown=argv$fge.demtopdown,
                                   var.dim=list(ndim=argv$fge.demndim,
                                                tpos=argv$fge.demtpos,
                                                epos=argv$fge.demepos,
                                                names=argv$fge.demdimnames),
                                   proj4=proj4fge,
                                   proj4_from_nc=proj4fge_from_nc,
                                   xy_as_vars=fge.xy_as_vars,
                                   x_as_var.varname=argv$fge.x_as_var.varname,
                                   y_as_var.varname=argv$fge.y_as_var.varname,
                                   xy_as_var.dim=list(ndim=argv$fge.xy_as_var.ndim,
                                                      tpos=argv$fge.xy_as_var.tpos,
                                                      epos=NULL,
                                                      names=argv$fge.xy_as_var.dimnames),
                                   xy_as_var.dh_max=NA,
                                   return_raster=T,
                                   return_obsloc=F,
                                   extent=extent,
                                   debug.file=NA,
                                   nc.dqc_mode="none")
      if (!is.null( res))
        rfgedem <- fge.demoffset + res$raster * fge.demcfact 
      rm( res)
    }
  }

  #
  if ( is.na( argv$fge.epos)) boom("ERROR fge.epos must have a valid value")

  if ( file.exists( argv$fge.file)) {
    ei <- nc4.getDim( argv$fge.file, 
                      varid = argv$fge.dimnames[argv$fge.epos])
    rfge <- list()
    for (ens in 1:length(ei)) {
      res <- get_data_from_ncfile( nc.file=argv$fge.file,
                                   nc.varname=argv$fge.varname,
                                   nc.t=argv$fge.t,
                                   nc.e=ei[ens],
                                   topdown=argv$fge.topdown,
                                   var.dim=list(ndim=argv$fge.ndim,
                                                tpos=argv$fge.tpos,
                                                epos=argv$fge.epos,
                                                names=argv$fge.dimnames),
                                   var.de_acc=argv$fge.acc,
                                   var.de_acc_by="-1 hour",
                                   proj4=proj4fge,
                                   proj4_from_nc=proj4fge_from_nc,
                                   xy_as_vars=fge.xy_as_vars,
                                   x_as_var.varname=argv$fge.x_as_var.varname,
                                   y_as_var.varname=argv$fge.y_as_var.varname,
                                   xy_as_var.dim=list(ndim=argv$fge.xy_as_var.ndim,
                                                      tpos=argv$fge.xy_as_var.tpos,
                                                      epos=NULL,
                                                      names=argv$fge.xy_as_var.dimnames),
                                   xy_as_var.dh_max=NA,
                                   return_raster=T,
                                   return_obsloc=F,
                                   extent=extent,
                                   debug.file=NA,
                                   nc.dqc_mode="none")
      if ( !is.null( res))
        rfge[[ens]] <- fge.offset + res$raster * fge.cfact
      rm(res)
    } # end of cycle over ensemble members
  }
  if (argv$verbose) {
    t1a<-Sys.time()
    cat( paste( "FGe   dim = ", ncell( rfge[[1]]), "\n"))
    for (i in 1:length(ei))
      cat( paste( "FGe i range = ", i, round( min( getValues( rfge[[i]]), na.rm=T), 2), 
                                      round( max( getValues( rfge[[i]]), na.rm=T), 2), "\n"))
    if ( !is.null( rfgdem)) {
      cat( paste( "FGedem   dim = ", ncell( rfgedem), "\n"))
      cat( paste( "FGedem range = ", round( min( getValues( rfgedem), na.rm=T), 2), 
                                     round( max( getValues( rfgedem), na.rm=T), 2), "\n"))
    }
    cat( paste( "time", round(t1a-t0a,1), attr(t1a-t0a,"unit"), "\n"))
    cat( "+---------------------------------+\n")
  }
  return( list( rfge      = rfge,
                rfgedem   = rfgedem))
}
