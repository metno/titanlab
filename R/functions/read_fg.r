#+ Read first-guess field (deterministic case)
read_fg <- function( argv, extent) {
#=============================================================================

  if ( argv$verbose) cat( "Read deterministic first-guess\n")
  t0a <- Sys.time()

  # initializations
  fg.offset <- strings_to_numbers( strings = argv$fg.offset, default=0,
                                   neg     = argv$fg.negoffset)
  fg.cfact  <- strings_to_numbers( strings = argv$fg.cfact, default=0,
                                   neg     = argv$fg.negcfact)
  fg.demoffset <- strings_to_numbers( strings = argv$fg.demoffset, default=0,
                                      neg     = argv$fg.demnegoffset)
  fg.demcfact  <- strings_to_numbers( strings = argv$fg.demcfact, default=0,
                                      neg     = argv$fg.demnegcfact)
  rfg    <- NULL
  rfgdem <- NULL
  if ( argv$proj4fg=="" & argv$fg.proj4_var=="" & argv$fg.proj4_att=="" ) {
    fg.xy_as_vars   <- T
    proj4fg         <- NULL
    proj4fg_from_nc <- NULL
  } else {
    fg.xy_as_vars   <- F
    proj4fg         <- argv$proj4fg
    proj4fg_from_nc <- list( var=argv$fg.proj4_var, att=argv$fg.proj4_att)
  }

  nc.dqc_mode <- "none"
  if ( !is.na( argv$fg.type) & argv$fg.type == "radar" & argv$fg.dodqc) 
    nc.dqc_mode <- "radar_hourly"

  debug.file <- ifelse( argv$debug, file.path( argv$debug.dir,"input_data_fg0.RData"), NA)

  # temperature: adjust for elevation differences
  if ( !is.null( argv$fg.demfile)) {
    if ( file.exists( argv$fg.demfile)) {
      res <- get_data_from_ncfile( nc.file=argv$fg.demfile,
                                   nc.varname=argv$fg.demvarname,
                                   nc.t=argv$fg.demt,
                                   nc.e=argv$fg.deme,
                                   topdown=argv$fg.demtopdown,
                                   var.dim=list(ndim=argv$fg.demndim,
                                                tpos=argv$fg.demtpos,
                                                epos=argv$fg.demepos,
                                                names=argv$fg.demdimnames),
                                   proj4=proj4fg,
                                   proj4_from_nc=proj4fg_from_nc,
                                   xy_as_vars=fg.xy_as_vars,
                                   x_as_var.varname=argv$fg.x_as_var.varname,
                                   y_as_var.varname=argv$fg.y_as_var.varname,
                                   xy_as_var.dim=list(ndim=argv$fg.xy_as_var.ndim,
                                                      tpos=argv$fg.xy_as_var.tpos,
                                                      epos=NULL,
                                                      names=argv$fg.xy_as_var.dimnames),
                                   xy_as_var.dh_max=NA,
                                   return_raster=T,
                                   return_obsloc=F,
                                   extent=extent,
                                   debug.file=debug.file,
                                   nc.dqc_mode="none")
      if ( !is.null( res)) 
        rfgdem <- fg.demoffset + res$raster * fg.demcfact
      rm( res)
    }
  }
  
  if ( file.exists( argv$fg.file)) {
    res <- get_data_from_ncfile( nc.file=argv$fg.file,
                                 nc.varname=argv$fg.varname,
                                 nc.t=argv$fg.t,
                                 nc.e=argv$fg.e,
                                 topdown=argv$fg.topdown,
                                 var.dim=list(ndim=argv$fg.ndim,
                                              tpos=argv$fg.tpos,
                                              epos=argv$fg.epos,
                                              names=argv$fg.dimnames),
                                 var.de_acc=argv$fg.acc,
                                 var.de_acc_by="-1 hour",
                                 proj4=proj4fg,
                                 proj4_from_nc=proj4fg_from_nc,
                                 xy_as_vars=fg.xy_as_vars,
                                 x_as_var.varname=argv$fg.x_as_var.varname,
                                 y_as_var.varname=argv$fg.y_as_var.varname,
                                 xy_as_var.dim=list(ndim=argv$fg.xy_as_var.ndim,
                                                    tpos=argv$fg.xy_as_var.tpos,
                                                    epos=NULL,
                                                    names=argv$fg.xy_as_var.dimnames),
                                 xy_as_var.dh_max=NA,
                                 return_raster=T,
                                 return_obsloc=F,
                                 extent=extent,
                                 debug.file=debug.file,
                                 nc.dqc_mode=nc.dqc_mode) 
    if ( !is.null( res)) 
      rfg <- fg.offset + res$raster * fg.cfact
  }
  if (argv$verbose) {
    t1a <- Sys.time()
    cat( paste( "FG   dim = ", ncell( rfg), "\n"))
    cat( paste( "FG range = ", round( min( getValues( rfg), na.rm=T), 2), 
                               round( max( getValues( rfg), na.rm=T), 2), "\n"))
    if ( !is.null( rfgdem)) {
      cat( paste( "FGdem   dim = ", ncell( rfgdem), "\n"))
      cat( paste( "FGdem range = ", round( min( getValues( rfgdem), na.rm=T), 2), 
                                    round( max( getValues( rfgdem), na.rm=T), 2), "\n"))
    }
    cat( paste( " time", round( t1a-t0a, 1), attr( t1a-t0a, "unit"), "\n"))
    cat("+---------------------------------+\n")
  }

  return( list( rfg      = rfg,
                rfgdem   = rfgdem))
}
