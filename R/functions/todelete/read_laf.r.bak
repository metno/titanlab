#+ read land area fraction
read_laf <- function( argv) { 

  if (argv$proj4laf=="" & argv$laf.proj4_var=="" & argv$laf.proj4_att=="" ) {
    laf.xy_as_vars<-T
    proj4laf<-NULL
    proj4laf_from_nc<-NULL
  } else {
    laf.xy_as_vars<-F
    proj4laf<-argv$proj4laf
    proj4laf_from_nc<-list(var=argv$laf.proj4_var, att=argv$laf.proj4_att)
  }

  if (argv$verbose) cat("read land area fraction\n")
  debug.file<-ifelse(argv$debug, file.path(argv$debug.dir,"lafnc.RData"), NA)
  laf<-get_data_from_ncfile(nc.file=argv$laf.file,
                            nc.varname=argv$laf.varname,
                            nc.t=NA,
                            nc.e=NA,
                            topdown=argv$laf.topdown,
                            var.dim=list(ndim=argv$laf.ndim,
                                         tpos=argv$laf.tpos,
                                         epos=NULL,
                                         names=argv$laf.dimnames),
                            proj4=proj4laf,
                            proj4_from_nc=proj4laf_from_nc,
                            xy_as_vars=laf.xy_as_vars,
                            x_as_var.varname=argv$laf.x_as_var.varname,
                            y_as_var.varname=argv$laf.y_as_var.varname,
                            xy_as_var.dim=list(ndim=argv$laf.xy_as_var.ndim,
                                               tpos=argv$laf.xy_as_var.tpos,
                                               epos=NULL,
                                               names=argv$laf.xy_as_var.dimnames),
                            xy_as_var.dh_max=NA,
                            debug.file=debug.file)
  laf<-laf/100
  if (any(is.na(laf))) laf[which(is.na(laf))]<-1
  if (argv$debug) save.image(file.path(argv$debug.dir,"input_data_laf.RData")) 
  if (argv$verbose) cat("+---------------------------------+\n")
  #
  return( laf)
}
