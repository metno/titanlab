  if (argv$verbose) print(paste0("precipitation (in-situ) and temperature (field) cross-check (",argv$ccrrt.code,")"))
  # read temperature from gridded field
  debug.file<-ifelse(argv$debug, file.path(argv$debug.dir,"input_data_cc_t2m.RData"), NA)
  res<-get_data_from_ncfile(nc.file=argv$t2m.file,
                            nc.varname=argv$t2m.varname,
                            nc.t=argv$t2m.t,
                            nc.e=argv$t2m.e,
                            topdown=argv$t2m.topdown,
                            var.dim=list(ndim=argv$t2m.ndim,
                                         tpos=argv$t2m.tpos,
                                         epos=argv$t2m.epos,
                                         names=argv$t2m.dimnames),
                            var.de_acc=FALSE,
                            var.de_acc_by="",
                            proj4=proj4t2m,
                            proj4_from_nc=proj4t2m_from_nc,
                            xy_as_vars=t2m.xy_as_vars,
                            x_as_var.varname=argv$t2m.x_as_var.varname,
                            y_as_var.varname=argv$t2m.y_as_var.varname,
                            xy_as_var.dim=list(ndim=argv$t2m.xy_as_var.ndim,
                                               tpos=argv$t2m.xy_as_var.tpos,
                                               epos=NULL,
                                               names=argv$t2m.xy_as_var.dimnames),
                            xy_as_var.dh_max=NA,
                            return_raster=F,
                            debug.file=debug.file,
                            nc.dqc_mode="none") 
  t2m<-t2m.offset+ res*t2m.cfact; rm(res)
  # read elevation from gridded field
  debug.file<-ifelse(argv$debug, file.path(argv$debug.dir,"input_data_cc_t2mdem.RData"), NA)
  res<-get_data_from_ncfile(nc.file=argv$t2m.demfile,
                            nc.varname=argv$t2m.demvarname,
                            nc.t=argv$t2m.demt,
                            nc.e=argv$t2m.deme,
                            topdown=argv$t2m.demtopdown,
                            var.dim=list(ndim=argv$t2m.demndim,
                                         tpos=argv$t2m.demtpos,
                                         epos=argv$t2m.demepos,
                                         names=argv$t2m.demdimnames),
                            var.de_acc=FALSE,
                            var.de_acc_by="",
                            proj4=proj4t2m,
                            proj4_from_nc=proj4t2m_from_nc,
                            xy_as_vars=t2m.xy_as_vars,
                            x_as_var.varname=argv$t2m.x_as_var.varname,
                            y_as_var.varname=argv$t2m.y_as_var.varname,
                            xy_as_var.dim=list(ndim=argv$t2m.xy_as_var.ndim,
                                               tpos=argv$t2m.xy_as_var.tpos,
                                               epos=NULL,
                                               names=argv$t2m.xy_as_var.dimnames),
                            xy_as_var.dh_max=NA,
                            return_raster=F,
                            debug.file=debug.file,
                            nc.dqc_mode="none") 
  zt2mdem<-t2m.demoffset+ res*t2m.demcfact; rm(res)
  t2m<-t2m+argv$gamma.standard*(z-zt2mdem)
  # cross-check
  for (f in 1:nfin) 
    dqcflag[which(data$prid==argv$prid[f] & 
                  t2m<argv$ccrrt.tmin[f] &
                  is.na(dqcflag))]<-argv$ccrrt.code
  #
  if (argv$verbose) {
    print("precipitaton and temperature  crosscheck")
    print(paste("temp thresholds =",toString(argv$ccrrt.tmin)))
    print(paste("# suspect observations=",
          length(which(dqcflag==argv$ccrrt.code & !is.na(dqcflag)))))
    print("+---------------------------------+")
  }
  if (argv$debug) save.image(file.path(argv$debug.dir,"dqcres_ccrrt.RData")) 
  if (argv$rr.wcor) { rm(zt2mdem) } else { rm(t2m,zt2mdem) }

