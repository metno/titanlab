  if (argv$verbose) print("read digital elevation model")
  debug.file<-ifelse(argv$debug, file.path(argv$debug.dir,"demnc.RData"), NA)
  zdem<-get_data_from_ncfile(nc.file=argv$dem.file,
                             nc.varname=argv$dem.varname,
                             nc.t=NA,
                             nc.e=NA,
                             topdown=argv$dem.topdown,
                             var.dim=list(ndim=argv$dem.ndim,
                                          tpos=argv$dem.tpos,
                                          epos=NULL,
                                          names=argv$dem.dimnames),
                             proj4=proj4dem,
                             proj4_from_nc=proj4dem_from_nc,
                             xy_as_vars=dem.xy_as_vars,
                             x_as_var.varname=argv$dem.x_as_var.varname,
                             y_as_var.varname=argv$dem.y_as_var.varname,
                             xy_as_var.dim=list(ndim=argv$dem.xy_as_var.ndim,
                                                tpos=argv$dem.xy_as_var.tpos,
                                                epos=NULL,
                                                names=argv$dem.xy_as_var.dimnames),
                             xy_as_var.dh_max=NA,
                             debug.file=debug.file)
  # fill missing elevation with dem
  if (argv$dem.fill) {
    iz<-which( !is.na(data$value) &
               !is.na(data$lat)   & 
               !is.na(data$lon)   &
               !is.na(zdem) &
               !(zdem<argv$zmin | zdem>argv$zmax) &
               (is.na(z) | is.nan(z) | z<argv$zmin | z>argv$zmax) )
    z[iz]<-zdem[iz]
    dqcflag[iz]<-dqcflag.bak[iz]
    rm(dqcflag.bak)
    if (argv$verbose) {
      print(paste("# stations with elevation derived from DEM=",length(iz)))
      print("+---------------------------------+")
    }
    rm(iz)
  }  
  if (argv$debug) save.image(file.path(argv$debug.dir,"dem.RData")) 

