#+
read_fg <- function( argv) {
  fg.offset<-strings_to_numbers(strings=argv$fg.offset,default=0,
                                        neg=argv$fg.negoffset)
  fg.cfact<-strings_to_numbers(strings=argv$fg.cfact,default=0,
                                       neg=argv$fg.negcfact)
  fg.demoffset<-strings_to_numbers(strings=argv$fg.demoffset,default=0,
                                           neg=argv$fg.demnegoffset)
  fg.demcfact<-strings_to_numbers(strings=argv$fg.demcfact,default=0,
                                          neg=argv$fg.demnegcfact)
  if (argv$proj4fg=="" & argv$fg.proj4_var=="" & argv$fg.proj4_att=="" ) {
    fg.xy_as_vars<-T
    proj4fg<-NULL
    proj4fg_from_nc<-NULL
  } else {
    fg.xy_as_vars<-F
    proj4fg<-argv$proj4fg
    proj4fg_from_nc<-list(var=argv$fg.proj4_var, att=argv$fg.proj4_att)
  }

  if (argv$verbose) print("Read deterministic first-guess")
  nc.dqc_mode<-"none"
  if (!is.na(argv$fg.type) & argv$fg.type=="radar" & argv$fg.dodqc) 
    nc.dqc_mode<-"radar_hourly"
  debug.file<-ifelse(argv$debug, file.path(argv$debug.dir,"input_data_fg0.RData"), NA)
  res<-get_data_from_ncfile(nc.file=argv$fg.file,
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
                            debug.file=debug.file,
                            nc.dqc_mode=nc.dqc_mode) 
  rfg<-res$raster
  fg<-res$values # this has the dimension of the observation vector
  rrad<-NA
  if (argv$radarout) rrad<-rfg
  rm(res)
  fg<-fg.offset+ fg * fg.cfact
  if (argv$cool & argv$usefg.cool) {
    aux<-fg.offset+ getValues(rfg) * fg.cfact
    ix_aux<-which(!is.na(aux)& !is.nan(aux) & is.finite(aux) & is.numeric(aux))
    if (!exists("xobs_cool_aux")) xobs_cool_aux<-integer(0)
    if (!exists("yobs_cool_aux")) yobs_cool_aux<-integer(0)
    if (!exists("pridobs_cool_aux")) pridobs_cool_aux<-integer(0)
    if (!exists("yo_cool_aux")) yo_cool_aux<-integer(0)
    if (length(ix_aux)>0) {
      yo_cool_aux<-c(yo_cool_aux,aux[ix_aux])
      xygrid_cool<-xyFromCell(rfg,ix_aux)
      xobs_cool_aux<-c(xobs_cool_aux,xygrid_cool[,1])
      yobs_cool_aux<-c(yobs_cool_aux,xygrid_cool[,2])
      pridobs_cool_aux<-c(pridobs_cool_aux,rep(NA,length(ix_aux)))
    }
    rm(aux,ix_aux)
  }
  if (argv$debug) save.image(file.path(argv$debug.dir,"input_data_fg1.RData")) 
  # temperature: adjust for elevation differences
  if (argv$variable=="T") {
    debug.file<-ifelse(argv$debug, file.path(argv$debug.dir,"fgdemnc.RData"), NA)
    ref<-get_data_from_ncfile(nc.file=argv$fg.demfile,
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
#                              return_raster=F,
                              return_raster=T,
                              debug.file=debug.file,
                              nc.dqc_mode="none")
    rfgdem<-res$raster
    fg<-res$values # this has the dimension of the observation vector
    zfgdem<-fg.demoffset+ res$values * fg.demcfact
    fg<-fg+argv$gamma.standard*(z-zfgdem)
    rm(zfgdem,res)
  }
  if (argv$debug) save.image(file.path(argv$debug.dir,"input_data_fg.RData"))
  if (argv$verbose) print("+---------------------------------+")
  return( list( fg=fg,
                rrad=rrad,
                cool_aux=list( yo_cool_aux=yo_cool_aux,
                               xobs_cool_aux=xobs_cool_aux,
                               yobs_cool_aux=yobs_cool_aux,
                               pridobs_cool_aux=pridobs_cool_aux)))
}
