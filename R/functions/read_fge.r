#+
read_fge <- function( argv) {
#==============================================================================
  if (argv$verbose) print("Read ensemble first-guess")
  t0a<-Sys.time()

  fge.offset<-strings_to_numbers(strings=argv$fge.offset,default=0,
                                         neg=argv$fge.negoffset)
  fge.cfact<-strings_to_numbers(strings=argv$fge.cfact,default=0,
                                        neg=argv$fge.negcfact)
  fge.demoffset<-strings_to_numbers(strings=argv$fge.demoffset,default=0,
                                            neg=argv$fge.demnegoffset)
  fge.demcfact<-strings_to_numbers(strings=argv$fge.demcfact,default=0,
                                           neg=argv$fge.demnegcfact)


  if (argv$proj4fge=="" & argv$fge.proj4_var=="" & argv$fge.proj4_att=="" ) {
    fge.xy_as_vars<-T
    proj4fge<-NULL
    proj4fge_from_nc<-NULL
  } else {
    fge.xy_as_vars<-F
    proj4fge<-argv$proj4fge
    proj4fge_from_nc<-list(var=argv$fge.proj4_var, att=argv$fge.proj4_att)
  }

  # temperature: adjust for elevation differences
  if (argv$variable=="T") {
    debug.file<-ifelse(argv$debug, file.path(argv$debug.dir,"fgedemnc.RData"), NA)
    zfgedem<-get_data_from_ncfile(nc.file=argv$fge.demfile,
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
                                 return_raster=F,
                                 debug.file=debug.file,
                                 nc.dqc_mode="none")
    zfgedem<-fge.demoffset+ zfgedem * fge.demcfact
  }
  #
  if (is.na(argv$fge.epos)) boom("ERROR fge.epos must have a valid value")
  ei<-nc4.getDim(argv$fge.file, varid=argv$fge.dimnames[argv$fge.epos])
  edata<-array(data=NA,dim=c(ndata,length(ei)))
  first<-T
  for (ens in 1:length(ei)) {
    debug.file<-ifelse(argv$debug, 
                       file.path(argv$debug.dir,
                        paste0("input_data_fge_member",
                               formatC(ei[ens],width=2,flag="0"),".RData")), NA)
    res<-get_data_from_ncfile(nc.file=argv$fge.file,
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
                              return_raster=F,
                              debug.file=debug.file,
                              nc.dqc_mode="none")
    edata[,ens]<-fge.offset+ res* fge.cfact
    if (argv$variable=="T") 
      edata[,ens]<-edata[,ens]+ argv$gamma.standard * (z-zfgedem)
    rm(res)
  } # end of cycle over ensemble members
  # compute mean and sd 
  fge.mu<-rowMeans(edata,na.rm=T)
  fge_sd<-function(i) { sd(edata[i,],na.rm=T) }
  if (!is.na(argv$cores)) {
    fge.sd<-mcmapply(fge_sd,
                     1:dim(edata)[1],
                     mc.cores=argv$cores,
                     SIMPLIFY=T)
  # no-multicores
  } else {
    fge.sd<-mapply(fge_sd,
                   1:dim(edata)[1],
                   SIMPLIFY=T)
  }
  fge.sd<-pmax(fge.sd,argv$sdmin.fge,na.rm=T)
  rm(edata)
  if (argv$debug) save.image(file.path(argv$debug.dir,"input_data_fge.RData")) 
  if (argv$verbose) {
    t1a<-Sys.time()
    print(paste("fge sd(5,25,50,75,95-th)=",
          toString(round(as.vector(quantile(fge.sd,
                             probs=c(0.05,0.25,0.5,0.75,0.95),
                             na.rm=T)),2))))
    print(paste("time",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
    print("+---------------------------------+")
  }
  return( list( fge.mu=fge.mu, fge.sd=fge.sd ))
}
