  if (argv$verbose | argv$debug) 
    print("include radar-derived precipitation in the output file")
  # (optional) aggregate radar data onto a coarser grid
  if (!is.na(argv$radarout.aggfact) & argv$radarout.aggfact>1) {
    if (argv$radarout.aggfun=="ngb") {
      raux<-resample(rrad,
                     aggregate(rrad,
                               fact=argv$radarout.aggfact,
                               na.rm=T,
                               expand=T),
                     method="ngb")
    } else if (argv$radarout.aggfun=="modal") {
      raux<-aggregate(rrad,
                      fun=argv$radarout.aggfun,
                      ties='highest',
                      na.rm=T,
                      expand=T, 
                      fact=argv$radarout.aggfact)
    } else {
      raux<-aggregate(rrad,
                      fun=argv$radarout.aggfun,
                      na.rm=T,
                      expand=T, 
                      fact=argv$radarout.aggfact)
    }
    rrad<-raux
    rm(raux)
  }
  drad<-getValues(rrad)
  # get radar-point coordinates into output CRS 
  ix<-which(!is.na(drad)) 
  if (length(ix)) {
    radxy<-as.data.frame(xyFromCell(rrad,ix))
    names(radxy)<-c("x","y")
    coordinates(radxy)<-c("x","y")
    proj4string(radxy)<-CRS(argv$proj4fg)
    radxy.from<-as.data.frame(spTransform(radxy,CRS=argv$proj4_output_files))  
    radx.from<-radxy.from[,1]
    rady.from<-radxy.from[,2]
    radrr<-drad[ix]
  } else {
    radx.from<-integer(0)
    rady.from<-integer(0)
    radrr<-integer(0)
  }

