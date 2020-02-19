if (argv$spatconv) {
  if (argv$debug) print("conversion of spatial coordinates")
  # initialization
  x<-data$lon
  y<-data$lon
  x[]<-NA
  y[]<-NA
  # do it
  coord<-SpatialPoints(cbind(data$lon,data$lat),
                       proj4string=CRS(argv$proj4_input_obsfiles))
  coord.new<-spTransform(coord,CRS(argv$proj4_where_dqc_is_done))
  xy.new<-coordinates(coord.new)
  x<-round(xy.new[,1],0)
  y<-round(xy.new[,2],0)
  xp<-expand.grid(c(extent_lonmin,extent_lonmax),
                  c(extent_latmin,extent_latmax))
  coord<-SpatialPoints(xp,
                       proj4string=CRS(argv$proj4_input_obsfiles))
  coord.new<-spTransform(coord,CRS(argv$proj4_where_dqc_is_done))
  # define the extent for the SCT grid
  e<-extent(coord.new)
  xl<-e[1:2]
  yl<-e[3:4]
  rm(coord,coord.new,xy.new,xp)
} else {
  x<-data$lon
  y<-data$lat
  xl<-c(extent_lonmin,extent_lonmax)
  yl<-c(extent_latmin,extent_latmax)
  e<-extent(c(xl,yl))
}
if (argv$debug) save.image(file.path(argv$debug.dir,"input_data.RData")) 
if (argv$verbose) print("+---------------------------------+")

