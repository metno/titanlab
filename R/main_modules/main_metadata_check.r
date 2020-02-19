# NOTE: keep-listed stations could be flagged here
ix<-which(is.na(dqcflag) | dqcflag==argv$keep.code)
if (length(ix)>0) {
  meta<-is.na(data$lat[ix]) | 
        is.na(data$lon[ix]) |
        is.na(z[ix]) | 
        z[ix]<argv$zmin | 
        z[ix]>argv$zmax |
        is.na(data$value[ix]) 
  if (argv$dqc_inbox_only) 
    meta<- meta | ( data$lat[ix] < extent_latmin | 
                    data$lat[ix] > extent_latmax |
                    data$lon[ix] < extent_lonmin | 
                    data$lon[ix] > extent_lonmax )
  if (any(meta)) dqcflag[ix[which(meta)]]<-argv$nometa.code
} else {
  print("no valid observations left, no metadata check")
}
if (argv$verbose) {
  flagaux<-dqcflag==argv$nometa.code & !is.na(dqcflag)
  print("test for no metdata, statistics over the whole dataset")
  print(paste("# observations lacking metadata and/or NAs=",
        length(which(flagaux))))
  print(paste("  # NAs                 =",
        length(which(flagaux & is.na(data$value))))) # coincides with all the NAs
  if (argv$dqc_inbox_only) {
    print(paste("  # lon-lat missing (*) =",
          length(which(flagaux & (is.na(data$lat) | 
                                  is.na(data$lon) | 
                                  data$lat < extent_latmin | 
                                  data$lat > extent_latmax | 
                                  data$lon < extent_lonmin | 
                                  data$lon > extent_lonmax ) ))))
  } else {
    print(paste("  # lon-lat missing =",
          length(which(flagaux & (is.na(data$lat) | 
                                  is.na(data$lon) ))))) 
  }
  print(paste("  # z missing           =",
        length(which(flagaux & is.na(z)))))
  print(paste("  # z out of range      =",
        length(which(flagaux & !is.na(z) & 
                     (z<argv$zmin | z>argv$zmax) ))))
  if (argv$dqc_inbox_only) 
    print("(*) or outside the specified box")
  rm(flagaux)
  print("+---------------------------------+")
}
rm(ix)
if (exists("meta")) rm(meta)

