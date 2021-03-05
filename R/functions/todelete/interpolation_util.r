#+ nearest-neighbour spatial interpolation
spint_nn<-function(xy,rx,ry,rval,dh_max) {
  deltax<-abs(xy[1]-rx); deltay<-abs(xy[2]-ry)
  if (!any(deltax<dh_max & deltay<dh_max)) return(NA)
  if (any(deltax<(dh_max/20) & deltay<(dh_max/20))) {
    ixnear<-which(deltax<(dh_max/20) & deltay<(dh_max/20))
  } else if (any(deltax<(dh_max/10) & deltay<(dh_max/10))) {
    ixnear<-which(deltax<(dh_max/10) & deltay<(dh_max/10))
  } else {
    ixnear<-which(deltax<(dh_max) & deltay<(dh_max))
  }
  ixnearest<-which.min(deltax[ixnear]*deltax[ixnear]+
                       deltay[ixnear]*deltay[ixnear])
  return(rval[ixnear[ixnearest]])
}

#+ spatial interpolation for cool test 
spint_cool<-function(i,
                     thres,  # same unit as yo_cool
                     dh_max, # max distance to be considered a nn
                     condition="lt",
                     mode="nearest") {
#------------------------------------------------------------------------------
# spatial interpolation for the cool test. Defualt is nearest neighbour.
#------------------------------------------------------------------------------
  if (mode=="nearest") { 
    deltax<-abs(xgrid_cool[i]-xobs_cool)
    deltay<-abs(ygrid_cool[i]-yobs_cool)
    if (!any(deltax<dh_max & deltay<dh_max)) return(NA)
    if (any(deltax<(dh_max/20) & deltay<(dh_max/20))) {
      ixnear<-which(deltax<(dh_max/20) & deltay<(dh_max/20))
    } else if (any(deltax<(dh_max/10) & deltay<(dh_max/10))) {
      ixnear<-which(deltax<(dh_max/10) & deltay<(dh_max/10))
    } else {
      ixnear<-which(deltax<(dh_max) & deltay<(dh_max))
    }
    ixnearest<-which.min(deltax[ixnear]*deltax[ixnear]+
                         deltay[ixnear]*deltay[ixnear])
#    if (sqrt((xgrid_cool[i]-xobs_cool[ixnear])**2+
#             (ygrid_cool[i]-yobs_cool[ixnear])**2)>dh_max) 
#      return(NA)
    if (condition=="lt") {
      ifelse(yo_cool[ixnear[ixnearest]]< thres,return(0),return(1))
    } else if (condition=="le") {
      ifelse(yo_cool[ixnear[ixnearest]]<=thres,return(0),return(1))
    } else if (condition=="gt") {
      ifelse(yo_cool[ixnear[ixnearest]]> thres,return(0),return(1))
    } else if (condition=="ge") {
      ifelse(yo_cool[ixnear[ixnearest]]>=thres,return(0),return(1))
    }
  } else {
    return(NA)
  }
}

