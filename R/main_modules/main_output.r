if (argv$verbose | argv$debug) {
  print("write the output file")
  print(argv$output)
}
if (argv$proj4_output_files!=argv$proj4_input_obsfiles) {
    xy<-as.data.frame(x=data$lon,y=data$lat)
    coordinates(xy)<-c("x","y")
    proj4string(xy)<-CRS(argv$proj4_input_obsfiles)
    xyt<-as.data.frame(spTransform(xy,CRS=argv$proj4_output_files))  
    xout<-xyt[,1]
    yout<-xyt[,2]
    rm(xy,xyt)
} else {
  xout<-data$lon
  yout<-data$lat
}
varidx.out<-varidx
if (any(!is.na(argv$varname.opt))) 
  varidx.out<-c(varidx,varidx.opt[which(!is.na(varidx.opt))])
dataout_ncol<-length(varidx.out)+4
if (file.exists(argv$fg.file)) dataout_ncol<-dataout_ncol+1
if (file.exists(argv$fge.file)) dataout_ncol<-dataout_ncol+2
dataout<-array( data=NA, dim=c(length(yout), dataout_ncol) )
ord.varidx.out<-order(varidx.out)
str<-vector()
for (s in 1:length(ord.varidx.out)) {
  varidx.out.s<-varidx.out[ord.varidx.out[s]]
  pos.s<-which(varidx.out==varidx.out.s)
  if (pos.s>4) {
    posopt.s<-which(varidx.opt==varidx.out.s & !is.na(varidx.opt))
    posopt.nona.s<-which(varidx.opt[which(!is.na(varidx.opt))]==varidx.out.s)
    str[s]<-argv$varname.opt[posopt.s]
    dataout[,s]<-dataopt[,posopt.nona.s]
  } else if (pos.s==1) {
    str[s]<-argv$varname.x.out
    dataout[,s]<-round(xout,argv$xy.dig.out)
  } else if (pos.s==2) {
    str[s]<-argv$varname.y.out
    dataout[,s]<-round(yout,argv$xy.dig.out)
  } else if (pos.s==3) {
    str[s]<-argv$varname.elev.out
    dataout[,s]<-round(z,argv$elev.dig.out)
  } else if (pos.s==4) {
    str[s]<-argv$varname.value.out
    dataout[,s]<-round(data$value,argv$value.dig.out)
  }
}
str[s+1]<-argv$varname.prid
dataout[,(s+1)]<-data$prid
str[s+2]<-argv$varname.dqc
dataout[,(s+2)]<-dqcflag
str[s+3]<-argv$varname.sct
dataout[,(s+3)]<-round(sctpog,2)
str[s+4]<-argv$varname.rep
dataout[,(s+4)]<-round(corep,5)
s<-s+4
if (file.exists(argv$fg.file)) {
  s<-s+1
  str[s]<-argv$varname.fg.out
  dataout[,s]<-round(fg,argv$value.dig.out)
}
if (file.exists(argv$fge.file)) {
  s<-s+1
  str[s]<-argv$varname.fge_mean.out
  dataout[,s]<-round(fge.mu,argv$value.dig.out)
  s<-s+1
  str[s]<-argv$varname.fge_sd.out
  dataout[,s]<-round(fge.sd,(argv$value.dig.out+3))
}
if (argv$radarout) {
  if (length(radrr)>0) {
    datarad<-array(data=NA,dim=c(length(radrr),dataout_ncol))
    datarad[,which(str==argv$varname.x.out)]<-round(radx.from,argv$xy.dig.out)
    datarad[,which(str==argv$varname.y.out)]<-round(rady.from,argv$xy.dig.out)
    datarad[,which(str==argv$varname.value.out)]<-round(radrr,argv$value.dig.out)
    datarad[,which(str==argv$varname.prid)]<-rep(argv$radarout.prid,length(radrr))
    datarad[,which(str==argv$varname.dqc)]<-rep(0,length(radrr))
    datarad[,which(str==argv$varname.rep)]<-rep(argv$radarout.corep,length(radrr))
    dataout<-rbind(dataout,datarad)
  }
}
dataout<-as.data.frame(dataout,stringsAsFactors=F)
names(dataout)<-str
write.table(file=argv$output,
            dataout,
            quote=F,
            col.names=str,
            row.names=F,
            dec=".",
            sep=argv$separator.out)
if (argv$verbose | argv$debug) 
  print("+---------------------------------+")

