#
load("test_sct.rdata")
#
ix0<-which(res[[1]]==0)
ix1<-which(res[[1]]==1)
ix11<-which(res[[1]]==11)
ix12<-which(res[[1]]==12)
ixna<-which(res[[1]]<0)
ixw<-which(flags==1 & (values == (30 - 0.0065 * elevs)))
print(ixw)
print(length(ixw))
#
num_inner[ixna]
innov[ixna]
print(ixna)
png(file="vz.png",width=800,height=800)
plot(values,elevs)
points( values[ix0], elevs[ix0], pch=21, bg="gray")
points( values[ix11], elevs[ix11], pch=21, bg="black")
points( values[ix12], elevs[ix12], pch=21, bg="black")
points( values[ix1], elevs[ix1], pch=21, bg="red")
points( values[ixw], elevs[ixw], pch=21, bg="darkred")
points( values[ixna], elevs[ixna], pch=21, bg="blue")
dev.off()
length( ix0)  / length( res[[1]]) * 100
length( ix1)  / length( res[[1]]) * 100
length( ixna) / length( res[[1]]) * 100
png(file="map.png",width=800,height=800)
plot(lons,lats)
points(lons[ix0],lats[ix0],pch=21,bg="gray")
points(lons[ix1],lats[ix1],pch=21,bg="red")
points(lons[ix11],lats[ix11],pch=21,bg="blue")
points(lons[ix12],lats[ix12],pch=21,bg="cyan")
points(lons[ixw],lats[ixw],pch=21,bg="gold",cex=2)
dev.off()
print(ixw)
