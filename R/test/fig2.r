load("tmp.rdata")
yo<-yo_sct[ix]
yb<-yo-res[[6]]
ya<-yo-res[[4]]
yav<-yo-res[[5]]
ix01<-which(res[[1]]==1)
ix11<-which(res[[1]]==11)
ix12<-which(res[[1]]==12)
ix00<-which(res[[1]]==0)
png("fig2.png",height=800,width=800)
plot(x[ix],y[ix],xlim=c(-550000,750000),ylim=c(-1000000,1000000),col="beige")
points(x[ix][ix00],y[ix][ix00],col="gray")
points(x[ix][ix11],y[ix][ix11],pch=21,bg="blue")
points(x[ix][ix12],y[ix][ix12],pch=21,bg="green")
points(x[ix][ix01],y[ix][ix01],pch=21,bg="red")

dev.off()
quit()
