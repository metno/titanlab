#
dirout <- "/home/cristianl/data/sweet/synsct_tg/res_png"
ffin   <- file.path(dirout,"evasct_tg_res.txt")
col    <- c( "darkred","red","coral", "pink", "gray60", "cadetblue2", "cornflowerblue", "blue", "darkblue" )
a <- c( 1, 5, 10, 15, 21, 25, 30, 35, 41)
#
# read data
t <- read.table(file="/home/cristianl/data/sweet/synsct_tg/res_png/evasct_tg_res.txt.bak",header=T,sep=";",stringsAsFactors=F,strip.white=T)

pge <- t$pGE
upge <- unique( pge)
n_upge <- length( upge)
n_a <- length(a)

val <- t$values_in
th  <- t$th

#
# ets
valbest <- array( data=NA, dim=c(n_upge,n_a))
thbest  <- array( data=NA, dim=c(n_upge,n_a))

for (l in 1:n_a) {
  for (i in 1:n_upge) { 
    if ( upge[i] == 0) next
    print(paste(l,i))
    ix <- which( pge == upge[i] & t$a == a[l] & t$score == "ets")
    j  <- which.max( val[ix]) 
    thbest[i,l]  <- th[ix][j]
    valbest[i,l] <- val[ix][j]
  }
}
#
y<- log(upge)
x<- thbest
iy<-2:length(y)
ff<-"~/data/sweet/synsct_tg/res_png/synsct_tg_res_pGEvsth.png"
png(file=ff,width=800,height=800)
par(mar=c(5,5,1,1))
plot( x[iy,1], y[iy], xlim=c( min(x,na.rm=T), max(x,na.rm=T)), ylim=c( min(y[iy],na.rm=T), max(y[iy],na.rm=T)), axes=F, col="white", xlab="", ylab="" )
abline(v=seq(0,100,by=1),lty=2,lwd=1,col="lightgray")
abline(h=log(upge),lty=2,lwd=1,col="lightgray")
abline(h=log(seq(10,50,by=10)),lty=2,lwd=2,col="darkgray")
#abline(v=x,lty=2,col="gray")
for( i in 1:n_a) lines( x[iy,i], y[iy], col=col[i], lwd=(n_a+6-i))
#axis(2,at=y,labels=upge,cex.axis=2)
axis(2,at=log(c(1:5,7,10,seq(15,40,by=5),50)),labels=c(1:5,7,10,seq(15,40,by=5),50),cex.axis=2,las=2)
axis(1,cex.axis=2)
mtext("P(GE)", side=2, line=3, cex=2.5)
mtext("Best SCT threshold", side=1, line=3, cex=2.5)
box()
legend(x="topright",col=col,lwd=(n_a+6-1:length(col)),legend=c("-20 degC","-16","-11","-6","0","4","9","14","20"),cex=2,bg="white")
dev.off()
print( paste("file",ff))
q()
#
# pod
valbest <- array( data=NA, dim=c(n_upge,n_a))
thbest  <- array( data=NA, dim=c(n_upge,n_a))

for (l in 1:n_a) {
  for (i in 1:n_upge) { 
    if ( upge[i] == 0) next
    ix <- which( pge == upge[i] & t$a == a[l] & t$score == "pod" & val>0.8)
    if ( length(ix) == 0) next
    j  <- which.max( th[ix])
    thbest[i,l]  <- th[ix][j]
    valbest[i,l] <- val[ix][j]
  }
}

#
y<- log(upge)
x<- thbest
iy<-2:length(y)
ff<-"~/data/sweet/synsct_tg/res_png/synsct_tg_res_pod_pGEvsth.png"
png(file=ff,width=800,height=800)
par(mar=c(5,5,1,1))
plot( x[iy,1], y[iy], xlim=c( min(x,na.rm=T), max(x,na.rm=T)), ylim=c( min(y[iy],na.rm=T), max(y[iy],na.rm=T)), axes=F, col="white", xlab="", ylab="" )
abline(v=seq(0,100,by=1),lty=2,lwd=1,col="lightgray")
abline(h=log(upge),lty=2,lwd=1,col="lightgray")
abline(h=log(seq(10,50,by=10)),lty=2,lwd=2,col="darkgray")
#abline(v=x,lty=2,col="gray")
for( i in 1:n_a) lines( x[iy,i], y[iy], col=col[i], lwd=(n_a+6-i))
#axis(2,at=y,labels=upge,cex.axis=2)
axis(2,at=log(c(1:5,7,10,seq(15,40,by=5),50)),labels=c(1:5,7,10,seq(15,40,by=5),50),cex.axis=2,las=2)
axis(1,cex.axis=2)
mtext("P(GE)", side=2, line=3, cex=2.5)
mtext("Best SCT threshold", side=1, line=3, cex=2.5)
box()
legend(x="topright",col=col,lwd=(n_a+6-1:length(col)),legend=c("10 km","50 km","100 km","200 km","400 km"),cex=2,bg="white")
dev.off()
print( paste("file",ff))

#
# pofd

valbest <- array( data=NA, dim=c(n_upge,n_a))
thbest  <- array( data=NA, dim=c(n_upge,n_a))

for (l in 1:n_a) {
  for (i in 1:n_upge) { 
#    if ( upge[i] == 0) next
    ix <- which( pge == upge[i] & t$a == a[l] & t$score == "pofd" & val>0.02)
    if ( length(ix) == 0) next
    j  <- which.max( th[ix])
    thbest[i,l]  <- th[ix][j]
    valbest[i,l] <- val[ix][j]
  }
}
#
y<- upge
x<- thbest
iy<-1:length(y)
ff<-"~/data/sweet/synsct_tg/res_png/synsct_tg_res_pofd_pGEvsth.png"
png(file=ff,width=800,height=800)
par(mar=c(5,5,1,1))
plot( x[iy,1], y[iy], xlim=c( min(x,na.rm=T), max(x,na.rm=T)), ylim=c( min(y[iy],na.rm=T), max(y[iy],na.rm=T)), axes=F, col="white", xlab="", ylab="" )
abline(v=seq(0,100,by=1),lty=2,lwd=1,col="lightgray")
abline(h=(upge),lty=2,lwd=1,col="lightgray")
abline(h=(seq(10,50,by=10)),lty=2,lwd=2,col="darkgray")
#abline(v=x,lty=2,col="gray")
for( i in 1:n_a) lines( x[iy,i], y[iy], col=col[i], lwd=(n_a+6-i))
axis(2,at=y,labels=upge,cex.axis=2)
#axis(2,at=log(c(1:5,7,10,seq(15,40,by=5),50)),labels=c(1:5,7,10,seq(15,40,by=5),50),cex.axis=2,las=2)
axis(1,cex.axis=2)
mtext("P(GE)", side=2, line=3, cex=2.5)
mtext("Best SCT threshold", side=1, line=3, cex=2.5)
box()
legend(x="topright",col=col,lwd=(n_a+6-1:length(col)),legend=c("10 km","50 km","100 km","200 km","400 km"),cex=2,bg="white")
dev.off()
print( paste("file",ff))
#
q()
