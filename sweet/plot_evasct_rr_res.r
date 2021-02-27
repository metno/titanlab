#
dirout <- "/home/cristianl/data/sweet/synsct_rr/res_png"
ffin   <- file.path(dirout,"evasct_rr_res.txt")
col     <- c( "red","gold","green", "cornflowerblue", "black")
lscales <- c( 10000, 50000, 100000, 200000, 400000)
#
# read data
t <- read.table(file="/home/cristianl/data/sweet/synsct_rr/res_png/evasct_rr_res.txt.bak",header=T,sep=";",stringsAsFactors=F,strip.white=T)

pge <- t$pGE
upge <- unique( pge)
n_upge <- length( upge)
n_l <- length(lscales)

val <- t$values
th  <- t$th

#
# ets
valbest <- array( data=NA, dim=c(n_upge,n_l))
thbest  <- array( data=NA, dim=c(n_upge,n_l))

for (l in 1:n_l) {
  for (i in 1:n_upge) { 
    if ( upge[i] == 0) next
    ix <- which( pge == upge[i] & t$lscale == lscales[l] & t$score == "ets")
    j  <- which.max( val[ix]) 
    thbest[i,l]  <- th[ix][j]
    valbest[i,l] <- val[ix][j]
  }
}
#
y<- log(upge)
x<- thbest
iy<-2:length(y)
png(file="~/data/sweet/synsct_rr/res_png/synsct_rr_res_pGEvsth.png",width=800,height=800)
par(mar=c(5,5,1,1))
plot( x[iy,1], y[iy], xlim=c( min(x,na.rm=T), max(x,na.rm=T)), ylim=c( min(y[iy],na.rm=T), max(y[iy],na.rm=T)), axes=F, col="white", xlab="", ylab="" )
abline(v=seq(0,100,by=1),lty=2,lwd=1,col="lightgray")
abline(h=log(upge),lty=2,lwd=1,col="lightgray")
abline(h=log(seq(10,50,by=10)),lty=2,lwd=2,col="darkgray")
#abline(v=x,lty=2,col="gray")
for( i in 1:n_l) lines( x[iy,i], y[iy], col=col[i], lwd=(n_l+6-i))
#axis(2,at=y,labels=upge,cex.axis=2)
axis(2,at=log(c(1:5,7,10,seq(15,40,by=5),50)),labels=c(1:5,7,10,seq(15,40,by=5),50),cex.axis=2,las=2)
axis(1,cex.axis=2)
mtext("P(GE)", side=2, line=3, cex=2.5)
mtext("Best SCT threshold", side=1, line=3, cex=2.5)
box()
legend(x="topright",col=col,lwd=(n_l+6-1:length(col)),legend=c("10 km","50 km","100 km","200 km","400 km"),cex=2,bg="white")
dev.off()
q()

x<- log(upge)
y<- thbest
ix<-2:length(x)
png(file="out.png",width=800,height=800)
plot( x[ix], y[ix,1], xlim=c( min(x[ix],na.rm=T), max(x[ix],na.rm=T)), ylim=c( min(y,na.rm=T), max(y,na.rm=T)), axes=F, col="white", xlab="", ylab="" )
abline(v=x,lty=2,col="gray")
for( i in 1:n_l) lines( x[ix], y[ix,i], col=col[i], lwd=(n_l+2-i))
axis(1,at=x,labels=upge)
axis(2)
box()
dev.off()
q()
#
# pod
valbest <- array( data=NA, dim=c(n_upge,n_l))
thbest  <- array( data=NA, dim=c(n_upge,n_l))

for (l in 1:n_l) {
  for (i in 1:n_upge) { 
    if ( upge[i] == 0) next
    ix <- which( pge == upge[i] & t$lscale == lscales[l] & t$score == "pod" & val>0.9)
    if ( length(ix) == 0) next
    j  <- which.max( th[ix])
    thbest[i,l]  <- th[ix][j]
    valbest[i,l] <- val[ix][j]
  }
}

x<- log(upge)
y<- thbest
ix<-2:length(x)
plot( x[ix], y[ix,1], xlim=c( min(x[ix],na.rm=T), max(x[ix],na.rm=T)), ylim=c( min(y,na.rm=T), max(y,na.rm=T)))
for( i in 1:n_l) lines( x[ix], y[ix,i])

#
# pofd

valbest <- array( data=NA, dim=c(n_upge,n_l))
thbest  <- array( data=NA, dim=c(n_upge,n_l))

for (l in 1:n_l) {
  for (i in 1:n_upge) { 
    if ( upge[i] == 0) next
    ix <- which( pge == upge[i] & t$lscale == lscales[l] & t$score == "pofd" & val>0.01)
    if ( length(ix) == 0) next
    j  <- which.max( th[ix])
    thbest[i,l]  <- th[ix][j]
    valbest[i,l] <- val[ix][j]
  }
}

x<- upge
y<- thbest
ix<-2:length(x)
plot( x[ix], y[ix,1], xlim=c( min(x[ix],na.rm=T), max(x[ix],na.rm=T)), ylim=c( min(y,na.rm=T), max(y,na.rm=T)))
for( i in 1:n_l) lines( x[ix], y[ix,i])

q()
