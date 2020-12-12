source("functions/boxcox.r")
#
lambda <- 0.5
alpha<- 0.1
beta <- 0.2
#
x<-seq(0,300,by=0.05)
dx <- alpha*x
dx[dx<1]<-1
xup<-x+dx
xdw<-x-dx
xdw[xdw<0]<-0
ya<-( ( boxcox( xup, lambda=lambda) - boxcox(   x, lambda=lambda) + 
       boxcox(   x, lambda=lambda) - boxcox( xdw, lambda=lambda)) / 2)**2
ix <- which(x>100 & x<300)
lma <- lm( ya[ix] ~ x[ix])
#plot(x,y,type="l",lwd=4)
#lines(x,x*lm$coefficients[2]+lm$coefficients[1],lwd=3,lty=2)
#lm$coefficients
#
xt <- boxcox( x, lambda=lambda)
xtupa <- xt + sqrt(x*lma$coefficients[2]+lma$coefficients[1])
xtdwa <- xt - sqrt(x*lma$coefficients[2]+lma$coefficients[1])
#xtdw[xtdw<0]<-0
#xinv <- tboxcox( xt, lambda=lambda)
#xinvup <- tboxcox( xtup, lambda=lambda)
#xinvdw <- tboxcox( xtdw, lambda=lambda)
#
dx <- beta*x
dx[dx<1]<-1
xup<-x+dx
xdw<-x-dx
xdw[xdw<0]<-0
yb<-( ( boxcox( xup, lambda=lambda) - boxcox(   x, lambda=lambda) + 
        boxcox(   x, lambda=lambda) - boxcox( xdw, lambda=lambda)) / 2)**2
lmb <- lm( yb[ix] ~ x[ix])
xt <- boxcox( x, lambda=lambda)
xtupb <- xt + sqrt(x*lmb$coefficients[2]+lmb$coefficients[1])
xtdwb <- xt - sqrt(x*lmb$coefficients[2]+lmb$coefficients[1])
plot( x, yb, type="l",lwd=4)
lines( x, x * lmb$coefficients[2] + lmb$coefficients[1], lwd=3, lty=2)
lines( x, ya, type="l",lwd=4)
lines( x, x * lma$coefficients[2] + lma$coefficients[1], lwd=3, lty=2)
#
plot( x, xtupb, type="l",lwd=4, lty=2, xlim=c(0,20), ylim=c(-3.5,5))
lines( x, xtdwb, type="l",lwd=4, lty=2)
lines( x, xtupa, type="l",lwd=4, lty=3)
lines( x, xtdwa, type="l",lwd=4, lty=3)
lines( x, xt, type="l",lwd=4, lty=1)

#
lambda <- seq(0.3,0.7,by=0.05)
alpha<- 0.1
stdevp <- array( data=NA, dim=c( length(x), length(lambda))) 
stdevn <- array( data=NA, dim=c( length(x), length(lambda))) 
dx <- alpha*x
dx[dx<1]<-1
xup<-x+dx
xdw<-x-dx
xdw[xdw<0]<-0
for (l in 1:length(lambda)) {
  stdevp[,l] <- boxcox( xup, lambda=lambda[l]) - boxcox(   x, lambda=lambda[l])
  stdevn[,l] <- boxcox(   x, lambda=lambda[l]) - boxcox( xdw, lambda=lambda[l])
  ix <- which(x<20)
#  lma <- lm( ya[ix] ~ x[ix])
  print( paste( l, lambda[l], round(sqrt(var(stdevn[ix,l])),3), 
                              round(sqrt(var(stdevp[ix,l])),3) ))
}
