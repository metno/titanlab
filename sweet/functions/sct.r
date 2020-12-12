#+ spatial consistency test 
sct <- function( i,
                 pmax,
                 r_inn = 25000,
                 r_out = 50000,
                 use0  = F,
                 justi = F,
                 plot  = F ) {
#------------------------------------------------------------------------------
# Check for the occurrence of gross measurement errors by comparing observed
# values against their neighbours.
#
# array levels:
# 0. global -> 1. outer circle -> 2. inner circle -> 3 inner circle to check
#==============================================================================

# return(c(xa,xa_errvar,o_errvar,xidi,idiv,av))
# NOTE: av is the leave-one-out CV. However, its errvar is not returned.
#       todo: figure out how to compute the leave-ione-out errvar.
# global variables: xgrid_spint, ygrid_spint, yb_spint, xb_spint
#                   VecX, VecY, dh, dh2,
  dist <- sqrt( (xgrid_spint[i]-VecX)**2 + (ygrid_spint[i]-VecY)**2)
  if ( use0) {
    i01  <- which( dist < r_out & flag == 0)
  } else {
    i01  <- which( dist < r_out & flag != 1)
  }
  if ( justi & !(i %in% i01)) i01 <- c(i,i01)
  if ( length( i01) > pmax) 
    i01 <- i01[order( dist[i01], decreasing=F)[1:pmax]] # ix from global to outer obs
  dist_out <- dist[i01]
  flag_out <- flag[i01]
  if ( length( i12 <- which( dist_out < r_inn)) < 2)  # ix from outer to inner obs
    return(list(ix=integer(0)))
  if ( justi) {
    if (length( i13 <- which( i01 == i & flag_out != 0)) < 1) { 
      return(list(ix=integer(0))) # ix from outer to inner obs to check
    }
  } else {
    if (length( i13 <- which( dist_out < r_inn & flag_out != 0)) < 2) 
      return(list(ix=integer(0))) # ix from outer to inner obs to check
  }
  z <- i12; z[] <- NA; ya <- z; yav <- z; sigma <- NA; sigma_mu <- NA
  i02 <- i01[i12] # ix from global to inner obs
  i03 <- i01[i13] # ix from global to inner obs to check
  i23 <- which( i12 %in% i13) # ix from inner obs to inner obs to check
  flag_inn <- i23; flag_inn[]<--1
  yb <- median( yo[i01])
  if ( any( yb < values_minok[i03] | yb > values_maxok[i03])) {
    # OI
    dist_inn <- dist[i02]
    dh2  <- dh**2
    inno <- yo[i01] - rep( yb, length(i01))
    S <- exp( -0.5*( outer( VecY[i01], VecY[i01], FUN="-")**2. + 
                     outer( VecX[i01], VecX[i01], FUN="-")**2) / dh2)
    SRinv <- chol2inv( chol( ( S + diag( x=eps2[i01], length(i01)))))
    SRinv_di <- crossprod( SRinv, inno) 
    G <- exp(-0.5*( outer( VecY[i02], VecY[i01], FUN="-")**2. + 
                    outer( VecX[i02], VecX[i01], FUN="-")**2) / dh2)
    ya  <- rep( yb, length(i02)) + G %*% SRinv_di
    yav <- yo[i02] - 1 / diag( SRinv[i12,i12]) * SRinv[i12,] %*% inno
    # checks 
    ya[ya<values_min]   <- values_min
    yav[yav<values_min] <- values_min
    ya[ya>values_max]   <- values_max
    yav[yav>values_max] <- values_max
    # 
    chi <- sqrt( ( yo[i02] - ya) * ( yo[i02] - yav))
#    mu <- mean(chi)
#    sigma <- as.numeric(sqrt(var(chi)))
    iz <- which( yav > values_low[i02] & yav < values_up[i02])
    if (length(iz)==0) {
      flag_inn[]<-1
      sigma <- 0; sigma_mu <- 0
      return(list(ix=i03,flag=flag_inn,z=z[i23],ya=ya[i23],yav=yav[i23],sigma=sigma,sigma_mu=sigma_mu))
    }
    mu <- median(chi[iz])
    sigma <- as.numeric(diff(quantile(chi[iz],probs=c(0.25,0.75))))
    sigma_mu <- sigma / sqrt(length(iz)) 
    z <- (chi-mu)/(sigma+sigma_mu)
    ix_bad <- i23[which.max(z[i23])]
    if ( z[ix_bad] > thr & 
         ( yav[ix_bad] < values_minok[i02[ix_bad]] | 
           yav[ix_bad] > values_maxok[i02[ix_bad]])) {
      flag_inn[which.max(z[i23])] <- 1
#      print("--------------------")
#      print(paste("chi mean stdev stdev_mu",round(mu,3),round(sigma,3),round(sigma_mu,3)))
#      print("ix or yo yb ya yav chi z flag")
#      print(cbind(i03, values_or[i03],round(yo[i03],2),round(rep(yb,length(i03)),2),round(ya[i23],2),round(yav[i23],2),
#                  round(chi[i23],2), round(z[i23],2),flag_inn))
    } else if (!any(z>thr)) {
      flag_inn[] <- 0
    }
     
#    if (i%%100==0) print(i)
    if ( plot & any(flag_inn==1)) {
      print("--------------------")
      print(paste("chi mean stdev stdev_mu",round(mu,3),round(sigma,3),round(sigma_mu,3)))
      print("or yo yb ya yav chi z")
      print(cbind(values_or[i03],round(yo[i03],2),round(rep(yb,length(i03)),2),round(ya[i23],2),round(yav[i23],2),
                  round(chi[i23],2), round(z[i23],2)))
      xymx<-max(c(abs(yo[i02]-ya),abs(yo[i02]-yav)))
      xall<-seq(-100,100,by=0.001)
      ij <- paste0(formatC(j,flag="0",width=4),"_",formatC(i,flag="0",width=4))
      fffig<-paste0("png/fig_",ij,".png")
      png(file=fffig,width=800,height=800)
      par(mar=c(5,5,1,1))
      plot(yo[i03]-ya[i23],yo[i03]-yav[i23],xlim=c(-xymx,xymx),ylim=c(-xymx,xymx),col="white",axes=F,xlab="",ylab="")
      for (ii in c(1:25)) 
        lines( xall, ((mu + ii * (sigma+sigma_mu))**2)/xall,lty=3, col="gray")  
      for (ii in c(seq(5,50,by=5))) 
        lines( xall, ((mu + ii * (sigma+sigma_mu))**2)/xall,lty=3, col="gray10")  
      lines(xall,xall,lty=2)
      lines(xall,xall*(1+eps2[1])/eps2[1],lty=2)
      abline(h=0,v=0)
      points( yo[i02]-ya, yo[i02]-yav, pch=21, bg="gray", col="darkgray")
      points( yo[i03]-ya[i23], yo[i03]-yav[i23], col="darkblue", lwd=3)
      if ( length( h<-which( flag_inn == 1))>0) 
        points( yo[i02[i23[h]]]-ya[i23[h]], yo[i02[i23[h]]]-yav[i23[h]], pch=21, col="darkred", bg="red", lwd=2, cex=2)
      axis(1)
      axis(2)
      mtext(1,line=3,text="Analysis residuals",cex=2)
      mtext(2,line=3,text="CV-analysis residuals",cex=2)
      box()
      dev.off()
      #
      ffmap<-paste0("png/map_",ij,".png")
      png(file=ffmap,width=800,height=800)
      par(mar=c(1,1,1,1))
      dat<-t(data[,,1])
      r[]<-dat
      r<-disaggregate(r,fact=5,method="bilinear")
      r[r<0]<-0
      r <- crop( r, extent( range(obsnet$x[i01])[1]-10000, range(obsnet$x[i01])[2]+10000,
                            range(obsnet$y[i01])[1]-10000, range(obsnet$y[i01])[2]+10000))
      image(r, breaks=c(0,0.1,2,4,8,16,32,64,128,256,512), col=c("gray",rev(rainbow(9))), axes=F,xlab="",ylab="")
      t <- seq(0,r_inn*pi,length=1000); coords <- t(rbind( obsnet$x[i]+sin(t)*r_inn, obsnet$y[i]+cos(t)*r_inn))
      points(coords,cex=0.5,pch=21,bg="darkgray",col="gray10") 
      t <- seq(0,r_out*pi,length=1000); coords <- t(rbind( obsnet$x[i]+sin(t)*r_out, obsnet$y[i]+cos(t)*r_out))
      points(coords,cex=0.5,pch=21,bg="darkgray",col="gray10") 
      # global 
      ix1 <- which( flag == 1)
      points( obsnet$x[ix1], obsnet$y[ix1], cex=2, pch=4)
      ixm1 <- which( flag == -1)
      points( obsnet$x[ixm1], obsnet$y[ixm1], cex=2, pch=1)
      ix0 <- which( flag == 0)
      points( obsnet$x[ix0], obsnet$y[ix0], cex=2, pch=2)
      # inner/outer
      ixm1 <- which( flag[i01] == -1)
      points( obsnet$x[i01[ixm1]], obsnet$y[i01[ixm1]], cex=5,pch=21, bg="white", col="gray")
      text(obsnet$x[i01[ixm1]], obsnet$y[i01[ixm1]], values_or[i01[ixm1]])
      ix0 <- which( flag[i01] == 0)
      points( obsnet$x[i01[ix0]], obsnet$y[i01[ix0]], cex=5,pch=24, bg="white", col="gray")
      text(obsnet$x[i01[ix0]], obsnet$y[i01[ix0]], values_or[i01[ix0]])
      # inner
      points( obsnet$x[i02[i23]],obsnet$y[i02[i23]], cex=5, pch=21,  bg="cyan", col="blue")
      text(obsnet$x[i02[i23]], obsnet$y[i02[i23]], values_or[i02[i23]])
      if ( length( h<-which( flag_inn == 1))>0) { 
        points( obsnet$x[i02[i23[h]]],obsnet$y[i02[i23[h]]], cex=5, pch=21, bg="pink", col="red")
        text(obsnet$x[i02[i23[h]]], obsnet$y[i02[i23[h]]], values_or[i02[i23[h]]])
      }
#      points( obsnet$x[i], obsnet$y[i], cex=4,pch=21, bg="pink", col="red")
      text(obsnet$x[i], obsnet$y[i], values_or[i], col="maroon")
      box()
      dev.off()
      fffin<-paste0("png/mapfig_",ij,".png")
      system(paste("convert +append",ffmap,fffig,fffin))
      system(paste("rm ",ffmap,fffig))
    }
  } else {
    flag_inn[]<-0
  }
  return(list(ix=i03,flag=flag_inn,z=z[i23],ya=ya[i23],yav=yav[i23],sigma=sigma,sigma_mu=sigma_mu))
}

