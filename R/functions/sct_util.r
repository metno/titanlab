#+ vertical profile of temperature (Frei, 2014)
tvertprof<-function(z,t0,gamma,a,h0,h1i) {
# ref:
# Frei, C. (2014). Interpolation of temperature in a mountainous region 
#  using nonlinear profiles and non‐Euclidean distances.
#  International Journal of Climatology, 34(5), 1585-1605.
# input
#  z= array. elevations [m amsl]
#  t0= numeric. temperature at z=0 [K or degC]
#  gamma=numeric. temperature lapse rate [K/m]
#  a= numeric. inversion (spatial) length
#  h0= numeric. z where inversion starts [m]
#  h1i= numeric. h0+h1i is z where inversion stops [m]
#       (Frei uses h1 directly, I use an increment to h0 so to avoid ending
#        up with h1<=h0 during the optimization)
# Output
#  t= array. temperature [K or degC]
#------------------------------------------------------------------------------
  t<-z
  t[]<-NA
  h1<-h0+abs(h1i)
  z.le.h0<-which(z<=h0)
  z.ge.h1<-which(z>=h1)
  z.in<-which(z>h0 & z<h1)
  if (length(z.le.h0)>0)
   t[z.le.h0]<-t0-gamma*z[z.le.h0]-a 
  if (length(z.ge.h1)>0)
   t[z.ge.h1]<-t0-gamma*z[z.ge.h1] 
  if (length(z.in)>0)
   t[z.in]<-t0-gamma*z[z.in]-a/2*(1+cos(pi*(z[z.in]-h0)/(h1-h0)))
  return(t)
}

#+ cost function used for optimization of tvertprof parameter
tvertprof2opt<-function(par) {
  te<-tvertprof(z=zopt,t0=par[1],gamma=par[2],a=par[3],h0=par[4],h1i=par[5])
  return(log((mean((te-topt)**2))**0.5))
}

#+ vertical profile of temperature (linear)
tvertprof_basic<-function(z,t0,gamma) {
# input
#  z= array. elevations [m amsl]
#  t0= numeric. temperature at z=0 [K or degC]
#  gamma=numeric. temperature lapse rate [K/m]
# Output
#  t= array. temperature [K or degC]
#------------------------------------------------------------------------------
  return(t0+gamma*z)
}

#+ cost function used for optimization of tvertprof parameter
tvertprofbasic2opt<-function(par) {
  te<-tvertprof_basic(z=zopt,t0=par[1],gamma=argv$gamma.standard)
  return(log((mean((te-topt)**2))**0.5))
}

#+ cost function used for optimization of vertprof parameter
vertprofbasic2opt<-function(par,vert_coord,gamma,obs) {
  pred<-tvertprof_basic(z=vert_coord,t0=par[1],gamma=gamma)
  return(log((mean((pred-obs)**2))**0.5))
}

#+ cost function used for optimization of vertprof parameter
vertprof2opt<-function(par,vert_coord,obs) {
  pred<-tvertprof(z=vert_coord,
                  t0=par[1],
                  gamma=par[2],
                  a=par[3],
                  h0=par[4],
                  h1i=par[5])
  return(log((mean((pred-obs)**2))**0.5))
}


#+ SCT - spatial consistency test
sct<-function(ixynp,
              nmin=50,
              dzmin=30,
              Dhmin=10000,
              Dz=200,
              Dz.bg=1500,
              eps2.bg=0.5,
              eps2=NA,
              T2=NA,
              T2pos=NA,
              T2neg=NA,
              sus.code=4,
              faster=F) {
# ref:
#  Lussana, C., Uboldi, F., & Salvati, M. R. (2010). A spatial consistency 
#   test for surface observations from mesoscale meteorological networks.
#   Quarterly Journal of the Royal Meteorological Society, 136(649), 1075-1088.
# input
#  ixynp= vector(4). 1=box identifier on the grid; 
#                    2/3=easting/northing coord (center of the box);
#                    4=number of stations within the box
#  NOTE: stations are associated to a box before running this function
#  nmin= numeric. minimum number of stations to fit a vertical profile
#  dzmin= numeric. minimum elevation range to fit a vertical profile [m]
#  Dhmin= numeric. minimum value for OI horizontal decorellation length [m]
#  Dz= numeric. OI vertical decorellation length [m]
#  Dz.bg= numeric. OI vertical decorellation length 
#         for the construction of the background (RH,RR,SD) [m]
#  eps2.bg= numeric. OI ratio between obs_err_variance/backg_err_variance
#  eps2= numeric. OI ratio between obs_err_variance/backg_err_variance
#  T2=numeric. SCT threshold. (obs-pred)^2/(varObs+varPred)^2 > T2, suspect!
#  T2pos=numeric. SCT threshold. (obs-pred)>=0 AND 
#                                (obs-pred)^2/(varObs+varPred)^2 > T2, suspect!
#  T2neg=numeric. SCT threshold. (obs-pred) <0 AND
#                                (obs-pred)^2/(varObs+varPred)^2 > T2, suspect!
#  NOTE: if T2pos and T2neg are specified then they are used, otherwise use T2
#  sus.code=numeric. identifier code for suspect observation
# output
#  number of rejected stations. (special cases: (i) NA if the function has not
#   been applied; (ii) -1 if just one station in the domain
#  
#  NOTE: "dqcflag" global variable is also updated
#------------------------------------------------------------------------------
  # check for something strange with the number of stations
  if (is.na(ixynp[4]) | is.null(ixynp[4]) | !is.finite(ixynp[4]) ) return(NA)
  # j, index for the stations in the box
  if (argv$usefge.sct) {
    j<-which(itot==ixynp[1] & !is.na(fge.mu[ix]))
  } else if (argv$usefg.sct) {
    j<-which(itot==ixynp[1] & !is.na(fg[ix]))
  } else {
    j<-which(itot==ixynp[1])
  }
  if (length(j)==0) {
    if (argv$verbose) print(paste("warning: no stations in box",ixynp[1]))
    return(-1)
  }
  # case of just one station
  if (ixynp[4]==1) {
    sctpog[ix[j]]<--1
    assign("sctpog",sctpog,envir=.GlobalEnv)
    if (argv$verbose) print(paste("warning: just one station in box",ixynp[1]))
    return(-1)
  }
  # "zopt" and "topt" are set as global variables, so that they can be used by 
  #   other functions in the optimization procedure
  assign("zopt",ztot[j],envir=.GlobalEnv)
  dz<-as.numeric(quantile(zopt,probs=0.95))-
      as.numeric(quantile(zopt,probs=0.05))
  assign("topt",ttot[j],envir=.GlobalEnv)
  # distance matrices
  disth<-(outer(xtot[j],xtot[j],FUN="-")**2.+
          outer(ytot[j],ytot[j],FUN="-")**2.)**0.5
  distz<-abs(outer(ztot[j],ztot[j],FUN="-"))
  # if the background is derived from a first-guess field
  if (argv$usefge.sct) {
    tb<-fge.mu[ix[j]]
  } else if (argv$usefg.sct) {
    tb<-fg[ix[j]]
  # no external first-guess AND variable is temperature
  } else if (argv$variable=="T") {
    # if region is too flat or not enough obs, then go for a basic profile
    if (dz<dzmin | ixynp[4]<nmin) {
      par<-c(mean(topt))
      opt<-optimize(f=tvertprofbasic2opt,interval=c(argv$vmin,argv$vmax))
      tb<-tvertprof_basic(zopt,
                          t0=opt$minimum,
                          gamma=argv$gamma.standard)
    # otherwise, fit a vertical profile of temperature
    } else {
      par<-c(mean(topt),argv$gamma.standard,5,
             as.numeric(quantile(zopt,probs=0.1)),
             as.numeric(quantile(zopt,probs=0.9)))
      opt<-optim(par,tvertprof2opt)
      tb<-tvertprof(zopt,
                    t0=opt$par[1],
                    gamma=opt$par[2],
                    a=opt$par[3],
                    h0=opt$par[4],
                    h1i=opt$par[5])
    }
  # no external first-guess AND variable is relative humidity, prec, snow-depth
  } else if (argv$variable %in% c("RH","RR","SD")) {
    tb<-vector(mode="numeric",length=ixynp[4])
    # if less than five observations in the box... not that much one can do 
    if (ixynp[4]<5) {
      tb[]<-mean(topt)
    } else {
      # set the reference length for the box-dependent large-scale 
      Dh.bg<-max((3*Dhmin),
                 mean(apply(cbind(1:nrow(disth),disth),
                     MARGIN=1,
                     FUN=function(x)
          {as.numeric(quantile(x[which(!((1:length(x))%in%c(1,(x[1]+1))))],
                      probs=0.75))})))
      # background error correlation matrix
      S.bg<-exp(-0.5*(disth/Dh.bg)**2.-0.5*(distz/Dz.bg)**2.)
      if (argv$laf.sct) {
        S.bg<-S.bg * 
              (1-(1-argv$lafmin.sct)*abs(outer(laftot[j],laftot[j],FUN="-")))
      }
      # background=OI(obs-mean(obs)) with Dh.bg,Dz.bg,eps2.bg
      tb[]<-mean(topt)+
            crossprod(S.bg,
                      crossprod(chol2inv(chol((S.bg+eps2.bg*diag(ncol(S.bg))))),
                      (topt-mean(topt))))
      rm(S.bg,Dh.bg)
    }
  }
  if (any(tb<sctvmin)) tb[which(tb<sctvmin)]<-sctvmin
  if (any(tb>sctvmax)) tb[which(tb>sctvmax)]<-sctvmax
  # OI for SCT (Lussana et al., 2010)
  # initialize variables
  # expand eps2 to eps2.vec
  eps2.vec<-vector(length=length(j))
  t2.vec<-vector(length=length(j))
  t2pos.vec<-vector(length=length(j))
  t2neg.vec<-vector(length=length(j))
  t2.vec[]<-NA
  t2pos.vec[]<-NA
  t2neg.vec[]<-NA
  for (f in 1:nfin) { 
    if (any(pridtot[j]==argv$prid[f])) {
      ipx<-which(pridtot[j]==argv$prid[f])
      eps2.vec[ipx]<-eps2[f]
      if (!any(is.na(T2))) 
        t2.vec[ipx]<-T2[f]
      if (!any(is.na(T2pos))) 
        t2pos.vec[ipx]<-T2pos[f]
      if (!any(is.na(T2neg))) 
        t2neg.vec[ipx]<-T2neg[f]
    }
  }

  # Use the smart-box approach implemented in C
  if(argv$smartbox.sct & argv$variable == "T") {
    ns = length(j)
    # TODO: Deal with sel vs sel2check in sct_wrapper
    out<-.C("sct_smart_boxes",ns=as.integer(ns),
                                       x=as.double(xtot[j]),
                                       y=as.double(ytot[j]),
                                       z=as.double(ztot[j]),
                                       t=as.double(ttot[j]),
                                       nmax=as.integer(1000),
                                       nmin=as.integer(100),
                                       nminprof=as.integer(nmin),
                                       dzmin=as.double(dzmin),
                                       dhmin=as.double(Dhmin),
                                       dz=as.double(Dz),
                                       t2pos=as.double(t2pos.vec),
                                       t2neg=as.double(t2neg.vec),
                                       eps2=as.double(eps2.vec),
                                       flags=as.integer(1:ns),
                                       sct=as.double(1:ns),
                                       rep=as.double(1:ns),
                                       boxids=as.integer(1:ns))

    keeping = which(out$flags == 0)
    remove = which(out$flags == 1)
    dqcflag[ix[j[remove]]] <- sus.code

    assign("dqcflag",dqcflag,envir=.GlobalEnv)
    corep[ix[j]] <- out$rep
    corep[ix[j[remove]]] <- NA
    assign("corep",corep,envir=.GlobalEnv)
    sctpog[ix[j]] <- out$sct
    assign("sctpog",sctpog,envir=.GlobalEnv)

    return(length(remove))
  }
  else {
    #  dqc flags
    dqctmp<-dqcflag[ix[j]]
    doittmp<-doit[ix[j]]
    #  probability of gross error (pog)
    pog<-dqctmp
    pog[]<-NA
    # set to optimal Dh for the SCT
    # either Dhmin or the average 10-percentile of the set of distances between a 
    # station and all the others
    Dh<-max(Dhmin,
            mean(apply(cbind(1:nrow(disth),disth),
                       MARGIN=1,
                       FUN=function(x){
         as.numeric(quantile(x[which(!((1:length(x))%in%c(1,(x[1]+1))))],
                            probs=0.1))})))
    # background error correlation matrix
    S<-exp(-0.5*(disth/Dh)**2.-0.5*(distz/Dz)**2.)
    if (argv$laf.sct) {
      S<-S * (1-(1-argv$lafmin.sct)*abs(outer(laftot[j],laftot[j],FUN="-")))
    }
    # S+eps2I
    diag(S)<-diag(S)+eps2.vec
    # innovation
    d<-topt-tb
    # select observations used in the test
    sel<-which(is.na(dqctmp) | dqctmp==argv$keep.code)
    # select observations to test 
    sel2check<-which(is.na(dqctmp) & doittmp==1)
    first<-T
    # loop over SCT iterations 
    # NOTE: SCT flags at most one observation, iterate until all observations pass
    while (length(sel)>1) { 
      # first iteration, inver the matrix
      if (first) {
        SRinv<-chol2inv(chol(S))
        # from S+R go back to S
        diag(S)<-diag(S)-eps2.vec
        first<-F
      } else if (length(indx)>1) {
        S<-S[-indx,-indx]
        eps2.vec<-eps2.vec[-indx]
        diag(S)<-diag(S)+eps2.vec
        SRinv<-chol2inv(chol(S))
        # from S+R go back to S
        diag(S)<-diag(S)-eps2.vec
      } else {
        # Update inverse matrix (Uboldi et al 2008, Appendix AND erratum!)
        aux<-SRinv
        SRinv<-aux[-indx,-indx]-
               (tcrossprod(aux[indx,-indx],aux[-indx,indx]))*Zinv[indx]
        S<-S[-indx,-indx]
        eps2.vec<-eps2.vec[-indx]
        rm(aux)
      }
      # next tree lines: compute cvres=(obs - CrossValidation_prediction)
      Zinv<-1/diag(SRinv)
      SRinv.d<-crossprod(SRinv,d[sel])
      ares<-crossprod(S,SRinv.d)-d[sel] #   a-Obs
      cvres<--Zinv*SRinv.d              # CVa-Obs, Lussana et al 2010 Eq(A13)
      sig2o<-mean(d[sel]*(-ares))       # Lussana et al 2010, Eq(32)
      if (sig2o<0.01) sig2o<-0.01       # better not use sig2o too small
      # pog=cvres/(sig2obs+sig2CVpred), Lussana et al 2010 Eq(20)
      pog[sel]<-(ares*cvres)/sig2o
      sctpog[ix[j[sel]]]<-pog[sel]
      assign("sctpog",sctpog,envir=.GlobalEnv)
      if (length(sel2check)==0) break
      # define the threshold for each observation
      if (any(!is.na(t2pos.vec))) {
        cvres<-(-cvres) # Obs-CVa 
        # cvres is a sel-vector, while T2vec is a sel2check-vector
        # sel includes all the sel2check values, plus possibly others
        T2vec<-vector(length=length(sel2check),mode="numeric")
        match<-match(sel2check,sel)
        ipos2check<-which(cvres[match]>=0 & is.na(dqctmp[sel2check]))
        if (length(ipos2check)>0) 
          T2vec[ipos2check]<- t2pos.vec[sel2check[ipos2check]]
        ipos2check<-which(cvres[match]<0 & is.na(dqctmp[sel2check]))
        if (length(ipos2check)>0)
          T2vec[ipos2check]<- t2neg.vec[sel2check[ipos2check]]
        rm(ipos2check)
      } else {
        T2vec<-t2.vec[sel2check]
      }
      # check if any obs fails the test
      if (any(pog[sel2check]>T2vec)) {
        # flag as suspect only the observation with the largest cvres 
        # allow for more than obs being flagged at the same time
        if (faster) {
          if (any(pog[sel2check]>(2*T2vec))) {
            indx<-which(pog[sel2check]>(2*T2vec))
          } else {
            ixprobsus<-which(pog[sel2check]>T2vec)
            indx<-ixprobsus[which.max(pog[ixprobsus])]
          }
        } else {
          ixprobsus<-which(pog[sel2check]>T2vec)
          indx<-ixprobsus[which.max(pog[ixprobsus])]
        }
        dqctmp[sel2check[indx]]<-sus.code
        # update global variable with flags
        dqcflag[ix[j[sel2check[indx]]]]<-sus.code
        assign("dqcflag",dqcflag,envir=.GlobalEnv)
        # update corep (useful from 2nd iteration onwards, if an obs fails the
        # sct, then no need for representativeness
        corep[ix[j[sel2check[indx]]]]<-NA
        assign("corep",corep,envir=.GlobalEnv)
        # update selection
        sel<-which(is.na(dqctmp) | dqctmp==argv$keep.code)
        sel2check<-which(is.na(dqctmp) & doittmp==1)
      } else {
        break
      }
    } # end cycle SCT model
    # coefficient of observation representativeness (more than 1 obs left)
    if (length(sel)>1) { 
      corep[ix[j[sel]]]<-(d[sel]*(-ares))/sig2o
      assign("corep",corep,envir=.GlobalEnv)
    }
    return(length(which(dqctmp==sus.code)))
  }
}

