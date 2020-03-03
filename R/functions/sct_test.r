#+ SCT - Spatial Consistency Test
sct_test <- function(argv,data,dqcflag,x,y,z,laf){
#==============================================================================
if (argv$verbose | argv$debug)
  print(paste0("SCT (",argv$sct.code,")"))
nprev<-0
# set doit vector
doit<-vector(length=ndata,mode="numeric"); doit[]<-NA
assign("doit",doit,env=.GlobalEnv)
eps2.sct<-vector(length=ndata,mode="numeric"); eps2.sct[]<-NA
T2vec<-vector(length=ndata,mode="numeric"); T2vec[]<-NA
T2posvec<-vector(length=ndata,mode="numeric"); T2posvec[]<-NA
T2negvec<-vector(length=ndata,mode="numeric"); T2negvec[]<-NA
for (f in 1:nfin) {
  if (!any(data$prid==argv$prid[f])) next
  aux<-which(data$prid==argv$prid[f])
  doit[aux]<-argv$doit.sct[f]
  eps2.sct[aux]<-argv$eps2.sct[f]
  T2vec[aux]<-argv$thr.sct[f]
  T2posvec[aux]<-argv$thrpos.sct[f]
  T2negvec[aux]<-argv$thrneg.sct[f]
  rm(aux)
}
# test
for (i in 1:argv$i.sct) {
  # use only (probably) good observations with doit!=0
  ix<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) & doit!=0 )  
  assign("ix",ix,env=.GlobalEnv)
  if (length(ix)>0) {
    t0a<-Sys.time()
    #--------------------------------------------------------------------------
    # SCT station-by-station
    if (argv$stn_by_stn.sct) {
      if (i>1) break # the real SCT loop follows and it needs to be done just once
      sct_loop<-T
      cont_sct_loop<-0
      # obs_to_check[i] = should we check the i-th observation?
      obs_to_check<-rep(T,length(dqcflag))
      obs_to_check[!is.na(doit) & doit!=1]<-F
      # obs_to_use[i] = should we use the i-th observation for SCT?
      #  note that obs_to_use is not updated within the sct_loop
      obs_to_use<-rep(T,length(dqcflag))
      if (argv$transf.sct) {
        yo_sct<-boxcox(x=data$value,lambda=argv$boxcox.lambda)
      } else {
        yo_sct<-data$value
      }
      fg_min<-ifelse(argv$transf.sct,
                     boxcox(x=argv$vmin,lambda=argv$boxcox.lambda),
                     argv$vmin)
      fg_max<-ifelse(argv$transf.sct,
                     boxcox(x=argv$vmax,lambda=argv$boxcox.lambda),
                     argv$vmax)
      if (argv$usefge.sct) {
        if (argv$transf.sct) {
          b_sct<-boxcox(x=fge.mu,lambda=argv$boxcox.lambda)
        } else {
          b_sct<-fge.mu
        }
        argv$fglab.sct<-NA
        obs_to_use<-!is.na(b_sct) & !is.nan(b_sct) & is.finite(b_sct)
      } else if (argv$usefg.sct) {
        if (argv$transf.sct) {
          b_sct<-boxcox(x=fg,lambda=argv$boxcox.lambda)
        } else {
          b_sct<-fg
        }
        argv$fglab.sct<-NA
        obs_to_use<-!is.na(b_sct) & !is.nan(b_sct) & is.finite(b_sct)
      }
      while (sct_loop & cont_sct_loop<(length(ix)/2)) {
        cont_sct_loop<-cont_sct_loop+1
        t00a<-Sys.time()
        # ixg= index to observations to check for the current iteration
        ixg<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) &
                    obs_to_check & obs_to_use )
        nobs_to_check<-length(ixg)
        xgrid_spint<-x[ixg]
        ygrid_spint<-y[ixg]
        zgrid_spint<-z[ixg]
        lafgrid_spint<-laf[ixg]
        yo_to_check<-yo_sct[ixg]
        # ixg= index to observations to use in SCT for the current iteration
        if (argv$usefge.sct | argv$usefg.sct) xb_spint<-b_sct[ixg]
        ixo<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) & doit!=0 &
                    obs_to_use )
        xobs_spint<-x[ixo]
        yobs_spint<-y[ixo]
        zobs_spint<-z[ixo]
        lafobs_spint<-laf[ixo]
        eps2_spint<-eps2.sct[ixo]
        nobs<-length(ixo)
        yo_spint<-yo_sct[ixo]
        if (argv$usefge.sct | argv$usefg.sct) yb_spint<-b_sct[ixo]
        # CV-analysis and variances at station points
        if (!is.na(argv$cores)) {
          arr<-t(mcmapply(oi_var_gridpoint_by_gridpoint,
                          1:nobs_to_check,
                          mc.cores=argv$cores,
                          SIMPLIFY=T,
                          dh=argv$DhorMin.sct,
                          box_o_nearest_halfwidth=argv$box_o_nearest_halfwidth.sct,
                          dz=argv$Dver.sct,
                          lafmin=argv$lafmin.sct,
                          dh_adaptive=T,
                          corr=argv$corr.sct,
                          pmax=argv$pmax.sct,
                          fg=argv$fglab.sct,
                          fg_gamma=argv$fg_gamma.sct,
                          fg_min=fg_min,
                          fg_max=fg_max,
                          succ_corr=argv$succ_corr.sct,
                          y_elab=F,
                          loocv=T,
                          o_errvar_min=argv$o_errvar_min.sct,
                          o_errvar_max=argv$o_errvar_max.sct,
                          xa_errvar_max=argv$xa_errvar_max.sct,
                          xa_errvar_min=argv$xa_errvar_min))
        # no-multicores
        } else {
          arr<-t(mapply(oi_var_gridpoint_by_gridpoint,
                        1:nobs_to_check,
                        SIMPLIFY=T,
                        dh=argv$DhorMin.sct,
                        box_o_nearest_halfwidth=argv$box_o_nearest_halfwidth.sct,
                        dz=argv$Dver.sct,
                        lafmin=argv$lafmin.sct,
                        dh_adaptive=T,
                        corr=argv$corr.sct,
                        pmax=argv$pmax.sct,
                        fg=argv$fglab.sct,
                        fg_gamma=argv$fg_gamma.sct,
                        fg_min=fg_min,
                        fg_max=fg_max,
                        succ_corr=argv$succ_corr.sct,
                        y_elab=F,
                        loocv=T,
                        o_errvar_min=argv$o_errvar_min.sct,
                        o_errvar_max=argv$o_errvar_max.sct,
                        xa_errvar_max=argv$xa_errvar_max.sct,
                        xa_errvar_min=argv$xa_errvar_min))
        }
        yav<-arr[,1]
        yav_errvar<-arr[,2]
        yo_errvar<-arr[,3]
        dh_ref<-arr[,7]
        rm(arr)
        # probability of gross error
        pog<-(yav-yo_to_check)**2/(yav_errvar+yo_errvar)
        sctpog[ixg]<-pog
        # ''presumption of innocence'' apply here
        flag_sus<-rep(F,nobs_to_check)
        # compare pog with defined thresholds
        if (any(!is.na(T2posvec[ixg])))
          flag_sus<-flag_sus |
                    (!is.na(T2posvec[ixg]) & !is.na(pog) &
                     pog>T2posvec[ixg] & (yo_to_check-yav)>0)
        if (any(!is.na(T2negvec[ixg])))
          flag_sus<-flag_sus |
                    (!is.na(T2negvec[ixg]) & !is.na(pog) &
                     pog>T2negvec[ixg] & (yo_to_check-yav)<0)
        if (any(!is.na(T2vec[ixg])))
          flag_sus<-flag_sus |
                    (!is.na(T2vec[ixg]) & !is.na(pog) &
                     pog>T2vec[ixg])
        ix_sus<-which(flag_sus)
        rm(flag_sus)
        t01a<-Sys.time()
        # case of no suspect observations
        if (length(ix_sus)==0) {
          print(paste(" stn-by-stn iteration=",cont_sct_loop,
                      "/#obs checked=",length(ixg),
                      "/no suspects",
                      "/time",round(t01a-t00a,1),attr(t01a-t00a,"unit")))
          sct_loop<-F
        # case of just one suspect observation
        } else if (length(ix_sus)==1) {
          print(paste(" stn-by-stn iteration=",cont_sct_loop,
                      "/#obs checked=",length(ixg),
                      "/#sus=1",
                      "/time",round(t01a-t00a,1),attr(t01a-t00a,"unit")))
          dqcflag[ixg[ix_sus]]<-argv$sct.code
          sct_loop<-F
        # case of more than one suspect observation
        } else {
          #+ is the suspect observation the one with the largest pog in the neighbourhood? 
          find_the_largeErr<-function(i,dist_max){
            dist<-sqrt((xgrid_largeErr[i]-xgrid_largeErr)**2+
                       (ygrid_largeErr[i]-ygrid_largeErr)**2)
            ix_near<-which(dist<dist_max)
            ifelse(any(pog_largeErr[ix_near]>pog_largeErr[i]),F,T)
          }
          #
          xgrid_largeErr<-xgrid_spint[ix_sus]
          ygrid_largeErr<-ygrid_spint[ix_sus]
          pog_largeErr<-pog[ix_sus]
          largeErr<-mapply(find_the_largeErr,
                           1:length(pog_largeErr),
                           SIMPLIFY=T,
                           dist_max=dh_ref[ix_sus])
          # largeErr[i]=F, for all i
          # largeErr is unable to find local maxima... should not happen
          if (!any(largeErr)) {
            if (argv$verbose) print("warning: SCT anomalous behaviour in largeErr")
            sct_loop<-F
          }
          # observations that are locally the more suspicious ones are flagged
          dqcflag[ixg[ix_sus[which(largeErr)]]]<-argv$sct.code
          nsct_sus<-length(which(largeErr))
          # 
          # largeErr[i]=T, for all i. 
          # All the suspicious observations have been flagged simultaneously
          if (!any(!largeErr)) sct_loop<-F
          # prepare for the next sct loop
          if (sct_loop & argv$fast.sct) {
            # optimization:
            #  observations having small pog are not be checked again
            #  observations having large pog are flagged as suspect (even if not local max)
            #  observations checked twice against the same CV-analysis are not checked again
            if (any(!is.na(T2vec[ixg]))) {
              ix_not_to_check<-which(pog<(T2vec[ixg]/4) &
                                     !is.na(T2vec[ixg]))
              if (length(ix_not_to_check)>0)
                obs_to_check[ixg[ix_not_to_check]]<-F
              ix_superBad<-which(pog>(4*T2vec[ixg]) &
                                 !is.na(T2vec[ixg]))
              if (length(ix_superBad)>0)
                dqcflag[ixg[ix_superBad]]<-argv$sct.code
              nsct_sus<-nsct_sus+length(ix_superBad)
              rm(ix_not_to_check,ix_superBad)
            }
            if (any(!is.na(T2posvec[ixg]))) {
              ix_not_to_check<-which(pog<(T2posvec[ixg]/4) &
                                     !is.na(T2posvec[ixg]) &
                                     (yo_to_check-yav)>0)
              if (length(ix_not_to_check)>0)
                obs_to_check[ixg[ix_not_to_check]]<-F
              ix_superBad<-which(pog>(4*T2posvec[ixg]) &
                                 !is.na(T2posvec[ixg]) &
                                 (yo_to_check-yav)>0)
              if (length(ix_superBad)>0)
                dqcflag[ixg[ix_superBad]]<-argv$sct.code
              nsct_sus<-nsct_sus+length(ix_superBad)
              rm(ix_not_to_check,ix_superBad)
            }
            if (any(!is.na(T2negvec[ixg]))) {
              ix_not_to_check<-which(pog<(T2negvec[ixg]/4) &
                                     !is.na(T2negvec[ixg]) &
                                     (yo_to_check-yav)<0)
              if (length(ix_not_to_check)>0)
                obs_to_check[ixg[ix_not_to_check]]<-F
              ix_superBad<-which(pog>(4*T2negvec[ixg]) &
                                 !is.na(T2negvec[ixg]) &
                                 (yo_to_check-yav)<0)
              if (length(ix_superBad)>0)
                dqcflag[ixg[ix_superBad]]<-argv$sct.code
              nsct_sus<-nsct_sus+length(ix_superBad)
              rm(ix_not_to_check,ix_superBad)
            }
            if (!exists("yav_prev")) {
              yav_prev<-dqcflag; yav_prev[]<-NA
              yav_prev[ixg]<-yav
            } else {
              if (any(yav_prev[ixg]==yav))
                obs_to_check[ixg[which(yav_prev[ixg]==yav)]]<-F
              yav_prev[ixg]<-yav
            }
          } # prepare for the next sct loop 
          print(paste(" stn-by-stn iteration=",cont_sct_loop,
                      "/#obs checked=",length(ixg),
                      "/#possibly sus=",length(ix_sus),
                      "/#sus=",nsct_sus,
                      "/time",round(t01a-t00a,1),attr(t01a-t00a,"unit")))
        } # end case of suspect observations =0 or =1 or >1
      } # end sct_loop
      if (cont_sct_loop>(length(ix)/2)) {
        print("Warning: SCT loop stopped. Too many iterations. Better check this out.")
      }
      # compute coefficients of representativeness (if needed)
      if (any(is.na(argv$const.corep))) {
        t00a<-Sys.time()
        ixg<-which(is.na(dqcflag))
        nobs_to_check<-length(ixg)
        xgrid_spint<-x[ixg]
        ygrid_spint<-y[ixg]
        zgrid_spint<-z[ixg]
        lafgrid_spint<-laf[ixg]
        yo_to_check<-yo_sct[ixg]
        if (argv$usefge.sct | argv$usefg.sct) xb_spint<-b_sct[ixg]
        ixo<-ixg
        xobs_spint<-x[ixo]
        yobs_spint<-y[ixo]
        zobs_spint<-z[ixo]
        lafobs_spint<-laf[ixo]
        eps2_spint<-eps2.sct[ixo]
        nobs<-length(ixo)
        yo_spint<-yo_sct[ixo]
        if (argv$usefge.sct | argv$usefg.sct) yb_spint<-b_sct[ixo]
          # CV-analysis and variances at station points
        if (!is.na(argv$cores)) {
          arr<-t(mcmapply(oi_var_gridpoint_by_gridpoint,
                          1:nobs_to_check,
                          mc.cores=argv$cores,
                          SIMPLIFY=T,
                          dh=argv$DhorMin.sct,
                          box_o_nearest_halfwidth=argv$box_o_nearest_halfwidth.sct,
                          dz=argv$Dver.sct,
                          lafmin=argv$lafmin.sct,
                          dh_adaptive=T,
                          corr=argv$corr.sct,
                          pmax=argv$pmax.sct,
                          fg=argv$fglab.sct,
                          fg_gamma=argv$fg_gamma.sct,
                          fg_min=fg_min,
                          fg_max=fg_max,
                          succ_corr=argv$succ_corr.sct,
                          y_elab=F,
                          loocv=T,
                          o_errvar_min=argv$o_errvar_min.sct,
                          o_errvar_max=argv$o_errvar_max.sct,
                          xa_errvar_max=argv$xa_errvar_max.sct,
                          xa_errvar_min=argv$xa_errvar_min))
        # no-multicores
        } else {
          arr<-t(mapply(oi_var_gridpoint_by_gridpoint,
                        1:nobs_to_check,
                        SIMPLIFY=T,
                        dh=argv$DhorMin.sct,
                        box_o_nearest_halfwidth=argv$box_o_nearest_halfwidth.sct,
                        dz=argv$Dver.sct,
                        lafmin=argv$lafmin.sct,
                        dh_adaptive=T,
                        corr=argv$corr.sct,
                        pmax=argv$pmax.sct,
                        fg=argv$fglab.sct,
                        fg_gamma=argv$fg_gamma.sct,
                        fg_min=fg_min,
                        fg_max=fg_max,
                        succ_corr=argv$succ_corr.sct,
                        y_elab=F,
                        loocv=T,
                        o_errvar_min=argv$o_errvar_min.sct,
                        o_errvar_max=argv$o_errvar_max.sct,
                        xa_errvar_max=argv$xa_errvar_max.sct,
                        xa_errvar_min=argv$xa_errvar_min))
        }
        yo_errvar<-arr[,3]
        rm(arr)
        corep[ixo]<-yo_errvar/mean(yo_errvar)
        t01a<-Sys.time()
        print(paste(" repres coeff=",cont_sct_loop,
                    "/#obs=",length(ixg),
                    "/time",round(t01a-t00a,1),attr(t01a-t00a,"unit")))
      } # end calculation of representativeness coefficients
      if (exists("arr")) rm(arr)
      if (exists("ixo")) rm(ixo,xobs_spint,yobs_spint,zobs_spint,lafobs_spint,
                            eps2_spint,nobs,yo_spint)
      if (exists("yb_spint")) rm(yb_spint)
      if (exists("ixg")) rm(ixg,nobs_to_check,xgrid_spint,ygrid_spint,
                           zgrid_spint,lafgrid_spint,yo_to_check)
      if (exists("xb_spint")) rm(xb_spint)
      if (exists("yo_sct")) rm(yo_sct)
      if (exists("b_sct")) rm(b_sct)
      if (exists("yav_prev")) rm(yav_prev)
      if (exists("yav")) rm(yav)
      if (exists("yav_errvar")) rm(yav_errvar)
      if (exists("yo_errvar")) rm(yo_errvar)
      if (exists("dh_ref")) rm(dh_ref)
      # 
    # END SCT stn-by-stn
    #--------------------------------------------------------------------------
    # SCT split grid into boxes
    } else {
      # set min and max for the background values
      sctvmin<-ifelse(argv$variable=="RR",-1./argv$boxcox.lambda,
                                          argv$vmin)
      assign("sctvmin",sctvmin,env=.GlobalEnv)
      sctvmax<-ifelse(argv$variable=="RR",boxcox(argv$vmax,argv$boxcox.lambda),
                                          argv$vmax)
      assign("sctvmax",sctvmax,env=.GlobalEnv)
      # create the grid for SCT. SCT is done independently in each box
      # NOTE: box size around 100Km should be ok
      if (i==1) {
        r<-raster(e,ncol=argv$grid.sct[2],nrow=argv$grid.sct[1])
        xy<-xyFromCell(r,1:ncell(r))
        xr<-xy[,1]
        yr<-xy[,2]
        ir<-1:ncell(r)
        r[]<-1:ncell(r)
      }
      # define global 1D vector used in sct (1D for fast access)
      assign("xtot",x[ix],env=.GlobalEnv)
      assign("ytot",y[ix],env=.GlobalEnv)
      assign("ztot",z[ix],env=.GlobalEnv)
      # xtot<-x[ix]
      # ytot<-y[ix]
      # ztot<-z[ix]
      if (argv$variable=="RR") {
        # ttot<-boxcox(x=data$value[ix],lambda=argv$boxcox.lambda)
        assign("ttot",boxcox(x=data$value[ix],lambda=argv$boxcox.lambda),env=.GlobalEnv)
      } else {
        # ttot<-data$value[ix]
        assign("ttot",data$value[ix],env=.GlobalEnv)
      }
      # pridtot<-data$prid[ix]
      assign("pridtot",data$prid[ix],env=.GlobalEnv)
      # laftot<-laf[ix]
      assign("laftot",laf[ix],env=.GlobalEnv)
      # assign each station to the corresponding box
      if (argv$debug) itotdeb<-extract(r,cbind(x,y))
      assign("itot",extract(r,cbind(xtot,ytot)),env=.GlobalEnv)
      # itot<-extract(r,cbind(xtot,ytot))
      # count the number of observations in each box
      rnobs<-rasterize(cbind(xtot,ytot),r,ttot,fun=function(x,...)length(x))
      nr<-getValues(rnobs)
      # create the 4D array for the function call via apply
      ixyn<-cbind(ir,xr,yr,nr)
      # SCT within each (grid) box
      # NOTE: dqcflag is updated in "sct" function
      out<-apply(ixyn,
                 FUN=sct,MARGIN=1,
                 nmin=argv$n.sct,
                 dzmin=argv$dz.sct,
                 Dhmin=argv$DhorMin.sct,
                 Dz=argv$Dver.sct,
                 eps2=argv$eps2.sct,
                 T2=argv$thr.sct,
                 T2pos=argv$thrpos.sct,
                 T2neg=argv$thrneg.sct,
                 sus.code=argv$sct.code,
                 faster=argv$fast.sct)
    } # endif SCT stn-by-stn or boxes
  } else {
    print("no valid observations left, no SCT")
  }
  ncur<-length(which(dqcflag==argv$sct.code))
  if (argv$verbose | argv$debug) {
    t1a<-Sys.time()
    str<-"("
    for (f in 1:nfin) {
      if (f>1) str<-paste0(str,"; ")
      aux<-which(dqcflag==argv$sct.code & data$prid==argv$prid[f])
      str<-paste0(str,"prid",argv$prid[f],"=",length(aux))
      rm(aux)
    }
    str<-paste0(str,")")
    print(paste("SCT, iteration=",i,
                "/time",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
    print(paste("# suspect observations=",ncur-nprev,str))
    rm(str)
  }
  if ((ncur-nprev)<=argv$break.sct) break
  nprev<-ncur
}
rm(doit)
if (argv$verbose | argv$debug)
  print("+---------------------------------+")
if (argv$debug)
  save.image(file.path(argv$debug.dir,"dqcres_sct.RData"))
#
# coefficient of observation representativeness
#-----------------------------------------------------------------------------
# corep has been set by function sct to the observation error variance
if (!any(is.na(argv$const.corep))) {
  for (f in 1:nfin) {
    if (any(data$prid[ix]==argv$prid[f])) {
      ip<-which(data$prid[ix]==argv$prid[f])
      if (length(ip)>0) corep[ix[ip]]<-argv$const.corep[f]
    } else {
      print(paste("provider ",argv$prid[f],
      ": no valid data found to compute the coefficient of representativeness",sep=""))
    }
  }
} else {
  ix<-which(!is.na(corep) & (is.na(dqcflag) | dqcflag==argv$keep.code))
  if (length(ix)>0) {
    qmn<-0.25
    qmx<-0.75
    qav<-0.5
    acorep<-abs(corep[ix])
    # ecdf(x)(x) here should give us something similar to rank(x)/length(x)
    qcorep<-ecdf(acorep)(acorep)
    if (any(qcorep<qmn)) qcorep[which(qcorep<qmn)]<-qmn
    if (any(qcorep>qmx)) qcorep[which(qcorep>qmx)]<-qmx
    for (f in 1:nfin) {
      if (any(data$prid[ix]==argv$prid[f])) {
        ip<-which(data$prid[ix]==argv$prid[f] & qcorep<=qav)
        if (length(ip)>0)
          corep[ix[ip]]<-argv$min.corep[f]+
            (argv$mean.corep[f]-argv$min.corep[f])*(qcorep[ip]-qmn)/(qav-qmn)
        ip<-which(data$prid[ix]==argv$prid[f] & qcorep>qav)
        if (length(ip)>0)
          corep[ix[ip]]<-argv$mean.corep[f]+
            (argv$max.corep[f]-argv$mean.corep[f])*(qcorep[ip]-qav)/(qmx-qav)
      } else {
        print(paste("provider ",argv$prid[f],
        ": no valid data found to compute the coefficient of representativeness",sep=""))
      }
    }
  } else {
    print("no valid first guess for the observation error variance found")
  }
}
if (argv$debug)
  save.image(file.path(argv$debug.dir,"dqcres_sct.RData"))
#
return(dqcflag)
}
