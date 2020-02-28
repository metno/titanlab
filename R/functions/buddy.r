#+ buddy check (standard)
buddy <- function(argv,data,dqcflag){
#==============================================================================
print(paste0("buddy-check (",argv$buddy.code,")"))
print(paste0("priorities ",toString(argv$prio.buddy)))
nsus<-vector(mode="numeric",length=length(argv$dr.buddy))
# set doit/prio vectors
doit<-vector(length=ndata,mode="numeric"); doit[]<-NA
prio<-vector(length=ndata,mode="numeric"); prio[]<-NA
for (f in 1:nfin) {
  aux<-which(data$prid==argv$prid[f])
  if (length(aux)==0) next
  doit[aux]<-argv$doit.buddy[f]
  prio[aux]<-argv$prio.buddy[f]
}
rm(aux)
# if needed, prepare fg for buddy check
# if argv$usefg.buddy are not all NAs and at least one of them == 1
fg_x<-integer(0)
fg_y<-integer(0)
fg_z<-integer(0)
fg_val<-integer(0)
fg_prio<-integer(0)
nfg_val<-0
if (any(!is.na(argv$usefg.buddy)) & any(argv$usefg.buddy==1)) {
  t0a<-Sys.time()
  dfg<-getValues(rfg)
  if (exists("rfgdem")) { dfgdem<-getValues(rfgdem) } else
                        { dfgdem<-rep(0,length(dfg)) }
  # get coordinates into CRS 
  ixx<-which(!is.na(dfg) & !is.na(dfgdem))
  if (length(ixx)>0) {
    fgxy<-as.data.frame(xyFromCell(rfg,ixx))
    names(fgxy)<-c("x","y")
    coordinates(fgxy)<-c("x","y")
    proj4string(fgxy)<-CRS(argv$proj4fg)
    fgxy_transf<-as.data.frame(spTransform(fgxy,CRS=argv$proj4_where_dqc_is_done))
    fg_x<-fgxy_transf[,1]
    fg_y<-fgxy_transf[,2]
    fg_z<-dfgdem[ixx]
    fg_val<-dfg[ixx]
    fg_prio<-rep(-1,length(ixx))
    rm(fgxy,fgxy_transf)
  }
  # thinning
  ix<-which( (is.na(dqcflag) | dqcflag==argv$keep.code) &
             !is.na(x) & !is.na(y) & !is.na(z) & !is.na(prio) &
             !is.na(data$value) & doit!=0 )
  thin_fg_for_buddy<-function(i, dr) {
    if (any(abs(obsToCheck_x-fg_x[i])<dr & abs(obsToCheck_y-fg_y[i])<dr)) return(1)
    return(0)
  }
  obsToCheck_x<-x[ix]
  obsToCheck_y<-y[ix]
  dr_max<-max(argv$dr.buddy[argv$usefg.buddy==1 & !is.na(argv$usefg.buddy)])
  if (!is.na(argv$cores)) {
    thin_fg<-mcmapply(thin_fg_for_buddy,
                      1:length(fg_x),
                      mc.cores=argv$cores,
                      SIMPLIFY=T,
                      dr=dr_max)
  # no-multicores
  } else {
    thin_fg<-mapply(thin_fg_for_buddy,
                    1:length(fg_x),
                    SIMPLIFY=T,
                    dr=dr_max)
  }
  ixx<-which(thin_fg>0)
  if (length(ixx)>0) {
    fg_x<-fg_x[ixx]
    fg_y<-fg_y[ixx]
    fg_z<-fg_z[ixx]
    fg_val<-fg_val[ixx]
    fg_prio<-fg_prio[ixx]
  }
  rm(ix,obsToCheck_x,obsToCheck_y,thin_fg)
  rm(dfg,ixx,dr_max)
  nfg_val<-length(fg_val)
  if (argv$verbose | argv$debug) {
    t1a<-Sys.time()
    print(paste0("use first-guess values, number of values is =",nfg_val,
                 "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
  }
} # end prepare fg for the test
# test
for (i in 1:argv$i.buddy) {
  priority<-ifelse((i==1 & any(prio!=(-1))),T,F)
  nsus[]<-0
  for (j in 1:length(argv$dr.buddy)) {
    # use only (probably) good observations with doit!=0
    flag_aux<-( (is.na(dqcflag) | dqcflag==argv$keep.code) &
                !is.na(x) & !is.na(y) & !is.na(data$value) &
                doit!=0)
    if (priority) flag_aux<-flag_aux & !is.na(prio)
    if (argv$variable=="T") flag_aux<-flag_aux & !is.na(z)
    ix<-which(flag_aux); rm(flag_aux)
    t0a<-Sys.time()
    obsToCheck_n<-length(ix)
    if (obsToCheck_n>0) {
      # define global 1D vector used in statSpat (1D for fast access)
      assign("obsToCheck_i",1:obsToCheck_n,env=.GlobalEnv)
      assign("obsToCheck_x",x[ix],env=.GlobalEnv)
      assign("obsToCheck_y",y[ix],env=.GlobalEnv)
      assign("obsToCheck_z",as.numeric(z[ix]),env=.GlobalEnv)
      assign("obsToCheck_prio",as.numeric(prio[ix]),env=.GlobalEnv)
      assign("obsToCheck_val",data$value[ix],env=.GlobalEnv)
      # obsToCheck_i<-1:obsToCheck_n
      # obsToCheck_x<-x[ix]
      # obsToCheck_y<-y[ix]
      # obsToCheck_z<-as.numeric(z[ix])
      # obsToCheck_prio<-as.numeric(prio[ix])
      # obsToCheck_val<-data$value[ix]
      if (argv$usefg.buddy[j]==1 & !is.na(argv$usefg.buddy[j])) {
        assign("dataToUse_i",1:obsToCheck_n+nfg_val,env=.GlobalEnv)
        assign("dataToUse_x",c(obsToCheck_x,fg_x),env=.GlobalEnv)
        assign("dataToUse_y",c(obsToCheck_y,fg_y),env=.GlobalEnv)
        assign("dataToUse_z",as.numeric(c(obsToCheck_z,fg_z)),env=.GlobalEnv)
        assign("dataToUse_prio",as.numeric(c(prio[ix],fg_prio)),env=.GlobalEnv)
        assign("dataToUse_val",c(obsToCheck_val,fg_val),env=.GlobalEnv)
      # dataToUse_i<-1:(obsToCheck_n+nfg_val)
      # dataToUse_x<-c(obsToCheck_x,fg_x)
      # dataToUse_y<-c(obsToCheck_y,fg_y)
      # dataToUse_z<-as.numeric(c(obsToCheck_z,fg_z))
      # dataToUse_prio<-as.numeric(c(prio[ix],fg_prio))
      # dataToUse_val<-c(obsToCheck_val,fg_val)
      } else {
        assign("dataToUse_i",1:obsToCheck_n,env=.GlobalEnv)
        assign("dataToUse_x",obsToCheck_x,env=.GlobalEnv)
        assign("dataToUse_y",obsToCheck_y,env=.GlobalEnv)
        assign("dataToUse_z",as.numeric(obsToCheck_z),env=.GlobalEnv)
        assign("dataToUse_prio",as.numeric(prio[ix]),env=.GlobalEnv)
        assign("dataToUse_val",obsToCheck_val,env=.GlobalEnv)
      # dataToUse_i<-1:obsToCheck_n
      # dataToUse_x<-obsToCheck_x
      # dataToUse_y<-obsToCheck_y
      # dataToUse_z<-as.numeric(obsToCheck_z)
      # dataToUse_prio<-as.numeric(prio[ix])
      # dataToUse_val<-obsToCheck_val
      }
      if (argv$transf.buddy) {
        dataToUse_val<-boxcox(x=dataToUse_val,lambda=argv$boxcox.lambda)
        obsToCheck_val<-boxcox(x=obsToCheck_val,lambda=argv$boxcox.lambda)
      }
      if (!is.na(argv$cores)) {
        stSp_buddy<-mcmapply(statSpat_mapply,
                             1:obsToCheck_n,
                             mc.cores=argv$cores,
                             SIMPLIFY=T,
                             adjust_for_elev_diff=(argv$variable=="T"),
                             dr=argv$dr.buddy[j],
                             priority=priority,
                             pmax=1000)
      # no-multicores
      } else {
        stSp_buddy<-mapply(statSpat_mapply,
                           1:obsToCheck_n,
                           SIMPLIFY=T,
                           adjust_for_elev_diff=(argv$variable=="T"),
                           dr=argv$dr.buddy[j],
                           priority=priority,
                           pmax=1000)
      }
      # probability of gross error
      stSp_buddy[4,]<-pmax(argv$sdmin.buddy[j],stSp_buddy[4,])
      stSp_buddy[4,which(stSp_buddy[1,]==1)]<-argv$sdmin.buddy[j]
      pog<-abs(obsToCheck_val-stSp_buddy[3,])/stSp_buddy[4,]
      n.buddy<-ifelse(priority,0,argv$n.buddy[j])
      # suspect if: 
      sus<-which( pog>argv$thr.buddy[j] &
                  stSp_buddy[1,]>n.buddy &
                  stSp_buddy[2,]<argv$dz.buddy[j] &
                  is.na(dqcflag[ix]) &
                  doit[ix]==1 )
      # set dqcflag
      if (length(sus)>0) dqcflag[ix[sus]]<-argv$buddy.code
    } else {
      print("no valid observations left, no buddy check")
    }
    nsus[j]<-ifelse(exists("sus"),length(sus),0)
    if (argv$verbose | argv$debug) {
      t1a<-Sys.time()
      str<-" (#TOT "
      for (f in 1:nfin) {
        if (f>1) str<-paste0(str,"; ")
        aux<-which(dqcflag==argv$buddy.code & data$prid==argv$prid[f])
        str<-paste0(str,"prid",argv$prid[f],"=",length(aux))
        rm(aux)
      }
      str<-paste0(str,")")
      if (i==1) {
        print(paste0("iteration=",i,
                     "/test=",j,
                     "/dqc param: thres=",argv$thr.buddy[j],
                     "rad=",argv$dr.buddy[j],
                     "usefg=",argv$usefg.buddy[j],
                     "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
      } else {
        print(paste0("iteration=",i,
                     "/test=",j,
                     "/time ",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
      }
      print(paste0(nsus[j]," new suspect observations",str))
      rm(str)
    }
  } # end for j
  if (!priority & sum(nsus)<=argv$break.buddy) break
}  # end for i
rm(doit,prio)
if (argv$debug) save.image(file.path(argv$debug.dir,"dqcres_buddy.RData"))
if (argv$verbose | argv$debug)
  print("+---------------------------------+")
if (exists("stSp_buddy")) rm(stSp_buddy)
if (exists("sus")) rm(sus)
if (exists("pog")) rm(pog)
if (exists("xtot")) rm(xtot)
if (exists("ytot")) rm(ytot)
if (exists("ztot")) rm(ztot)
if (exists("itot")) rm(itot)
rm(list = ls()[grep("dataToUse", ls())])
rm(list = ls()[grep("obsToCheck", ls())])
rm(list = ls()[grep("stSp_buddy", ls())])
rm(list = ls()[grep("aux", ls())])
#
return(dqcflag)
}
