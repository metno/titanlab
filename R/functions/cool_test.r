#+ COOL test
cool_test <- function( argv, ndata, data, dqcflag, x, y){
  nprev<-0
  if (argv$verbose | argv$debug) 
    print(paste0("cool test (",argv$cool.code,")"))
  # set doit vector
  doit<-vector(length=ndata,mode="numeric")
  doit[]<-NA
  for (f in 1:nfin) doit[data$prid==argv$prid[f]]<-argv$doit.cool[f]
  ix<-which((is.na(dqcflag) | dqcflag==argv$keep.code) & doit!=0)
  ptmp<-length(ix)
  if (ptmp<1) {
    print("cool test no valid observations left, no test")
  } else {
    rgrid_cool<-raster(ext=e,resolution=argv$grid_res.cool)
    rgrid_cool[]<-NA
    xygrid_cool<-xyFromCell(rgrid_cool,1:ncell(rgrid_cool))
    xgrid_cool<-xygrid_cool[,1]
    ygrid_cool<-xygrid_cool[,2]
    rm(xygrid_cool)
    ngrid_cool<-length(ygrid_cool)
    if (!exists("xobs_cool_aux")) xobs_cool_aux<-integer(0)
    if (!exists("yobs_cool_aux")) yobs_cool_aux<-integer(0)
    if (!exists("pridobs_cool_aux")) pridobs_cool_aux<-integer(0)
    if (!exists("yo_cool_aux")) yo_cool_aux<-integer(0)
    # test
    for (i in 1:argv$i.cool) {
      t0a<-Sys.time()
      for (j in 1:length(argv$thres.cool)) {
        # use only (probably) good observations
        ix<-which((is.na(dqcflag) | dqcflag==argv$keep.code) & doit!=0)
        if (length(ix)>0) {
          xobs_to_check_cool<-x[ix]
          yobs_to_check_cool<-y[ix]
          pridobs_to_check_cool<-data$prid[ix]
          xobs_cool<-c(x[ix],xobs_cool_aux)
          yobs_cool<-c(y[ix],yobs_cool_aux)
          yo_cool<-c(data$value[ix],yo_cool_aux)
          rgrid1<-rasterize(x=cbind(xobs_cool,yobs_cool),
                            y=rgrid_cool,
                            field=yo_cool,fun=mean,na.rm=T)
          ix1<-which(!is.na(getValues(rgrid1)))
          xy1<-xyFromCell(rgrid1,ix1)
          xobs_cool<-xy1[,1]
          yobs_cool<-xy1[,2]
          yo_cool<-getValues(rgrid1)[ix1]
          if (!is.na(argv$cores)) {
            arr<-t(mcmapply(spint_cool,
                            1:ngrid_cool,
                            mc.cores=argv$cores,
                            SIMPLIFY=T,
                            thres=argv$thres.cool[j],
                            condition=argv$condition.cool[j],
                            dh_max=argv$dh_max.cool))
          # no-multicores
          } else {
            arr<-t(mapply(spint_cool,
                          1:ngrid_cool,
                          SIMPLIFY=T,
                          thres=argv$thres.cool[j],
                          condition=argv$condition.cool[j],
                          dh_max=argv$dh_max.cool))
          }
          rgrid_cool[]<-arr
          rc<-clump(rgrid_cool)
          dc<-getValues(rc)
          freq_rc<-freq(rc)
          ytmp<-extract(rc,cbind(xobs_to_check_cool,yobs_to_check_cool),na.rm=T)
          for (k in 1:length(freq_rc[,1])) {
            if (is.na(freq_rc[k,1])) next
            flag_k<-ytmp==freq_rc[k,1] & !is.na(ytmp)
            if (length(which(flag_k))<n.cool[j,1]) {
              for (f in 1:nfin) {
                ixijkf<-which(flag_k & pridobs_to_check_cool==argv$prid[f])
                if (length(ixijkf)<n.cool[j,(f+1)]) {
                  dqcflag[ix[ixijkf]]<-argv$cool.code
                }
              } 
            }
          }
        } else {
          print("no valid observations left, no cool test" )
        }
      } # end of loop over threshold that define events
      ncur<-length(which(dqcflag==argv$cool.code & !is.na(dqcflag)))
      if (argv$verbose | argv$debug) {
        t1a<-Sys.time()
        print(paste("cool test. iteration=",i,
                    "/time",round(t1a-t0a,1),attr(t1a-t0a,"unit")))
        print(paste("# suspect observations=",ncur-nprev))
      } 
      if ((ncur-nprev)<=argv$break.cool) break
      nprev<-ncur
    } # end of loop over test iterations
    rm(doit)
    if (argv$verbose | argv$debug) 
      print("+---------------------------------+")
    if (argv$debug) 
      save.image(file.path(argv$debug.dir,"dqcres_cool.RData")) 
  } # end of "if (length(mask_cool)==0)"
  return(dqcflag)
}
