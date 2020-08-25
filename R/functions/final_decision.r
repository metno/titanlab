#+ Final decision 
final_decision <- function( data, dqcflag){
  dqcflag[is.na(dqcflag)]<-0
  if (argv$verbose | argv$debug) {
    nsus<-length(which(dqcflag!=0))
    nok<-length(which(dqcflag==0))
    nsus_notna<-length(which(dqcflag!=0 & !is.na(data$value)))
    nok_notna<-length(which(dqcflag==0 & !is.na(data$value)))
    nnotna<-length(which(!is.na(data$value)))
    nna<-length(which(is.na(data$value)))
    print("summary:")
    print(" #  NAs, number of observations with no value (NAs)")
    print(" #  sus, number of suspicious observations or no-metadata")
    print(" # good, number of good observations")
    print(" NOTE for sus and good, the statistics consider only observations not NAs")
    print("summary:")
    print(paste0(" #  NAs= ",nna))
    print(paste0(" #  sus= ",nsus_notna," [",round(100*nsus_notna/nnotna,0),"%]"))
    print(paste0(" # good= ",nok," [", round(100*nok_notna/nnotna,0), "%]"))
    if (nfin>1) {
      for (f in 1:nfin) {
        nsus<-length(which(dqcflag!=0 & data$prid==argv$prid[f]))
        nok<-length(which(dqcflag==0 & data$prid==argv$prid[f]))
        nsus_notna<-length(which(dqcflag!=0 & 
                                 !is.na(data$value) & 
                                 data$prid==argv$prid[f]))
        nok_notna<-length(which(dqcflag==0 & 
                                !is.na(data$value) & 
                                data$prid==argv$prid[f]))
        nnotna<-length(which(!is.na(data$value) & 
                             data$prid==argv$prid[f]))
        nna<-length(which(is.na(data$value) & data$prid==argv$prid[f]))
        print(paste("--> summary provider",argv$prid[f]))
        print(paste0("  #  NAs= ",nna))
        print(paste0("  #  sus= ",nsus_notna," [",round(100*nsus_notna/nnotna,0),"%]"))
        print(paste0("  # good= ",nok," [", round(100*nok_notna/nnotna,0), "%]"))

      }
    }
    print("+---------------------------------+")
  }
  return(dqcflag)
}

