#+ create time sequences
ttun_timeseq<-function( argv) {
#------------------------------------------------------------------------------
  # input - sequence of time steps 
  # (a) get tseq from the date file
  if (!is.na(argv$ffin_date.file)) {
    if (!file.exists(argv$ffin_date.file))
       boom(paste0("file not found",argv$ffin_date.file))
    tin<-read.table(file=argv$ffin_date.file,header=F,stringsAsFactors=F,strip.white=T)
    tseq<-as.POSIXlt(str2Rdate(tin$V1,argv$ffin_date.format),tz="UTC")
  # (b) get tseq from other ways
  } else {
    # (b1) get tseq from the template file
    if (argv$date1=="none") {
      if ( !file.exists(argv$ffin_template))
         boom( paste0( "file not found", argv$ffin_template))
      tseq<-as.POSIXlt(str2Rdate(nc4.getTime(argv$ffin_template),
                       format="%Y%m%d%H%M"),tz="UTC")
    # (b2) compute tseq
    } else {
      # (b2a) get tseq from date1 -> date2 
      date2 <- argv$date2
      if (argv$date2=="none") {
        if ( is.na(argv$time_n_prev) & 
             is.na(argv$time_n_succ) ) boom(paste0("error in date definition"))
        if (!is.na(argv$time_n_prev)) {
          if (argv$time_unit %in% c("sec","secs","second","seconds")) {
            aux<-rev(seq(strptime(argv$date1,format=argv$date.format),
                         length=argv$time_n_prev,
                         by=(-argv$time_step)))
          } else {
            aux<-rev(seq(strptime(argv$date1,format=argv$date.format),
                         length=argv$time_n_prev,
                         by=paste((-argv$time_step),argv$time_unit)))
          }
          date2<-argv$date1
          date1<-format(aux[1],format=argv$date.format)
          rm(aux)
        }
        if (!is.na(argv$time_n_succ)) {
          aux<-rev(seq(strptime(argv$date1,format=argv$date.format),
                                length=argv$time_n_succ,
                                by=paste(argv$time_step,argv$time_unit)))
          date2<-format(aux[1],format=argv$date.format)
          rm(aux)
        }
      } else {
        date1<-argv$date1
      }
      tseq <- createTimeSeq( start_date     = date1,
                             stop_date      = date2,
                             format         = argv$date.format,
                             time_step      = argv$time_step,
                             unit           = argv$time_unit,
                             season         = NULL,
                             hourOFday.sel  = NULL,
                             dayOFmonth.sel = NULL,
                             N.prev         = NULL,
                             N.succ         = NULL,
                             RdateOnlyOut   = T,
                             verbose        = F)
    } # end if (argv$date1=="none")
  } # end if (!is.na(ffin_date.file))
  # consider only some months
  if (any(!is.na(argv$date_filter_by_month))) {
    if (length(ix<-which( as.integer(format(tseq,format="%m",tz="GMT")) %in% 
                          argv$date_filter_by_month ))>0) {
      tseq<-tseq[ix]
    } else {
      boom("date_filter_by_month is outside the time period chosen")
    }
  }
  n_tseq<-length(tseq)
  if (argv$debug) {
    print("input - time sequence")
    print(tseq)
    print(paste("number of time steps =",n_tseq))
  }
  #----------------------------------------------------------------------------
  if ( !exists( "tseq_out")) tseq_out<-NA
  return( list( n_tseq= n_tseq,
                tseq = tseq))
}
