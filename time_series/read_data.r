#+ 
read_data <- function( file) {
  if ( !file.exists( file)) return( NULL)
  t <- read.table( file, header=F, sep="\t", stringsAsFactors=F, strip.white=T)
  names(t) <- c( "id", "date_time", "value", "qc")
  t_date  <- strptime( t$date_time, format="%Y-%m-%d %H:%M:00.000", tz="UTC")
  t_value <- as.numeric( t$value)
  t_id    <- as.numeric( t$id)
  t_qc    <- as.numeric( t$qc)
  return( list( ids=t_id, date_times=t_date, values=t_value, qcs=t_qc))
}
#> aa<-unique(t1$qc);for (i in unique(t1$qc)) {print(paste(i,length(which(t1$qc==i))))  }
#[1] "0 174722"
#[1] "-2 20388"
#[1] "-10000 730"
#[1] "3 320"
#[1] "-10 153"
#[1] "100 7068"
#[1] "10000 10866"
#[1] "-10010 26"
#[1] "10100 31108"
#[1] "-110 1"
#[1] "-10100 60"
#[1] "-10110 2"
#[1] "10010 1"
#[1] "-10200 3"
#> ix<-which(t1$qc==0)
#> plot(t1_date[ix],t1_value[ix])
#> plot(t1_date,t1_value)
#> ix<-which(t1$qc==0)
#> plot(t1_date[ix],t1_value[ix])
#> ix<-which(t1$qc==0 | t1$qc==10100 | t1$qc==10000)
