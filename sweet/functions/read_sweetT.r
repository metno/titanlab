#+ read results of sweet_t
read_sweetT <- function( conn=NA, file, ens=NA, open=T, close=T, 
                         only_meta=F, only_next=F) {
#----------------------------------------------------------------------------
  open_par <- list()
  if (  open | is.na(conn) | only_meta) {
    conn <- file( file, "rb")
    open_par$n    <- readBin( conn, integer(), size=4)
    open_par$lon  <- readBin( conn, numeric(), size=4, n=open_par$n)
    open_par$lat  <- readBin( conn, numeric(), size=4, n=open_par$n)
    open_par$x    <- readBin( conn, numeric(), size=4, n=open_par$n)
    open_par$y    <- readBin( conn, numeric(), size=4, n=open_par$n)
    open_par$z    <- readBin( conn, numeric(), size=4, n=open_par$n)
    open_par$prid <- readBin( conn, numeric(), size=4, n=open_par$n)
    open_par$isin <- readBin( conn, integer(), size=4, n=open_par$n)
    open_par$nsamples <- readBin( conn, integer(), size=4)
    open_par$t0    <- readBin( conn, numeric(), size=4)
    open_par$gamma <- readBin( conn, numeric(), size=4)
    open_par$h0    <- readBin( conn, numeric(), size=4)
    open_par$h1i   <- readBin( conn, numeric(), size=4)
    open_par$a     <- readBin( conn, numeric(), size=4, n=open_par$nsamples)
    if (only_meta) return( list( conn=conn, meta=open_par, res_tot=NULL))
  }
  flag <- F
  if ( any( is.na( ens))) {
    flag <- T
  } else if ( length(ens) == 1) {
    aux <- ens; ens <- c( aux, aux)
  }
  res_tot <- integer(0)
  while ( 1 == 1) {
    e    <- readBin( conn, integer(), size=4)
    if ( length(e) == 0) break
    nrow <- readBin( conn, integer(), size=4)
    dat  <- readBin( conn, numeric(), size=4, n=nrow)
    if ( only_next) {
      res_tot <- dat
      break
    } 
    if ( (e >=ens[1] & e <=ens[2]) | flag) 
        res_tot <- cbind( res_tot, dat) 
  }
  if ( close) {
    close( conn)
  }
  if ( !exists("res_tot")) res_tot <- NULL
  return( list( conn=conn, meta=open_par, res_tot=res_tot))
}

