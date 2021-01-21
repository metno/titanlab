#+ read results of SCT
read_sctRes <- function( conn=NA, file, ens=NA, open=T, close=T) {
#  - read info in this order:
#   ensemble member, length of the res list, ith element of res, 
#   length of the ith element of res (is a vector), ith elem of res
#
#  - res[[i]] where the ith element of the list is:
#   1 ensemble member
#   2 dqc flag
#   3 score
#   4 rep
#   5 sod
#   6 num_inner
#   7 horizontal_scale
#   8 an_inc
#   9 an_res
#  10 cv_res
#  11 innov
#  12 idi
#  13 idiv
#  14 sig2o
#  15 true dqc flag
#  16 obs transf
#  17 obs

  if (  open | is.na(conn)) conn <- file(file, "rb")
  flag <- F
  if ( any( is.na( ens))) {
    flag <- T
  } else if ( length(ens) == 1) {
    aux <- ens; ens <- c( aux, aux)
  }
  while ( 1 == 1) {
    e    <- readBin( conn, integer(), size=4)
    if ( length(e) == 0) break
    lres <- readBin( conn, integer(), size=4)
    i    <- readBin( conn, integer(), size=4)
    nrow <- readBin( conn, integer(), size=4)
    dat  <- readBin( conn, numeric(), size=4, n=nrow)
    if ( (e >=ens[1] & e <=ens[2]) | flag) {
      if ( !exists("res",inherits=F)) {
        res <- array( data=NA, dim=c( nrow, (lres+1)))
      }
      res[,i+1] <- dat
      if ( i == lres) {
        res[,1] <- e
        if ( !exists("res_tot")) res_tot <- integer(0)
        res_tot <- rbind( res_tot, res) 
        rm( res)
      }
    }
  }
  if ( close) close( conn)
  if ( !exists("res_tot")) res_tot <- NULL
  res_tot
}
