#+ write results of SCT
write_sctRes <- function( conn=NA, file, res, ens, open=T, close=T) {
#  - write info in this order:
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
#
#----------------------------------------------------------------------------
  if (  open | is.na(conn)) conn <- file(file, "wb")
  for (i in 1:length(res)) {
    writeBin( as.integer( ens),               conn, size=4)
    writeBin( as.integer( length( res)),      conn, size=4)
    writeBin( as.integer( i),                 conn, size=4)
    writeBin( as.integer( length( res[[i]])), conn, size=4)
    writeBin( as.numeric( res[[i]]),          conn, size=4)
  }
  if ( close) close( conn)
  conn
}

