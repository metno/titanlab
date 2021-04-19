#+ Insert Gross-Errors
insert_ge <- function( val, prid, min0, max0,
                       mina, maxa, minv, maxv, 
                       PGE, PGE_prid,
                       strategy) {
#------------------------------------------------------------------------------
# Insert a predefined fraction of GEs into a vector of values
#
# strategy = 0 / random numbers. 
#                draw random numbers from a uniform distribution 
#                within a fixed interval (random numbers)
# strategy = 1 / pseudo-random numbers
#                draw random numbers from a uniform distribution
#                within predefined intervals depending on range_a/v
# strategy = 2 / random numbers in the range_a
#                
#------------------------------------------------------------------------------

  sel <- which( prid %in% PGE_prid) 

  ix <- sample( sel, ceiling( length(sel) * PGE / 100))
  u  <- runif( length(ix), min=0, max=1)

  ge <- val 
  ge[]   <- 0
  ge[ix] <- 1

  res <- val

  if ( strategy == 0) {
    res[ix] <- min0 + u * ( max0 - min0)
  } else if ( strategy == 1) {
    a <- mina[ix]
    b <- pmin( (mina[ix]/2), (2*minv[ix]), na.rm=T)
    c <- pmax( (maxa[ix]/2), (2*maxv[ix]), na.rm=T)
    d <- maxa[ix]
    ixu1 <- which(u <= 0.5)
    ixu2 <- which(u  > 0.5)
    ix1 <- ix[ixu1]
    ix2 <- ix[ixu2]
    res[ix1] <- a[ixu1] +  u[ixu1]/0.5      * ( b[ixu1] - a[ixu1])
    res[ix2] <- c[ixu2] + (u[ixu2]-0.5)/0.5 * ( d[ixu2] - c[ixu2])
  } else if ( strategy == 2) {
    a <- mina[ix]
    b <- maxa[ix]
    res[ix] <- a + u * ( b - a)
  }

  return( list( ge=ge, val=res))

}
