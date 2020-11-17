#+
sweet_add_ges_unif <- function( val, 
                                ge_frac, 
                                ge_min, 
                                ge_max) {
#------------------------------------------------------------------------------
  n_points <- length( val)
  n_ges    <- ceiling( ge_frac * n_points)
  val[ sample.int( n_points, size = n_ges, replace = F)] <- runif( n_ges, min = ge_min , max = ge_max)
  return(val)
}

