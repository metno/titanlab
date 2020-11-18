#+ Multi-Variate Normal simulations
mvn_sim <- function( x,
                     y = NA,
                     mean  = NA, 
                     stdev = NA, 
                     corr_fun_type = "gaussian", #soar
                     length_scale  = 1,
                     gamma_shape   = NA, 
                     gamma_rate    = NA)
#------------------------------------------------------------------------------
# x: n-vector. easting coordinates (1 value per point)
# y: n-vector. northing coordinates (if NA, then use only x)
# mean: n-vector. MNV mean value (if NA, then mean=0)
# stdev: n-vector. standard deviation (1 value per point. If NA, then stdev=1)
# corr_fun_type: character. "gaussian". "soar" secon order auto-regressive func
# length_scale: scalar value. Covariance matrix depends on 2D distances.
#               The correlation function determines the way the covariances 
#               approach zero with the increase of distance. 
#==============================================================================
{
  n <- length( x)
  # define the covariance matrix
  covmat <- outer( x, x, FUN="-")**2
  if ( ! any( is.na( y))) {
    if ( length( y) != n) y <- rep( y[1], n)
    covmat <- covmat + outer( y, y, FUN="-")**2
  }
  if ( corr_fun_type == "gaussian") {
    covmat <- exp( -0.5 * covmat / length_scale**2)
  } else if ( corr_fun_type == "soar") {
    covmat <- sqrt( covmat) / length_scale
    covmat <- ( 1 + covmat) * exp( -covmat)
  }
  if ( ! any( is.na( stdev))) {
    if ( length( stdev) != n) stdev <- rep( stdev[1], n)
    covmat <- outer( stdev, stdev, FUN="*") * covmat
  }
  # square root of the covariance matrix
  eig    <- eigen( covmat, symmetric=T)
  if ( any( eig$values < 0)) eig$values[eig$values<0] <- 0
  covmat_sqrt <- tcrossprod( tcrossprod( eig$vectors, diag(sqrt(eig$values))), eig$vectors )
  # define the mean
  if ( any( is.na( mean))) {
    mean[] <- rep( 0, n)
  } else {
    if ( length( mean) != n) mean <- rep( mean[1], n) 
  }
  # multi-variane normal simulation
  sim_gauss <- tcrossprod( covmat_sqrt, t( rnorm( n))) + mean
  #
  if ( any( is.na(gamma_shape)) || any( is.na(gamma_rate)) ) {
    return( sim_gauss)
  } else {
    # gaussian anamorphosis (gaussian to gamma)
    return( qgamma( pnorm( sim_gauss, mean=0, sd=1), 
            shape = gamma_shape, rate = gamma_rate))
  }
}

