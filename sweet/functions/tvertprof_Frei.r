#+ vertical profile of temperature (Frei, 2014)
tvertprof_Frei<-function( z,
                          t0,
                          gamma,
                          a,
                          h0,
                          h1i) {
# ref:
# Frei, C. (2014). Interpolation of temperature in a mountainous region 
#  using nonlinear profiles and non‐Euclidean distances.
#  International Journal of Climatology, 34(5), 1585-1605.
# input
#  z  = array. elevations [m amsl]
#  t0 = numeric. temperature at z=0 [K or degC]
#  gamma = numeric. temperature lapse rate [K/m]
#  a   = numeric. Temperature contrast or inversion strength [degC] 
#  h0  = numeric. z where inversion starts [m]
#  h1i = numeric. h0+h1i is z where inversion stops [m]
#       (Frei uses h1 directly, I use an increment to h0 so to avoid ending
#        up with h1<=h0 during the optimization)
# Output
#  t   = array. temperature [K or degC]
#------------------------------------------------------------------------------
  n <- length(z)
  if ( length(t0) != n) t0 <- rep( t0[1], n)
  if ( length(gamma) != n) gamma <- rep( gamma[1], n)
  if ( length(a)  != n) a  <- rep( a[1], n)
  if ( length(h0) != n) h0 <- rep( h0[1], n)
  if ( length(h1i) != n) h1i <- rep( h1i[1], n)
  t   <- rep( NA, n)
  h1  <- h0 + h1i
  z.le.h0 <- which( z <= h0)
  z.ge.h1 <- which( z >= h1)
  z.in    <- which( z >  h0 & z < h1)
  if ( length( z.le.h0) > 0)
   t[z.le.h0] <- t0[z.le.h0] + gamma[z.le.h0] * z[z.le.h0] - a[z.le.h0] 
  if ( length( z.ge.h1) > 0)
   t[z.ge.h1] <- t0[z.ge.h1] + gamma[z.ge.h1] * z[z.ge.h1] 
  if ( length(z.in)>0)
   t[z.in] <- t0[z.in] + gamma[z.in] * z[z.in] - a[z.in]/2 * ( 1 + cos( pi * (z[z.in]-h0[z.in]) / h1i[z.in]))
  return( t)
}


