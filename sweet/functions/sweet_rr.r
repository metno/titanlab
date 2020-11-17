#+
sweet_rr <- function( x, 
                      y, 
                      lscale, 
                      gamma_shape, 
                      gamma_rate) {
#------------------------------------------------------------------------------
  return( mvn_sim( x = x,
                   y = y,
                   mean  = 0, 
                   stdev = 1, 
                   corr_fun_type = "soar",
                   length_scale  = lscale,
                   gamma_shape   = gamma_shape,
                   gamma_rate    = gamma_rate))
}
