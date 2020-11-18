#+
sweet_t <- function( x, 
                     y, 
                     z,
                     t0_par,
                     gamma_par,
                     a_par,
                     h0_par,
                     h1i_par,
                     islocal) {
#------------------------------------------------------------------------------
  val <- t0_par + gamma_par * z
  val[islocal] <- tvertprof_Frei( z  = z[islocal],
                                  t0 = t0_par,
                                  gamma = gamma_par,
                                  a   = a_par,
                                  h0  = h0_par,
                                  h1i = h1i_par)
  val
}
