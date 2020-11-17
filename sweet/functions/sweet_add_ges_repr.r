sweet_add_ges_repr <- function( val,
                                x, 
                                y, 
                                err_model = "additive", # multiplicative
                                mean = 0,
                                stdev,
                                lscale,
                                corr_fun_type = "gaussian",
                                min = NA,
                                max = NA
                                ) {
#------------------------------------------------------------------------------
  if ( err_model == "additive") {
    v <- val + mvn_sim( x = x,
                        y = y,
                        mean  = mean, 
                        stdev = stdev, 
                        corr_fun_type = corr_fun_type,
                        length_scale  = lscale)
  } else if ( err_model == "multiplicative") {
    v <- val * mvn_sim( x = x,
                        y = y,
                        mean  = mean, 
                        stdev = stdev, 
                        corr_fun_type = corr_fun_type,
                        length_scale  = lscale)
  }
  if ( !is.na( min)) v[v<min] <- min
  if ( !is.na( max)) v[v>max] <- max
  return( v)
}
