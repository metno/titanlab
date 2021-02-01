#+
spread_check <- function(x, i, j, len, spread_ref, perc_na_max, flag) {
  if ( (j-i+1) < len) return( flag[i:j])
#  if ( any( flag[i:j]==1)) x[i:j][which(flag[i:j]==1)] <- NA
#  perc_na <- length( which( is.na(flag[i:j]))) / (j-i+1)
#print(spread_ref)
#  if ( perc_na > perc_na_max) {
#    flag[i:j] <- 1
#    return( flag[i:j])
#  }
#print(x[11000:11100])
#  xx<-x[i:j][!is.na(x[i:j])]
#  iqr <- diff( as.numeric( quantile( x[i:j], probs=c(0.01,0.99), na.rm=T)))
  iqr <- diff( range(x[i:j]))
  if ( iqr < spread_ref) {
    flag[i:j] <- 1
  } else {
    k <- round( mean( c(i,j)))
    flag[i:k] <- spread_check(x, i, k, len, spread_ref, perc_na_max, flag)
    flag[(k+1):j] <- spread_check(x, k+1, j, len, spread_ref, perc_na_max, flag)
  }
  return( flag[i:j])

  
}
