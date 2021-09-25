#+
costf_buddycheck_justone <- function( par, theta) {
#------------------------------------------------------------------------------
# 1 num_min_outer, 2 num_max_outer, 3 inner_radius, 4 outer_radius, 
# 5 kth, 6 vertical_scale, 7 thr, 8 a_delta, 9 v_delta, 10 a_fact, 11 v_fact,
# 12 boxcox_par 
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  theta[theta[length(theta)]] <- par

  #----------------------------------------------------------------------------

#  obsToCheck_mina <- pmin( pmax( obsToCheck_oval - theta[8],                    argv$plau_min), 
#                           pmax( obsToCheck_oval - theta[10] * obsToCheck_oval, argv$plau_min))
#  obsToCheck_maxa <- pmax( pmin( obsToCheck_oval + theta[8],                    argv$plau_max),
#                           pmin( obsToCheck_oval + theta[10] * obsToCheck_oval, argv$plau_max))
#  obsToCheck_minv <- pmin( pmax( obsToCheck_oval - theta[9],                    argv$plau_min), 
#                           pmax( obsToCheck_oval - theta[11] * obsToCheck_oval, argv$plau_min))
#  obsToCheck_maxv <- pmax( pmin( obsToCheck_oval + theta[9],                    argv$plau_max),
#                           pmin( obsToCheck_oval + theta[11] * obsToCheck_oval, argv$plau_max))
#  if ( argv$transformation) {
#
#    obsToCheck_mina <- boxcox( x=obsToCheck_mina, lambda=theta[12])
#    obsToCheck_maxa <- boxcox( x=obsToCheck_maxa, lambda=theta[12])
#    obsToCheck_minv <- boxcox( x=obsToCheck_minv, lambda=theta[12])
#    obsToCheck_maxv <- boxcox( x=obsToCheck_maxv, lambda=theta[12])
#    obsToCheck_val  <- boxcox( x=obsToCheck_oval,  lambda=theta[12])
#
#  }

  #
  #----------------------------------------------------------------------------
  res <- buddy_check(
              points = Points( obsToCheck_lat,
                               obsToCheck_lon,
                               obsToCheck_z),
              obsToCheck_val,
              as.numeric(theta[1]), # radius 
              as.integer(theta[2]), # num_min 
              as.numeric(theta[3]), # threshold
              as.numeric(theta[4]), # max_elev_diff
              as.numeric(theta[5]), # elev_gradient
              as.numeric(theta[6]), # min_std
              100,
              rep(1,p))

  flag <- res[[1]]

  n<- length(flag)
  nodd <- length( which(flag %in% c(11,12,-999)))
#  if ( nodd<(0.01*length(ixe))) {
#    penalty <- 0
#  } else {
#    penalty <- nodd #( (n-nodd) / (0.01*n))**2
#  }
  penalty <- exp( -0.5*(nodd/(0.1*n))**2)

  flag[(flag %in% c(11,12,-999)) | is.na(flag)] <- 1

  a <- length( which( ge[ixe] == 1 & flag[ixe] == 1))
  c <- length( which( ge[ixe] == 1 & flag[ixe] == 0))
  b <- length( which( ge[ixe] == 0 & flag[ixe] == 1))
  d <- length( which( ge[ixe] == 0 & flag[ixe] == 0))

  rand <- (a+c) * (a+b) / (a+b+c+d)
  ets  <- (a-rand) / (a+b+c-rand)
  if ((a+b+c-rand)==0) ets <- 0
  acc  <- (a+d)/(a+b+c+d)
  pod  <- a/(a+c)
  pofa <- b/(b+d)

  print( paste("---- par = ", round( theta[theta[length(theta)]],5)))
  print( paste0("TOT iso bad / a b c d= ", length(flag)," ", nodd," ", length(which( ge==1))," / ",
                                                  a, " ", b, " ", c, " ", d))
  print( paste0("acc pod pofa ets= ", round(acc,2), " ", round(pod,2), " ", round(pofa,2), " ",round(ets,2)))
  print( paste0("costf= ", round(ets*penalty,5)))

#  ets*penalty
  ets
}
