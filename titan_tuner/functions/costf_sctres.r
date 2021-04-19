costf_sctres <- function( par) {

  par[2] <- as.integer(par[2])
  par[5] <- as.integer(par[5])
  par[4] <- max( c(par[3],par[4]))

  res <- sct_resistant(
              points = Points( obsToCheck_lat,
                               obsToCheck_lon,
                               obsToCheck_z),
              obsToCheck_val,
              obsToCheck_chk,
              rep( background_values, p),
              background_elab_type.sct,
              argv$num_min_outer.sct,
              par[5],
              par[3],
              par[4],
              100,
              argv$num_min_prof.sct,
              argv$min_elev_diff.sct,
              argv$min_horizontal_scale.sct,
              argv$max_horizontal_scale.sct,
              par[2],
              par[6],
              obsToCheck_mina,
              obsToCheck_maxa,
              obsToCheck_minv,
              obsToCheck_maxv,
              rep( argv$eps2.sct, p),
              rep( par[1], p),
              rep( par[1], p),
              F)

  flag <- res[[1]]

  a <- length( which( ge[ixe] == 1 & flag[ixe] == 1))
  c <- length( which( ge[ixe] == 1 & flag[ixe] == 0))
  b <- length( which( ge[ixe] == 0 & flag[ixe] == 1))
  d <- length( which( ge[ixe] == 0 & flag[ixe] == 0))

  rand <- (a+c) * (a+b) / (a+b+c+d)
  ets  <- (a-rand) / (a+b+c-rand)
  acc  <- (a+d)/(a+b+c+d)
  pod  <- a/(a+c)
  pofa <- b/(b+d)

  n<- length(flag)
  nodd <- length( which(flag %in% c(11,12,-999)))
  if ( nodd<(0.01*length(ixe))) {
    penalty <- 0
  } else {
    penalty <- nodd #( (n-nodd) / (0.01*n))**2
  }
  print( paste("---- par = ", round(par[1],3),
                              round(par[2]),
                              round(par[3]),
                              round(par[4]),
                              round(par[5]),
                              round(par[6])))
  print( paste0("TOT TOT_tested bad / a b c d= ", length(flag)," ", (n-nodd)," ", length(which( ge==1))," / ",
                                                  a, " ", b, " ", c, " ", d))
  print( paste0("acc pod pofa ets= ", round(acc,2), " ", round(pod,2), " ", round(pofa,2), " ",round(ets,2)))
  print( paste0("costf= ", round(ets-penalty,5)))

  ets-penalty
}
