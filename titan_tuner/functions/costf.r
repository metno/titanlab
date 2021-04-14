costf <- function( par) {
  res <- sct_resistant(
              points = Points( obsToCheck_lat,
                               obsToCheck_lon,
                               obsToCheck_z),
              obsToCheck_val,
              obsToCheck_chk,
              rep( background_values, p),
              background_elab_type.sct,
              argv$num_min_outer.sct,
              argv$num_max_outer.sct,
              argv$inner_radius.sct,
              argv$outer_radius.sct,
              100,
              argv$num_min_prof.sct,
              argv$min_elev_diff.sct,
              argv$min_horizontal_scale.sct,
              argv$max_horizontal_scale.sct,
              argv$kth,
              argv$vertical_scale.sct,
              obsToCheck_mina,
              obsToCheck_maxa,
              obsToCheck_minv,
              obsToCheck_maxv,
              rep( argv$eps2.sct, p),
              rep( par, p),
              rep( par, p),
              F)
  flag <- res[[1]]
  a <- length( which( ge == 1 & flag == 1))
  c <- length( which( ge == 1 & flag == 0))
  b <- length( which( ge == 0 & flag == 1))
  d <- length( which( ge == 0 & flag == 0))
  rand <- (a+c) * (a+b) / (a+b+c+d)
  ets  <- (a-rand) / (a+b+c-rand)
  acc  <- (a+d)/(a+b+c+d)
  pod  <- a/(a+c)
  pofa <- b/(b+d)

      print( paste0("---- thr = ", par))
      print( paste0("TOT bad / a b c d= ", a+b+c+d, " ", length(which( ge==1)), " / ", a, " ", b, " ", c, " ", d))
      print( paste0("acc pod pofa ets= ", round(acc,2), " ", round(pod,2), " ", round(pofa,2), " ",round(ets,2)))


  ets
}
