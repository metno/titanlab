#+ Quality control check based on Wavelet decomposition (or representation)
wave_rep_check <- function( values, 
                            time_stamps, 
                            flags = NULL,
                            date_format = NULL,
                            len_spread_check = NA,
                            vmin  = NA, 
                            vmax  = NA, 
                            pads  = NA,
                            debug = F) {
#------------------------------------------------------------------------------
# Input:
#  values. vector with the time series to process 
#  time_stamps. vector with the time stamps
#  date_format. format of the time stamps
#  flags. quality flag (0=good; 1=bad)
#  vmin. range check
#  vmax. range check
#  pads. nondyadic pads with this value (default=mean of the time series)
#  len_spread_check. threshold used by the spread_check
#  debug. logical. plot some graphs (require a prefix)
#
# Output:
#  flags. vector. quality control flag (0=good; 1=bad)
# NOTES:
#
#------------------------------------------------------------------------------
  require( waveslim)
  #
  # adapt vector to fit the dyadic vectors used in wave_dec
  res <- wave_dec( values, 
                   time_stamps, 
                   flags = flags,
                   date_format=date_format,
                   vmin = vmin, 
                   vmax = vmax, 
                   pads = pads,
                   preproc = T)
  if ( !is.null( flags)) {
    flags0 <- flags
    flags  <- res$flags
    flags[res$ix] <- flags0
  } else {
    flags  <- res$flags
  }
  nt <- dim( res$mat_dwt)[1]
  n <- dim( res$mat_dwt)[2]-1
  ix_pads <- which( !(1:nt %in% res$ix_nopads))
  flags[ix_pads] <- 1
  val <- res$val
  time <- res$time_seq
  ix_match <- res$ix
  # 
  # spread_check. check for regions where the spread of values is too small
  if (!is.na(len_spread_check)) {
    if (debug) {
      png( file=paste0("pngs/",prefix,"_mat_dwt1_spreadcheck.png"), height=800, width=800)
      plot( res$time_seq, res$mat_dwt[,1], pch=21, bg="red",col="red" )
      points( res$time_seq[res$ix_nopads], res$mat_dwt[res$ix_nopads,1], pch=21, bg="blue", col="blue" )
      dev.off()
    }
    # for each point, count how many consecutive wavelet coefficients are = 0
    iszero_upto <- apply( res$mat_dwt, MAR=1, FUN=function(x){length( which( cumsum(x) == 0))})
    if (debug) {
      png( file=paste0("pngs/",prefix,"_iszero_upto.png"), height=800, width=800)
      par(mar=c(5,5,1,1))
      plot( res$time_seq, iszero_upto, pch=21, bg="red",col="red", axes=F,ylim=c(0,max(iszero_upto)))
      axis(2,at=0:max(iszero_upto),labels=c(0,round(2**(1:max(iszero_upto))/24,2)))
      axis(1)
      dev.off()
    }
    # if the number of consecutive wavelet coefficients is greater then the threshold
    #  then, the variability of the signal is not enough and the points get a flag=1
    flags[which( iszero_upto > len_spread_check)] <- 1
    if (debug) {
      png( file=paste0("pngs/",prefix,"_spreadcheck.png"), height=800, width=800)
      plot( res$time_seq, res$mat_dwt[,1], pch=21, bg="red",col="red" )
      points( res$time_seq[which(flags==0)], res$mat_dwt[which(flags==0),1], pch=21, bg="blue", col="blue" )
      points( res$time_seq[which(flags==1)], res$mat_dwt[which(flags==1),1], pch=21, bg="red", col="red" )
      dev.off()
    }
  }
  #
  # loop (stop after 100 iteration)
  for (i in 1:100) {
    res_i <- wave_dec( values=val, flags=flags, preproc=F)
    # squared energy
    en2_dwt <- res_i$en_dwt**2
    # start by checking the wavelet coefficients having most of the energy 
    en2_mean <- colMeans( en2_dwt[res_i$ix_nopads,1:n])
    if (debug) {
      png( file=paste0("pngs/",prefix,"_en2_mean_i",formatC(i,width=3,flags="0"),".png"),
           height=800, width=800)
      plot( 1:n, en2_mean, axes=F)
      lines( 1:n, en2_mean)
      axis( 1, at=1:n, labels=round(2**(1:n)/24,2))
      axis( 2)
      dev.off()
    }
    loop <- F
    # loop over wavelet energies (greatest to smallest)
    for ( j in order( en2_mean, decreasing=T)) {
      # compute variability (iqr) considering only the coefficents != 0
      x <- en2_dwt[res_i$ix_nopads,j][en2_dwt[res_i$ix_nopads,j]>0]
      if ( length(x) == 0) {
        if (debug) print( paste(i,j,0,length(which(flags==1))))
        next
      }
      q75 <- as.numeric( quantile( en2_dwt[res_i$ix_nopads,j], probs=0.75))
      q25 <- as.numeric( quantile( en2_dwt[res_i$ix_nopads,j], probs=0.25))
      q50 <- median( en2_dwt[res_i$ix_nopads,j])
      iqr <- q75-q25
      if ( iqr == 0) next
      # suspect points must have energies that are outliers
      # (we used factor = 5 for outlier detection, as in Lanzante (1999))
      if ( !any( en2_dwt[res_i$ix_nopads,j] > ( q50 + 5 * (q75-q50)))) {
        if (debug) print( paste(i,j,0,length(which(flags==1))))
        next
      }
      # refine the identification of outliers by fitting a gamma distribution ...
      gamma<-gamma_get_shape_rate_from_dataset_constrOptim( x)
      prob <- pgamma( en2_dwt[res_i$ix_nopads,j],
                      shape=gamma$shape,
                      rate=gamma$rate)
      # ... then outliers are those where the energy is unlikely
      # pnorm(5) = 0.9999997 (as unlikely as a values 5-times as big as the st.dev in a standard normal)
      # if an outlier is found, then flag the bad values and re-do the check
      if ( length( ix <- which( prob > 0.9999997)) > 0) {
        loop <- T
        flags[res_i$ix_nopads[ix]] <- 1
        if (debug) print( paste(i,j,length(ix),length(which(flags==1))))
        break
      }
      if (debug) print( paste(i,j,length(ix),length(which(flags==1))))
    }
    if (debug) { 
      png( file=paste0("pngs/",prefix,"_wave_rep_check_",formatC(i,width=3,flags="0"),".png"),
           height=800, width=800)
      plot(time,val,pch=21,col="blue",bg="blue")
      ix<-which(flags==1)
      points(time[ix],val[ix],pch=21,bg="red",col="red")
      dev.off()
    }
    if (!loop) break 
  }
  #
  flags[ix_match]
}
