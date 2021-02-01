#+ Wavelet representation (multiresolution signal decomposition) 
wave_dec <- function( values, 
                      time_stamps=NA, 
                      flags=NULL,
                      date_format=NULL,
                      vmin=NA, 
                      vmax=NA, 
                      pads=NA,
                      preproc=T) {
#------------------------------------------------------------------------------
# Input:
#  values. vector with the time series to process 
#  time_stamps. vector with the time stamps
#  date_format. format of the time stamps
#  flags. quality flag (0=good; 1=bad)
#  vmin. range check
#  vmax. range check
#  pads. nondyadic pads with this value (default=mean of the time series)
#  preproc. logic. T = create a dyadic vector; F = assume vectors are dyadic
#
# Output:
#  time_seq. dyadic vector with the time stamps (if preproc=T)
#  val. dyadic vector with the values
#  flags. dyadic vector with the flags
#  ix. matching indexes. val[ix] <- values
#  ix_nopads. indexes to elements in dyadic vectors that are not pads value
#  mat_dwt. matrix, each column is a dyadic vector with the wavelet coefficients 
#  en_dwt.  matrix, each column is a dyadic vector with the energies 
#  idxs. dyadic vector with the indexed used to expand the output of "dwt"
#  ws. weights useful to recover the original signal
#  n_lev. integer. decomposition depth
#  scaling. scaling coefficient of the coarser level (should be a number)
#
# NOTES:
#  1. dyadic vectors have dimensions equal to a power of 2
#  2. this way we recover the original signal
#    val_rec <- rowSums( mat_dwt * ws) + scaling / sqrt(2**n_lev)
#------------------------------------------------------------------------------
  require( waveslim)
  #
  # create dyadic vectors
  if ( preproc) {
    # date time operations
    time_seq0 <- as.POSIXct( time_stamps, format=date_format)
    # guess the time step
    diff <- diff( time_stamps)
    time_step <- unique(diff)[!is.na(unique(diff))][1]
    time_step_units <- attr( diff, "units")
    # ensure a complete time sequence
    begin <- time_stamps[1]
    end   <- time_stamps[length(time_stamps)]
    time_seq <- seq( begin, end, 
                     by = paste( time_step, time_step_units), 
                     format="%Y-%m-%d %H:%M:%S", tz="UTC")
    n <- length( time_stamps)
    # ensure a complete dyadic time sequence
    n_lev <- ceiling( log( n, base=2))
    n <- 2**n_lev
    time_seq <- seq( begin, length=n, by=paste(time_step, time_step_units), 
                     format="%Y-%m-%d %H:%M:%S", tz="UTC")
    # adapt the vector of values to the time sequence
    ix <- which( time_seq %in% time_seq0)
    val <- vector( mode="numeric", length=n)
    val[] <- NA
    val[ix] <- values
  # ... no preproc needed ...
  } else {
    time_seq <- NA 
    ix       <- NA
    val <- values
    n   <- length( val)
    n_lev <- ceiling( log( n, base=2))
    n_check <- 2**n_lev
    if ( n != n_check) {
      print("The length of the time series is not a power of 2! Try again with preproc=T")
      return(NULL)
    }
  }
  #
  # range check
  if ( !is.na(vmin) & any(val<vmin)) val[val<vmin] <- NA
  if ( !is.na(vmax) & any(val<vmax)) val[val>vmax] <- NA
  #
  # Initialize flags
  # set bad values to NA
  if ( !is.null( flags)) { 
    if ( any( flags == 1)) val[flags==1] <- NA
  } else {
    flags <- rep( 0, length(val))
  }
  #
  # pads with the chosen value (mean is the default) 
  if ( is.na( pads)) pads <- mean( val, na.rm=T)
  ix_nopads <- which( !is.na( val))
  if ( any( is.na( val))) val[which(is.na(val))] <- pads
  #  
  # >-- decompose --<
  val_dwt <- dwt( val, "haar", n.levels = n_lev)
  # Initialization
  mat_dwt <- array( data=NA, dim=c( n, n_lev))
  en_dwt  <- array( data=NA, dim=c( n, n_lev))
  idxs    <- array( data=NA, dim=c( n, n_lev))
  ws      <- array( data=NA, dim=c( n, n_lev))
  # loop over the levels
  for (i in 1:n_lev) {
    # resolution associated to i-th level (e.g. i=2, then res = 2^2 = 4 points)
    res <- 2**i
    # I want the output always on vectors of the same (dyadic) lenght
    aux1 <- 1:n/res
    aux2 <- floor( aux1 - 1/res)
    idx <- 1 + aux2 
    weights <- aux1 - aux2
    ws[which(weights<=0.5),i] <- -1/sqrt(res)
    ws[which(weights> 0.5),i] <- 1/sqrt(res)
    idxs[,i]    <- idx
    # wavelet coefficients
    mat_dwt[,i] <- val_dwt[[i]][idx]
    # energies
    en_dwt[,i]  <- sqrt( (mat_dwt[,i])**2 / res)
  }
  # 
  return( list( time_seq  = time_seq,
                mat_dwt   = mat_dwt, 
                en_dwt    = en_dwt, 
                idxs      = idxs,
                ws        = ws,
                val       = val,
                flags     = flags,
                ix        = ix,
                ix_nopads = ix_nopads,
                n_lev     = n_lev,
                scaling   = val_dwt[[(n_lev+1)]] ))
}
