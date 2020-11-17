# Read observational network metadata
read_obsNet <- function( file, crs_in, crs_out,
                         extent_out   = NULL, 
                         exclude_prid = NA,
                         thin_perc    = NA, 
                         thin_prid    = NA) {
  # read input file
  tab <- read.table( file, header=T, sep=";", stringsAsFactors=F, strip.white=T)
  # we expect
  # "lon"   "lat"   "z"     "prid"  "stid"  "wmoid" "qcode" "value"
  #
  # exclude some observations based on prid
  if ( !is.na( exclude_prid)) { 
    ix <- which( tab$prid != exclude_prid) 
  } else {
    ix <- 1:length(tab$lon)
  }
  # coordinate transformation
  coord <- spTransform( SpatialPoints( cbind(tab$lon[ix],tab$lat[ix]), 
                                       proj4string=CRS(crs_in)), CRS(crs_out))
  # reshape data
  prid <- tab$prid[ix]
  lat  <- tab$lat[ix]
  lon  <- tab$lon[ix]
  z    <- tab$z[ix]
  val  <- tab$value[ix]
  # initialize flag vector
  flag <- rep( T, length(ix))
  # selection based on extent
  if ( !is.null( extent_out)) {
    flag <- attr(coord,"coords")[,1] >= extent_out[1] & 
            attr(coord,"coords")[,1] <= extent_out[2] &
            attr(coord,"coords")[,2] >= extent_out[3] &
            attr(coord,"coords")[,2] <= extent_out[4]
  }
  # thinning
  if ( !is.na( thin_perc)) {
    if ( !is.na( thin_prid)) flag1 <- flag & prid == thin_prid
    ix1 <- which( flag1)
    if ( length(ix1) > 0) {
      toF <- sample.int( length( ix1), 
                         ceiling( thin_perc / 100 * length( ix1)), 
                         replace=F)
      flag[ix1[toF]] <- F
    }
  }
  # final selection
  ix2 <- which( flag)
  return( list( lon  = lon[ix2], 
                lat  = lat[ix2],
                x    = attr(coord,"coords")[ix2,1], 
                y    = attr(coord,"coords")[ix2,2],
                z    = z[ix2],
                prid = prid[ix2],
                val  = val[ix2],
                n    = length(ix2) ))
}


