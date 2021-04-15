read_data <- function() {

  data_tmp <- read.table( file=ffin, header=T, sep=argv$separator, stringsAsFactors=F, strip.white=T)

  names <- names( data_tmp)
  
  if ( !( ( argv$name_lat %in% names) & ( argv$name_lon %in% names) & 
          (   argv$name_z %in% names) & ( argv$name_val %in% names))) {
    boom("check the column names in the input file")
  }

  for ( i in 1:length(names)) {
    name <- names[i]
    if ( name == argv$name_lat) {
      lat <- as.numeric( data_tmp[,i])
    } else if ( name == argv$name_lon) {
      lon <- as.numeric( data_tmp[,i])
    } else if ( name == argv$name_z) {
      z   <- as.numeric( data_tmp[,i])
    } else if ( name == argv$name_val) {
      val <- as.numeric( data_tmp[,i])
    } else if ( name == argv$name_prid) {
      prid <- as.numeric( data_tmp[,i])
    } else if ( name == argv$name_cn) {
      cn <- data_tmp[,i]
    }
  }

  if ( !( argv$name_prid %in% names)) {
    prid <- val; prid[] <- 0   
  }
  
  flag <- vector( mode="numeric", length=length(lat))

  flag <- !is.na(val) & !is.na(lat) & !is.na(lon) & !is.na(z) & !is.na(prid)
 
  if ( argv$name_cn %in% names) {
    flag <- flag & (cn %in% c("FI","NO","SE"))
  } else {
    cn <- val; cn[] <- 0   
  }

  if ( any( !is.na(argv$ext))) {
    flag <- flag & 
            lon >= argv$ext[1] & lon <= argv$ext[2] & 
            lat >= argv$ext[3] & lat <= argv$ext[4]
  }
  
  ix <- which( flag)

  return( list( lon=lon[ix], lat=lat[ix],   z=z[ix], 
                val=val[ix], prid=prid[ix], cn=cn[ix]))

}
