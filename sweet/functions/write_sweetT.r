#+ write results of SCT
write_sweetT <- function( conn=NA, file, res, ens, open=T, close=T, 
                          open_par) {
  if (  open | is.na(conn)) {
    conn <- file( file, "wb")
    writeBin( as.integer( open_par$n),    conn, size=4)
    writeBin( as.numeric( open_par$lon),  conn, size=4)
    writeBin( as.numeric( open_par$lat),  conn, size=4)
    writeBin( as.numeric( open_par$x),    conn, size=4)
    writeBin( as.numeric( open_par$y),    conn, size=4)
    writeBin( as.numeric( open_par$z),    conn, size=4)
    writeBin( as.numeric( open_par$prid), conn, size=4)
    writeBin( as.integer( open_par$isin), conn, size=4)
    writeBin( as.integer( open_par$nsamples), conn, size=4)
    writeBin( as.numeric( open_par$t0),       conn, size=4)
    writeBin( as.numeric( open_par$gamma),    conn, size=4)
    writeBin( as.numeric( open_par$h0),       conn, size=4)
    writeBin( as.numeric( open_par$h1i),      conn, size=4)
    writeBin( as.numeric( open_par$a),        conn, size=4)
  }
  writeBin( as.integer( ens), conn, size=4)
  writeBin( as.integer( length(res)), conn, size=4)
  writeBin( as.numeric( res), conn, size=4)
  if ( close) close( conn)
  conn
}

