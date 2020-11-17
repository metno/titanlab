library(raster)
library(rgdal)
#
year <- 2019
path <- file.path( "/home/cristianl/data/titan_testbed/observations_testbed", year)
extent=c(-320000, -180000, -160000, -30000)
extent_small=c(-275000, -235000, -110000, -75000)
proj4.wgs84     <- "+proj=longlat +datum=WGS84"
proj4.ETRS_LAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
proj4.utm33     <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4.lcc       <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
#
source("/home/cristianl/projects/sweet/functions/read_obsNet.r")
#
diff_t <- vector()
diff_all_t <- vector()
j<-0
h<-0
for (mm in c("01","02","03","04","05","06","07","08","09","10","11","12")) {
  path_mm <- file.path(path,mm)
  files <- list.files(path = path_mm, pattern = "_TAM_", full.names=T)
  for (file in files) {
    obsnet <- read_obsNet( file=file, crs_in=proj4.wgs84, crs_out=proj4.lcc,
                           extent_out=extent( extent[1], extent[2], 
                                              extent[3], extent[4]))
    x_points <- obsnet$x
    y_points <- obsnet$y
    z_points <- obsnet$z
    val  <- obsnet$val
    isin <- x_points >= extent_small[1] & x_points <= extent_small[2] & 
            y_points >= extent_small[3] & y_points <= extent_small[4]
    zseq <- seq( range( z_points[isin])[1], range( z_points[isin])[2], by=25)
    k <- 0
    for (i in 1:(length(zseq)-1)) {
      fout <- which( z_points>=zseq[i] & z_points<zseq[i+1] & !isin )
      fin  <- which( z_points>=zseq[i] & z_points<zseq[i+1] &  isin )
      if ( any(fout) & any(fin)) {
        j<-j+1
        diff_t[j] <- mean(val[fin]) - mean(val[fout])
        if (abs(diff_t[j]>2)) k <- k+1
      }
    }
    fout <- which( z_points>=zseq[1] & z_points<zseq[length(zseq)] & !isin )
    fin  <- which( z_points>=zseq[1] & z_points<zseq[length(zseq)] &  isin )
    h<-h+1
    diff_all_t[h]<-mean(val[fin]) - mean(val[fout])
#    if ( k>1) {
      aux <- strsplit(file,"/")
      aux1 <- aux[[1]][length(aux[[1]])]
      ffout <- file.path("png",paste0(aux1,".png"))
      print( paste( ffout, round(mean(val[fin]) - mean(val[fout]),3)))
      png(file=ffout,width=800,height=800)
      plot(val,z_points)
      points(val[isin],z_points[isin],pch=21,bg="darkred",cex=1.5)
      dev.off()
#    }
  }
}
save.image( paste0("statistics_for_sweet_t_",year,".rdata"))
#
q()
