#!/usr/bin/env Rscript
# --~- synsct_rr.r -~--
# Test sct over synthetic data
# See the software repository here: https://github.com/cristianlussana/sweet
#..............................................................................
#Copyright and license
# Copyright (C) 2020 MET Norway. The software is licensed under GPL version 3 
# or (at your option) any later version.
# https://www.gnu.org/licenses/gpl-3.0.en.html
# 
# History:
# 09.11.2020 - Cristian Lussana. Original code.
# -----------------------------------------------------------------------------
#
rm( list = ls())
#
# -----------------------------------------------------------------------------
# Libraries
suppressPackageStartupMessages( library( "argparser"))
suppressPackageStartupMessages( library( "sp"))
suppressPackageStartupMessages( library( "raster"))
suppressPackageStartupMessages( library( "rgdal"))
suppressPackageStartupMessages( library( "ncdf4"))
suppressPackageStartupMessages( library( "dotnc"))
#options(warn = 2, scipen = 999)
options( scipen = 999999999)
#
#
#==============================================================================
# MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN -
#==============================================================================
t0<-Sys.time()
# path to the sweet.r
sweet_path <- Sys.getenv( "SWEET_PATH")
#------------------------------------------------------------------------------
# Load functions
for (file in list.files(path = file.path( sweet_path, "functions"),
                        pattern = ".r", full.names=T) ) 
  source(file)
rm(file)
#------------------------------------------------------------------------------
# Command line arguments
argv <- synsct_argparser()
#
#------------------------------------------------------------------------------
# Read input nc-file
dat <- read_sweetT( file=argv$ffin_sim, only_meta=T, close=F)
obsnet <- list( n=dat$meta$n, lat=dat$meta$lat, lon=dat$meta$lon,
                x=dat$meta$x, y=dat$meta$y, z=dat$meta$z,
                isin=dat$meta$isin)
nens <- dat$meta$nsamples
vertprof <- list( t0=dat$meta$t0, gamma=dat$meta$gamma,
                  h0=dat$meta$h0, h1i=dat$meta$h1i, a=dat$meta$a )
conn_in <- dat$conn
rm(dat)
#
  ff<-"/home/cristianl/data/geoinfo/meps_gmted2010_1km_topo_topdown.nc"
  ex<-as(extent(-340000,-150000,-180000,0),'SpatialPolygons')
  crs(ex)<-CRS("+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06")
  raux<-try(read_dotnc(nc.file=ff,
                       nc.varname="altitude",
                       topdown=F,
                       out.dim=list(ndim=3,
                                    tpos=3,
                                    epos=NULL,
                                    names=c("x","y","time")),
                       proj4="+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06",
                       nc.proj4=list(var=NULL,
                                     att=NULL),
                       selection=list(t=nc4.getTime(ff,format="%Y%m%d%H%M%S")[1],
                                      e=NULL,t_format="%Y%m%d%H%M%S")))
  raster <- disaggregate( crop(raux$stack,ex), fact=4,method="bilinear")


#print("-- obsNet --")
#print(obsnet$n)
#
#------------------------------------------------------------------------------
# synsct_tg_res_a01_th02_sod02_pGE00_sel00_n002.dat

dir_in<-"/home/cristianl/data/sweet/synsct_tg/res"
dir_out<-"/home/cristianl/data/sweet/synsct_tg/res_png"
i<-0
res<-list()
vth <- vector()
vscore <- vector()
vscore_in <- vector()
isin <- rep( obsnet$isin, argv$synsct_tg_nens)
argv$pGE <- formatC( argv$pGE, width=2, flag="0")
argv$thinobs_perc <- formatC( argv$thinobs_perc, width=2, flag="0")
argv$synsct_tg_nens <- formatC( argv$synsct_tg_nens, width=3, flag="0")
argv$a_vertprof_ix <- formatC( argv$a_vertprof_ix, width=2, flag="0")
if (argv$eva_score == "pofd") {
  score <- "pofd"
  scoreffout <- "pofd"
  score_lab <- "POFD"
  ylim <- c(0,0.83)
  type<-"above"
} else if (argv$eva_score == "pod") {
  score <- "pod"
  scoreffout <- "pod"
  score_lab <- "POD"
  ylim <- c(0,1.03)
  if (argv$pGE=="00") q()
  type<-"above"
} else if (argv$eva_score == "acc") {
  score <- "acc"
  scoreffout <- "acc"
  score_lab <- "ACC"
  ylim <- c(0,1)
  type<-"above"
} else if (argv$eva_score == "ets") {
  score <- "ets"
  scoreffout <- "ets"
  score_lab <- "ETS"
  ylim <- c(0,1.03)
  if (argv$pGE=="00") q()
  type<-"above"
} else if (argv$eva_score == "etsb") {
  score <- "ets"
  scoreffout <- "etsb"
  score_lab <- "ETSb"
  ylim <- c(0,1.03)
  if (argv$pGE=="00") q()
  type<-"below"
}

if ( !file.exists( file.path( dir_out, "evasct_tg_res.txt"))) 
    cat( file=file.path( dir_out, "evasct_tg_res.txt"), append=T,
         "score;a;th;pGE;thinobs_perc;nens;values;values_in;\n")

for (th in argv$t_score_eva) {
  ffin <- file.path( dir_in,
           paste0("synsct_tg_res_a",argv$a_vertprof_ix,"_th",th,
                  "_pGE",argv$pGE,"_sel",argv$thinobs_perc,
                  "_n",argv$synsct_tg_nens,".dat"))
  print( ffin)
  if ( !file.exists( ffin)) next
  i <- i+1
  res[[i]] <- read_sctRes( file=ffin)
  vth[i] <- th
  ens <- res[[i]][,1]
  val <- res[[i]][,5]
  flag <- res[[i]][,2]
  true_flag <- res[[i]][,4]
  ix <- which( flag %in% c(0,1))
  vscore[i] <- score_fun(x=flag[ix], x_ref=true_flag[ix], lab=score, threshold=.9, threshold1=.9, type=type) 
  ix_in <- which( flag %in% c(0,1) & isin == 1)
  vscore_in[i] <- score_fun(x=flag[ix_in], x_ref=true_flag[ix_in], lab=score, threshold=.9, threshold1=.9, type=type) 
  a <- length( which( true_flag[ix]==1 & flag[ix]==1))
  c <- length( which( true_flag[ix]==1 & flag[ix]==0))
  b <- length( which( true_flag[ix]==0 & flag[ix]==1))
  d <- length( which( true_flag[ix]==0 & flag[ix]==0))
  r <- as.numeric(a+c) * as.numeric(a+b) / as.numeric(a+b+c+d)
  ets <- as.numeric(a-r) / as.numeric(a+b+c-r)
  acc <- as.numeric(a+d)/as.numeric(a+b+c+d)
  pod <- as.numeric(a)/as.numeric(a+c)
  pofd <- as.numeric(b)/as.numeric(b+d)
  print( paste("th - tot - a(bad) b c d:", vth[i],"-",length(ix),"-",a,"(",length(which( true_flag[ix]==1)),")", b, c, d, a+b+c+d))
  print( paste("th - tot - acc pod pofd ets:", vth[i],"-",length(ix),"-",round(acc,2), round(pod,2), round(pofd,2), round(ets,2)))
  a <- length( which( true_flag[ix_in]==1 & flag[ix_in]==1))
  c <- length( which( true_flag[ix_in]==1 & flag[ix_in]==0))
  b <- length( which( true_flag[ix_in]==0 & flag[ix_in]==1))
  d <- length( which( true_flag[ix_in]==0 & flag[ix_in]==0))
  r <- as.numeric(a+c) * as.numeric(a+b) / as.numeric(a+b+c+d)
  ets <- as.numeric(a-r) / as.numeric(a+b+c-r)
  acc <- as.numeric(a+d)/as.numeric(a+b+c+d)
  pod <- as.numeric(a)/as.numeric(a+c)
  pofd <- as.numeric(b)/as.numeric(b+d)
  print( paste(" in: th - tot - a(bad) b c d:", vth[i],"-",length(ix_in),"-",a,"(",length(which( true_flag[ix_in]==1)),")", b, c, d, a+b+c+d))
  print( paste(" in: th - tot - acc pod pofd ets:", vth[i],"-",length(ix_in),"-",round(acc,2), round(pod,2), round(pofd,2), round(ets,2)))
  cat( file=file.path( dir_out, "evasct_tg_res.txt"), append=T,
         paste( score, argv$a_vertprof_ix, th,
                argv$pGE, argv$thinobs_perc, 
                argv$synsct_rr_nens, 
                round( vscore[i],5), round( vscore_in[i],5), sep=";", "\n"))
  plot_all <- T
  if (plot_all) {
    for (j in 1:10) {
      ffg<-file.path( dir_out, paste0("synsct_tg_graph_a",argv$a_vertprof_ix,"_th",th,"_pGE",argv$pGE,"_sel",argv$thinobs_perc, "_n",argv$synsct_tg_nens,"_ens",formatC(j,flag="0",width=3),".png"))
      ffm<-file.path( dir_out, paste0("synsct_tg_map_a",argv$a_vertprof_ix,"_th",th,"_pGE",argv$pGE,"_sel",argv$thinobs_perc, "_n",argv$synsct_tg_nens,"_ens",formatC(j,flag="0",width=3),".png"))
      ff<-file.path( dir_out, paste0("synsct_tg_fig_a",argv$a_vertprof_ix,"_th",th,"_pGE",argv$pGE,"_sel",argv$thinobs_perc, "_n",argv$synsct_tg_nens,"_ens",formatC(j,flag="0",width=3),".png"))
#      if (file.exists(ff)) next
      ix <- which( flag %in% c(0,1) & ens==j)
      s <- score_fun(x=flag[ix], x_ref=true_flag[ix], lab=score, threshold=.9, threshold1=.9, type="above") 
      ix_in <- which( flag %in% c(0,1) & ens==j & isin==1)
      sin <- score_fun(x=flag[ix], x_ref=true_flag[ix], lab=score, threshold=.9, threshold1=.9, type="above") 
      a <- length( which( true_flag[ix]==1 & flag[ix]==1))
      c <- length( which( true_flag[ix]==1 & flag[ix]==0))
      b <- length( which( true_flag[ix]==0 & flag[ix]==1))
      d <- length( which( true_flag[ix]==0 & flag[ix]==0))
      r <- as.numeric(a+c) * as.numeric(a+b) / as.numeric(a+b+c+d)
      ets <- as.numeric(a-r) / as.numeric(a+b+c-r)
      acc <- as.numeric(a+d)/as.numeric(a+b+c+d)
      pod <- as.numeric(a)/as.numeric(a+c)
      pofd <- as.numeric(b)/as.numeric(b+d)
      a <- length( which( true_flag[ix_in]==1 & flag[ix_in]==1))
      c <- length( which( true_flag[ix_in]==1 & flag[ix_in]==0))
      b <- length( which( true_flag[ix_in]==0 & flag[ix_in]==1))
      d <- length( which( true_flag[ix_in]==0 & flag[ix_in]==0))
      r <- as.numeric(a+c) * as.numeric(a+b) / as.numeric(a+b+c+d)
      ets_in <- as.numeric(a-r) / as.numeric(a+b+c-r)
      acc_in <- as.numeric(a+d)/as.numeric(a+b+c+d)
      pod_in <- as.numeric(a)/as.numeric(a+c)
      pofd_in <- as.numeric(b)/as.numeric(b+d)
  
      ix <- which( ens==j)
      ixiso <- which( ens==j & !(flag %in% c(0,1)) & flag>10)
      aux <- min(ix)
      xlim <- range(val[ix])
      ixa <- which( true_flag == 1 & flag == 1 & ens == j)
      ixc <- which( true_flag == 1 & flag == 0 & ens == j)
      ixb <- which( true_flag == 0 & flag == 1 & ens == j)
      ixd <- which( true_flag == 0 & flag == 0 & ens == j)
      iya <- which( true_flag == 1 & flag == 1 & ens == j & isin == 1)
      iyc <- which( true_flag == 1 & flag == 0 & ens == j & isin == 1)
      iyb <- which( true_flag == 0 & flag == 1 & ens == j & isin == 1)
      iyd <- which( true_flag == 0 & flag == 0 & ens == j & isin == 1)
      iza <- which( true_flag == 1 & flag == 1 & ens == j & isin == 0)
      izc <- which( true_flag == 1 & flag == 0 & ens == j & isin == 0)
      izb <- which( true_flag == 0 & flag == 1 & ens == j & isin == 0)
      izd <- which( true_flag == 0 & flag == 0 & ens == j & isin == 0)
      png(file=ffg,width=800,height=800)
      par( mar=c(5,5,1,1), mgp=c(3,1.3,0))
      plot( val[ix], obsnet$z, axes=F,xlab="",ylab="", pch=21, bg="white", col="white", cex=2, xlim=xlim)
      for (i in seq(-20,50,by=1)) lines(i-0.0065*0:2000,0:2000,lty=2,col="lightgray")
      points( val[izd], obsnet$z[izd-aux+1], pch=21, bg="darkgray", col="darkgray", cex=2)
      points( val[iza], obsnet$z[iza-aux+1], pch=21, bg="black", col="black", cex=2)
      points( val[izc], obsnet$z[izc-aux+1], pch=21, bg="cornflowerblue", col="cornflowerblue", cex=3)
      points( val[izb], obsnet$z[izb-aux+1], pch=21, bg="red", col="red", cex=3)
      points( val[iyd], obsnet$z[iyd-aux+1], pch=22, bg="darkgray", col="darkgray", cex=2)
      points( val[iya], obsnet$z[iya-aux+1], pch=22, bg="black", col="black", cex=2)
      points( val[iyc], obsnet$z[iyc-aux+1], pch=22, bg="cornflowerblue", col="cornflowerblue", cex=3)
      points( val[iyb], obsnet$z[iyb-aux+1], pch=22, bg="red", col="red", cex=3)
      points( val[ixiso], obsnet$z[ixiso-aux+1], cex=2)
      axis(1,cex.axis=2.5)
      mtext(side=1,line=3.5,text="Temperature [degC]",cex=3)
      mtext(side=2,line=3.2,text="[m amsl]",cex=2)
      axis(2,cex.axis=2.5)
      text( x=xlim[1]+0.6*diff(xlim), y=1470, cex=2.5, labels=paste0("P(GE)=",argv$pGE,"%  T=",as.numeric(th)), adj=c(0,0))
      text( x=xlim[1]+0.7*diff(xlim), y=1400, cex=2.4, labels=paste0("all ETS=",round(ets,2)), adj=c(0,0))
      text( x=xlim[1]+0.7*diff(xlim), y=1350, cex=2.4, labels=paste0("all POD=",round(pod,2)), adj=c(0,0))
      text( x=xlim[1]+0.7*diff(xlim), y=1300, cex=2.4, labels=paste0("all POFD=",round(pofd,2)), adj=c(0,0))
      text( x=xlim[1]+0.7*diff(xlim), y=1200, cex=2.4, labels=paste0("sel ETS=",round(ets_in,2)), adj=c(0,0))
      text( x=xlim[1]+0.7*diff(xlim), y=1150, cex=2.4, labels=paste0("sel POD=",round(pod_in,2)), adj=c(0,0))
      text( x=xlim[1]+0.7*diff(xlim), y=1100, cex=2.4, labels=paste0("sel POFD=",round(pofd_in,2)), adj=c(0,0))
      box()
      dev.off()
      png(file=ffm,width=800,height=800)
      par( mar=c(1,1,1,1))
      plot( obsnet$x, obsnet$y, axes=F,xlab="",ylab="", pch=21, bg="white", col="white", cex=2)
      contour( raster,add=T,col="lightgray",drawlabels=F)
      points( obsnet$x[izd-aux+1], obsnet$y[izd-aux+1], pch=21, bg="gray70", col="gray70", cex=2)
      points( obsnet$x[iza-aux+1], obsnet$y[iza-aux+1], pch=21, bg="black", col="black", cex=2)
      points( obsnet$x[izc-aux+1], obsnet$y[izc-aux+1], pch=21, bg="cornflowerblue", col="cornflowerblue", cex=3)
      points( obsnet$x[izb-aux+1], obsnet$y[izb-aux+1], pch=21, bg="red", col="red", cex=3)
      points( obsnet$x[iyd-aux+1], obsnet$y[iyd-aux+1], pch=22, bg="gray70", col="gray70", cex=2)
      points( obsnet$x[iya-aux+1], obsnet$y[iya-aux+1], pch=22, bg="black", col="black", cex=2)
      points( obsnet$x[iyc-aux+1], obsnet$y[iyc-aux+1], pch=22, bg="cornflowerblue", col="cornflowerblue", cex=3)
      points( obsnet$x[iyb-aux+1], obsnet$y[iyb-aux+1], pch=22, bg="red", col="red", cex=3)
      points( obsnet$x[ixiso-aux+1], obsnet$y[ixiso-aux+1], pch=21, bg="white", col="black", cex=2)
      legend(x="topleft",pch=21,cex=2,
             col=c("black","black","red","cornflowerblue","gray70"),
             pt.bg=c("white","black","red","cornflowerblue","gray70"),
             legend=c("isolated","corr.bad","false bad","false good","corr.good"))
      rect(-180000,-155000,-177000,-145000,angle=45,density=20)
      rect(-180000,-155000,-177000,-145000,angle=-45,density=20)
      rect(-180000,-145000,-177000,-135000,angle=-45,density=20)
      rect(-205000,-160000,-195000,-163000,angle=45,density=20)
      rect(-195000,-160000,-185000,-163000,angle=-45,density=20)
      rect(-195000,-160000,-185000,-163000,angle=45,density=20)
      text(x=-185000,y=-158000,"0 km",cex=1.8,col="gray40")
      text(x=-195000,y=-158000,"10",cex=1.8,col="gray40")
      text(x=-205000,y=-158000,"20",cex=1.8,col="gray40")
      text(x=-182000,y=-155000,"0",cex=1.8,col="gray40")
      text(x=-183000,y=-145000,"10",cex=1.8,col="gray40")
      text(x=-183000,y=-135000,"20",cex=1.8,col="gray40")
      box()
      dev.off()
      system(paste("convert +append",ffm,ffg,ff))
      system(paste("rm",ffg,ffm))
      print( paste("written file",ff))
    }
  }
}
q()
#
#------------------------------------------------------------------------------
ffout <- file.path( dir_out, 
                    paste0("synsct_tg_res_",scoreffout,
                           "vsth_a",argv$a_vertprof_ix,
                           "_pGE",argv$pGE,"_sel",argv$thinobs_perc,
                           "_n",argv$synsct_tg_nens,".png"))
png( file=ffout, width=800, height=800)
par(mar=c(4,4,1,1))
par(mgp=c(3,1.8,0))
plot( as.numeric(vth), vscore, xlab="",ylab="", main="", axes=F, ylim=ylim)
#if (argv$eva_score == "pofd") abline(h=seq(0,1,by=0.01),col="gray",lty=3) 
abline(v=1:10,lty=2,col="gray")
abline(v=seq(0.5,100,by=1),lty=3,col="gray")
abline(h=seq(0,1,by=0.05),col="gray",lty=3)
abline(h=seq(0,1,by=0.1),col="gray",lty=2)
abline(h=c(0,1))
lines(vth,vscore,col="black",lwd=4)
points( as.numeric(vth), vscore, pch=21, bg="gray",cex=4)
lines(vth,vscore_in,col="red",lwd=4)
points( as.numeric(vth), vscore_in, pch=21, bg="pink",cex=4)
axis(1,cex.axis=3,at=1:10)
axis(2,cex.axis=3)
#mtext(1,text="SCT threshold",line=3, cex=2)
#mtext(2,text=score_lab,line=2, cex=2)
#mtext(3,text=paste("l=",argv$rr_lscale," b=",argv$boxcox_lambda," pGE=",argv$pGE," sel=",argv$thinobs_perc," n=",argv$synsct_rr_nens),line=2, cex=2)
text(y=(ylim[2]-0.0*(ylim[2]-ylim[1])), x=8, paste0("P(GE)=",as.integer(argv$pGE),"%"), cex=4 )
if (argv$eva_score == "pofd") {
  text(y=(ylim[2]-0.0*(ylim[2]-ylim[1])), x=1.5, score_lab, col="darkgray", cex=3 )
} else {
  text(y=(ylim[2]-0.0*(ylim[2]-ylim[1])), x=1.3, score_lab, col="darkgray", cex=3 )
}
box()
devnull <- dev.off()
cat(paste("  written file",ffout,"\n"))
q()
