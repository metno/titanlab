#+
check_z_against_dem <- function( argv, data, z, zdem, nfin, dqcflag ) {
#==============================================================================
  if (argv$verbose | argv$debug) 
    print(paste0("check station elevations against digital elevation model (",argv$dem.code,")"))
  # set doit vector
  doit<-vector(length=ndata,mode="numeric")
  doit[]<-NA
  for (f in 1:nfin) doit[data$prid==argv$prid[f]]<-argv$doit.dem[f]
  # use only (probably) good observations
  ix<-which(is.na(dqcflag))
  if (length(ix)>0) {
    ixna<-which(!is.na(z) & !is.na(zdem) & is.na(dqcflag))
    sus<-which( abs(z[ixna]-zdem[ixna])>argv$dz.dem &
                doit[ixna]==1 )
    # set dqcflag
    if (length(sus)>0) dqcflag[ixna[sus]]<-argv$dem.code
  }  else {
    print("no valid observations left, no dem check")
  }
  if (argv$verbose | argv$debug) {
    print(paste("#stations with elevations too different from digital elevation model =",
                length(which(dqcflag==argv$dem.code))))
    print("+---------------------------------+")
  }
  rm(doit)
  if (argv$debug) 
    save.image(file.path(argv$debug.dir,"dqcres_demcheck.RData")) 
  #
  return(dqcflag)
}
