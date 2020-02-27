#!/bin/bash
#==============================================================================
../titan.r data/privateObs_20180326T08Z.txt ~/data/out.txt -v --variable T --input.files data/kdvh_Norway_wmo_2018032608.txt,data/kdvh_Norway_nowmo_nomet_2018032608.txt,data/kdvh_Norway_nowmo_met_2018032608.txt,data/smhi_Sweden_2018032608.txt -c --varname.elev elev,z,z,z,z --varname.value TA,TA,TA,TA,TA --prid 9,1,2,6,4 --dem.file data/dem.nc --dem.varname altitude --proj4dem "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06" --laf.file data/laf.nc --laf.varname land_area_fraction --proj4laf "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06" --laf.sct --dem --dem.fill --month.clim 3
#--fg.file data/meps_2_5km_20180326T00Z_test.nc --fg.t 201803260800 --fg.type meps --fg.demfile data/meps_2_5km_20180326T00Z_test.nc --debug --debug.dir ~/scratch --fge.file data/meps_2_5km_20180326T00Z_test.nc --fge.demfile data/meps_2_5km_20180326T00Z_test.nc --fge.t 201803260800 --fge.type meps 
#--fg --thr.fg 30 --fge --usefge.sct
