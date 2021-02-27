#!/bin/bash
#==============================================================================
export TITANR_PATH=/home/cristianl/projects/titanlab/R/functions
../titan.r --input.files data/privateObs_20180326T08Z.txt data/kdvh_Norway_wmo_2018032608.txt data/kdvh_Norway_nowmo_nomet_2018032608.txt data/kdvh_Norway_nowmo_met_2018032608.txt data/smhi_Sweden_2018032608.txt --output.file ~/data/out.txt --config.file test_titan.ini --varname.elev elev,z,z,z,z --varname.value TA,TA,TA,TA,TA --prid 9,1,2,6,4 --sct --pmax.sct 200 --DhorMin.sct 1000 --DhorMax.sct 30000 --DhorKth.sct 5 --inner_radius.sct 30000 --outer_radius.sct 100000 --laf.sct --background_elab_type.sct vertical_profile --thr.sct 9 --thrpos.sct 9 --thrneg.sct 9 --thrsod.sct 4 --prio.buddy 4,1,3,2,1
#--fg.file data/meps_2_5km_20180326T00Z_test.nc --fg.t 201803260800 --fg.type meps --fg.demfile data/meps_2_5km_20180326T00Z_test.nc --debug --debug.dir ~/scratch --fge.file data/meps_2_5km_20180326T00Z_test.nc --fge.demfile data/meps_2_5km_20180326T00Z_test.nc --fge.t 201803260800 --fge.type meps 
#--fg --thr.fg 30 --fge --usefge.sct
