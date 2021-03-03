#!/bin/bash
#==============================================================================
export TITANR_PATH=/home/cristianl/projects/titanlab/R/functions
../titan.r --input.files data/privateObs_20180326T08Z.txt data/kdvh_Norway_wmo_2018032608.txt data/kdvh_Norway_nowmo_nomet_2018032608.txt data/kdvh_Norway_nowmo_met_2018032608.txt data/smhi_Sweden_2018032608.txt --output.file ~/data/out.txt --config.files ini/input.ini ini/test_titan.ini ini/buddy.ini ini/fgt.ini --fg.files ini/fg_det.ini ini/fg_ens.ini --sct --pmax.sct 200 --DhorMin.sct 1000 --DhorMax.sct 30000 --DhorKth.sct 5 --inner_radius.sct 30000 --outer_radius.sct 100000 --laf.sct --background_elab_type.sct vertical_profile --thr.sct 9 --thrpos.sct 9 --thrneg.sct 9 --thrsod.sct 4
