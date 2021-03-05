#!/bin/bash
#==============================================================================
export TITANR_PATH=/home/cristianl/projects/titanlab/R/functions
../titan.r --input.files data/privateObs_20180326T08Z.txt data/kdvh_Norway_wmo_2018032608.txt data/kdvh_Norway_nowmo_nomet_2018032608.txt data/kdvh_Norway_nowmo_met_2018032608.txt data/smhi_Sweden_2018032608.txt --output.file ~/data/out.txt --config.files ini/input.ini ini/test_titan.ini ini/buddy.ini ini/fgt.ini ini/sct_resistant.ini ini/sct_fg.ini ini/isolation.ini --fg.files ini/fg_det.ini ini/fg_ens.ini
