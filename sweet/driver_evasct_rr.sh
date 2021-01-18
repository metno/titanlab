#!/bin/bash
#
# constants
ffin_obs=/home/cristianl/projects/titanlab/sweet/etc/obs_RR_20200724.txt
eva=/home/cristianl/projects/titanlab/sweet/evasct_rr.r
# sct param
synsct_rr_nens=0002
bxcx=0.5
#
#
for rr_lscale in 50000; do
  for thinobs_perc in 00 50; do
    for pGE in 00 20 40; do
      $eva --ffin_obs $ffin_obs --pGE $pGE --rr_lscale $rr_lscale --thinobs_perc $thinobs_perc --t_score_eva 02 04 09 16 25 --t_sod_eva 02 04 09 16 25 --synsct_rr_nens $synsct_rr_nens --boxcox_lambda $bxcx
    done
  done
done
#
exit 0


