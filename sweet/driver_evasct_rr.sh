#!/bin/bash
#
# constants
ffin_obs=/home/cristianl/projects/titanlab/sweet/etc/obs_RR_20200724.txt
eva=/home/cristianl/projects/titanlab/sweet/evasct_rr.r
# sct param
synsct_rr_nens=0100
bxcx=0.3
#
#
for rr_lscale in 50000; do
  for thinobs_perc in 00; do
    for pGE in 00 10 40; do
      for score in ets pod pofd; do
        $eva --ffin_obs $ffin_obs --pGE $pGE --rr_lscale $rr_lscale --thinobs_perc $thinobs_perc --t_score_eva 01 02 03 04 05 --synsct_rr_nens $synsct_rr_nens --boxcox_lambda $bxcx --eva_score $score
      done
    done
  done
done
#
exit 0
