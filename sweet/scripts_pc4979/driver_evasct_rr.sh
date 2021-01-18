#!/bin/bash
#
# constants
ffin_obs=/home/cristianl/projects/titanlab/sweet/etc/obs_RR_20200724.txt
eva=/home/cristianl/projects/titanlab/sweet/evasct_rr.r
# sct param
synsct_rr_nens=0100
#synsct_rr_nens=0002
#bxcx=0.5
bxcx=0.3
#
#
#for rr_lscale in 010000 025000 050000 075000 100000 150000 200000 400000; do
for rr_lscale in 010000 025000 050000 400000; do
#for rr_lscale in 050000; do
  for thinobs_perc in 00 50; do
#  for thinobs_perc in 00; do
    for pGE in 00 05 10 20 40; do
#    for pGE in 05; do
      echo "$eva --ffin_obs $ffin_obs --pGE $pGE --rr_lscale $rr_lscale --thinobs_perc $thinobs_perc --t_score_eva 02 04 09 16 25 --t_sod_eva 00 --synsct_rr_nens $synsct_rr_nens --boxcox_lambda $bxcx --eps2 0.1 --ffin_fields /home/cristianl/data/sweet/rr/synrr_l$rr_lscale\_n$synsct_rr_nens.nc"
      $eva --ffin_obs $ffin_obs --pGE $pGE --rr_lscale $rr_lscale --thinobs_perc $thinobs_perc --t_score_eva 02 04 09 16 25 --t_sod_eva 00 --synsct_rr_nens $synsct_rr_nens --boxcox_lambda $bxcx --eps2 0.1 --ffin_fields /home/cristianl/data/sweet/rr/synrr_l$rr_lscale\_n$synsct_rr_nens.nc
    done
  done
done
#
exit 0


