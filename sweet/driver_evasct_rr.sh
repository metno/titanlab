#!/bin/bash
#
# constants
ffin_obs=/home/cristianl/projects/titanlab/sweet/etc/obs_RR_20200724.txt
eva=/home/cristianl/projects/titanlab/sweet/evasct_rr.r
# sct param
synsct_rr_nens=0100
bxcx=0.3
#
rm /home/cristianl/data/sweet/synsct_rr/res_png/evasct_rr_res.txt 
for rr_lscale in 10000 50000 100000 200000 400000; do
  for thinobs_perc in 00; do
    for pGE in 00 01 05 10 15 20 25 30 40 50; do
      for score in ets pod pofd; do
        $eva --ffin_obs $ffin_obs --pGE $pGE --rr_lscale $rr_lscale --thinobs_perc $thinobs_perc --t_score_eva 01 02 03 04 05 06 07 08 09 10 11 12 --synsct_rr_nens $synsct_rr_nens --boxcox_lambda $bxcx --eva_score $score
      done
    done
  done
exit 0
done
#
exit 0

convert +append synsct_rr_res_etsvsth_l050000_b03_pGE01_sel00_n0100.png synsct_rr_res_etsvsth_l050000_b03_pGE05_sel00_n0100.png synsct_rr_res_etsvsth_l050000_b03_pGE10_sel00_n0100.png synsct_rr_res_etsvsth_l050000_b03_pGE15_sel00_n0100.png top.png; convert +append synsct_rr_res_etsvsth_l050000_b03_pGE20_sel00_n0100.png synsct_rr_res_etsvsth_l050000_b03_pGE25_sel00_n0100.png synsct_rr_res_etsvsth_l050000_b03_pGE40_sel00_n0100.png synsct_rr_res_etsvsth_l050000_b03_pGE50_sel00_n0100.png bottom.png; convert -append top.png bottom.png fig.png
