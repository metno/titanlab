#!/bin/bash
#
# constants
ffin_obs=/home/cristianl/projects/titanlab/sweet/etc/obs_RR_20200724.txt
eva=/home/cristianl/projects/titanlab/sweet/evasct_rr.r
# sct param
synsct_rr_nens=0100
bxcx=0.3
#
jump=0
if [ "$jump" -eq "0" ]; then
  rm /home/cristianl/data/sweet/synsct_rr/res_png/evasct_rr_res.txt 
  for rr_lscale in 10000 50000 100000 200000 400000; do
    for thinobs_perc in 00; do
      for pGE in 00 01 02 03 04 05 07 10 12 15 17 20 22 25 27 30 32 35 37 40 45 50; do
#        for score in ets pod pofd; do
        for score in pod pofd; do
          $eva --ffin_obs $ffin_obs --pGE $pGE --rr_lscale $rr_lscale --thinobs_perc $thinobs_perc --t_score_eva 01 02 03 04 05 06 07 08 09 10 11 12 --synsct_rr_nens $synsct_rr_nens --boxcox_lambda $bxcx --eva_score $score
        done
      done
    done
  done
fi
#
dir="/home/cristianl/data/sweet/synsct_rr/res_png"
for l in 010000 050000 100000 200000 400000; do
  for score in ets pod pofd; do
    convert +append $dir/synsct_rr_res_$score\vsth_l$l\_b03_pGE01_sel00_n0100.png $dir/synsct_rr_res_$score\vsth_l$l\_b03_pGE02_sel00_n0100.png $dir/synsct_rr_res_$score\vsth_l$l\_b03_pGE05_sel00_n0100.png $dir/top.png
    convert +append $dir/synsct_rr_res_$score\vsth_l$l\_b03_pGE10_sel00_n0100.png $dir/synsct_rr_res_$score\vsth_l$l\_b03_pGE15_sel00_n0100.png $dir/synsct_rr_res_$score\vsth_l$l\_b03_pGE20_sel00_n0100.png $dir/mid.png
    convert +append $dir/synsct_rr_res_$score\vsth_l$l\_b03_pGE30_sel00_n0100.png $dir/synsct_rr_res_$score\vsth_l$l\_b03_pGE40_sel00_n0100.png $dir/synsct_rr_res_$score\vsth_l$l\_b03_pGE50_sel00_n0100.png $dir/bot.png
    convert -append $dir/top.png $dir/mid.png $dir/bot.png $dir/synsct_rr_res_$score\vsth_l$l\_b03_sel00_n0100.png
    echo "written "$dir/synsct_rr_res_$score\vsth_l$l\_b03_sel00_n0100.png
    rm $dir/top.png $dir/mid.png $dir/bot.png
  done
done
#
exit 0
