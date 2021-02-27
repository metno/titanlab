#!/bin/bash
#
# constants
ffsweet=/home/cristianl/data/sweet/synsct_tg/syntg_n41.dat
eva=/home/cristianl/projects/titanlab/sweet/evasct_tg.r
# sct param
synsct_tg_nens=500
#
#
jump=0
if [ "$jump" -eq "0" ]; then
  rm /home/cristianl/data/sweet/synsct_tg/res_png/evasct_tg_res.txt
  for a_vertprof_ix in 01 05 10 15 21 25 30 35 41; do
    for thinobs_perc in 00; do
      for pGE in 00 01 05 10 15 20 25 30 40 50; do
        for score in ets pod pofd; do
          echo "$eva --ffin_sim $ffsweet --pGE $pGE --a_vertprof_ix $a_vertprof_ix --thinobs_perc $thinobs_perc --t_score_eva 0.5 01 1.5 02 2.5 03 3.5 04 4.5 05 5.5 06 6.5 07 7.5 08 09 10 --synsct_tg_nens $synsct_tg_nens --eva_score $score"
          $eva --ffin_sim $ffsweet --pGE $pGE --a_vertprof_ix $a_vertprof_ix --thinobs_perc $thinobs_perc --t_score_eva 0.5 01 1.5 02 2.5 03 3.5 04 4.5 05 5.5 06 6.5 07 7.5 08 09 10 --synsct_tg_nens $synsct_tg_nens --eva_score $score
        done
      done
    done
  done
fi
#
#
dir="/home/cristianl/data/sweet/synsct_tg/res_png"
for l in 01 05 10 15 21 25 30 35 41; do
  for score in ets pod pofd; do
    convert +append $dir/synsct_tg_res_$score\vsth_a$l\_pGE01_sel00_n500.png $dir/synsct_tg_res_$score\vsth_a$l\_pGE05_sel00_n500.png $dir/synsct_tg_res_$score\vsth_a$l\_pGE10_sel00_n500.png $dir/top.png
    convert +append $dir/synsct_tg_res_$score\vsth_a$l\_pGE15_sel00_n500.png $dir/synsct_tg_res_$score\vsth_a$l\_pGE20_sel00_n500.png $dir/synsct_tg_res_$score\vsth_a$l\_pGE25_sel00_n500.png $dir/mid.png
    convert +append $dir/synsct_tg_res_$score\vsth_a$l\_pGE30_sel00_n500.png $dir/synsct_tg_res_$score\vsth_a$l\_pGE40_sel00_n500.png $dir/synsct_tg_res_$score\vsth_a$l\_pGE50_sel00_n500.png $dir/bot.png
    convert -append $dir/top.png $dir/mid.png $dir/bot.png $dir/synsct_tg_res_$score\vsth_a$l\_sel00_n500.png
    echo "written "$dir/synsct_tg_res_$score\vsth_a$l\_sel00_n500.png
    rm $dir/top.png $dir/mid.png $dir/bot.png
  done
done
#
exit 0


