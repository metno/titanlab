#!/bin/bash
#
# constants
ffsweet=/home/cristianl/data/sweet/synsct_tg/syntg_n41.dat
eva=/home/cristianl/projects/titanlab/sweet/evasct_tg.r
# sct param
synsct_tg_nens=500
#
#
for a_vertprof_ix in 01 10 21 30 41; do
  for thinobs_perc in 00 50; do
    for pGE in 00 20 40; do
      echo "$eva --ffin_sim $ffsweet --pGE $pGE --a_vertprof_ix $a_vertprof_ix --thinobs_perc $thinobs_perc --t_score_eva 02 04 09 16 25 --t_sod_eva 02 04 09 16 25 --synsct_tg_nens $synsct_tg_nens"
      $eva --ffin_sim $ffsweet --pGE $pGE --a_vertprof_ix $a_vertprof_ix --thinobs_perc $thinobs_perc --t_score_eva 02 04 09 16 25 --t_sod_eva 02 04 09 16 25 --synsct_tg_nens $synsct_tg_nens
    done
  done
done
#
exit 0


