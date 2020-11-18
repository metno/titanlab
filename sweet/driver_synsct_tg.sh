#!/bin/bash
#
# constants
ffsweet=/home/cristianl/data/sweet/synsct_tg/syntg_n41.dat
ffconf=/home/cristianl/projects/sweet/etc/synsct_tg.ini
sct=/home/cristianl/projects/sweet/synsct_tg.r
# sct param
synsct_tg_nens=010
#
#
for a_vertprof_ix in 01 10 21 30 41; do
  for thinobs_perc in 00 50; do
    for pGE in 00 20 40; do
      for t in 02 04 09 16 25; do
        tpos_score=$t; tneg_score=$t
        for t_sod in 02 04 09 16 25; do
          ffout=/home/cristianl/data/sweet/synsct_tg/res/synsct_tg_res_a$a_vertprof_ix\_th$tpos_score\_sod$t_sod\_pGE$pGE\_sel$thinobs_perc\_n$synsct_tg_nens.dat
          echo "$sct --ffin_sim $ffsweet --ffout $ffout --config_file $ffconf --tpos_score $tpos_score --tneg_score $tneg_score --t_sod $t_sod --pGE $pGE --a_vertprof_ix $a_vertprof_ix --thinobs_perc $thinobs_perc --synsct_tg_nens $synsct_tg_nens"
          $sct --ffin_sim $ffsweet --ffout $ffout --config_file $ffconf --tpos_score $tpos_score --tneg_score $tneg_score --t_sod $t_sod --pGE $pGE --a_vertprof_ix $a_vertprof_ix --thinobs_perc $thinobs_perc --synsct_tg_nens $synsct_tg_nens
          echo "written file "$ffout
exit 0
        done
      done
    done
  done
done
#
exit 0


