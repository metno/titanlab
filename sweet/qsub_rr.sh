#!/bin/bash
n=100
bxcx=0.3
#for l in 10000 25000 50000 75000 100000 150000 200000 400000; do 
for l in 10000 100000; do 
#  for sel in 00 50; do 
  for sel in 00; do 
    qsub -o /lustre/storeB/users/cristianl/sweet/synsct_rr/log/synsct_rr_res_l$l\_sel$sel\_b$bxcx\_n$n\.log -e /lustre/storeB/users/cristianl/sweet/synsct_rr/log/synsct_rr_res_l$l\_sel$sel\_b$bxcx\_n$n\.err driver_synsct_rr.sh $l $n $sel $bxcx
  done
done
#
exit 0
