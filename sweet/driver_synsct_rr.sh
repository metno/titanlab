#!/bin/bash
#$ -N sweetrr 
#$ -S /bin/bash
#$ -l h_rt=24:00:00
#$ -pe shmem-1 1
#$ -q research-el7.q
#$ -l h_vmem=2500M
#==============================================================================
#
# load modules

#  module load R/R-3.5.2
#  module load hdf5/1.10.5-gcc
#  module load netcdf
#  module load gdal

# Constants
#export R_LIBS=/home/cristianl/Rpackages-centos
#export SWEET_PATH=/home/cristianl/projects/titanlab/sweet
#export R_DEFAULT_PACKAGES="datasets","utils","grDevices","graphics","stats","methods"
#
ffconf=/home/cristianl/projects/titanlab/sweet/etc/synsct_rr.ini
ffin_obs=/home/cristianl/projects/titanlab/sweet/etc/obs_RR_20200724.txt
sct=/home/cristianl/projects/titanlab/sweet/synsct_rr.r
#
# sct param
rr_lscale=$1
nsamples=$2
thinobs_perc=$3
bxcx=$4 #0.3 0.5
#
if [ "$rr_lscale" -lt "10000" ]; then
  l=00$rr_lscale
elif [ "$rr_lscale" -lt "100000" ]; then
  l=0$rr_lscale
else
  l=$rr_lscale
fi
n=$nsamples
if [ "$n" -lt "10" ]; then
  n=000$nsamples
elif [ "$n" -lt "100" ]; then
  n=00$nsamples
elif [ "$n" -lt "1000" ]; then
  n=0$nsamples
fi
if [ "$bxcx" == "0.3" ]; then
  b=03
elif [ "$bxcx" == "0.5" ]; then
  b=05
fi
ffin_fields=/home/cristianl/data/sweet/rr/synrr_l$l\_n$n.nc
#
for pGE in 00 20 40; do
  for t in 02 04 09 16 25; do
    tpos_score=$t; tneg_score=$t
    for t_sod in 02 04 09 16 25; do
      ffout=/home/cristianl/data/sweet/synsct_rr/res/synsct_rr_res_l$l\_b$b\_th$tpos_score\_sod$t_sod\_pGE$pGE\_sel$thinobs_perc\_n$n.dat
      echo "$sct --ffin_fields $ffin_fields --ffin_obs $ffin_obs --ffout $ffout --config_file $ffconf --tpos_score $tpos_score --tneg_score $tneg_score --t_sod $t_sod --pGE $pGE --thinobs_perc $thinobs_perc --boxcox_lambda $bxcx"
      $sct --ffin_fields $ffin_fields --ffin_obs $ffin_obs --ffout $ffout --config_file $ffconf --tpos_score $tpos_score --tneg_score $tneg_score --t_sod $t_sod --pGE $pGE --thinobs_perc $thinobs_perc --boxcox_lambda $bxcx
      echo "written file "$ffout
    done
  done
done
#
exit 0


