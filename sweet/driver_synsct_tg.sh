#!/bin/bash
#$ -N sweet 
#$ -S /bin/bash
#$ -l h_rt=36:00:00
#$ -pe shmem-1 1
#$ -q research-el7.q
#$ -l h_vmem=2500M
#==============================================================================
#
# load modules

  module load R/R-3.5.2
  module load hdf5/1.10.5-gcc
  module load netcdf
  module load gdal

# Constants
export R_LIBS=/home/cristianl/Rpackages-centos
export SWEET_PATH=/home/cristianl/projects/titanlab/sweet
export R_DEFAULT_PACKAGES="datasets","utils","grDevices","graphics","stats","methods"
#
ffsweet=/lustre/storeB/users/cristianl/sweet/synsct_tg/syntg_n41.dat
ffconf=/home/cristianl/projects/titanlab/sweet/etc/synsct_tg.ini
sct=/home/cristianl/projects/titanlab/sweet/synsct_tg.r
# sct param
synsct_tg_nens=500
#
#
a_vertprof_ix=$1
thinobs_perc=$2
#for pGE in 00 20 40; do
for pGE in 40; do
  for t in 02 04 09 16 25; do
    tpos_score=$t; tneg_score=$t
    ffout=/lustre/storeB/users/cristianl/sweet/synsct_tg/res/synsct_tg_res_a$a_vertprof_ix\_th$tpos_score\_pGE$pGE\_sel$thinobs_perc\_n$synsct_tg_nens.dat
    echo "$sct --ffin_sim $ffsweet --ffout $ffout --config_file $ffconf --tpos_score $tpos_score --tneg_score $tneg_score --pGE $pGE --a_vertprof_ix $a_vertprof_ix --thinobs_perc $thinobs_perc --synsct_tg_nens $synsct_tg_nens"
    $sct --ffin_sim $ffsweet --ffout $ffout --config_file $ffconf --tpos_score $tpos_score --tneg_score $tneg_score --pGE $pGE --a_vertprof_ix $a_vertprof_ix --thinobs_perc $thinobs_perc --synsct_tg_nens $synsct_tg_nens
    echo "written file "$ffout
  done
done
#
exit 0


