#!/bin/bash
#$ -N sweet_rr 
#$ -S /bin/bash
#$ -l h_rt=36:00:00
#$ -pe shmem-1 1
#$ -q research-el7.q
#$ -l h_vmem=3500M
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
dirout=/lustre/storeB/users/cristianl/sweet/rr
ffconf=/home/cristianl/projects/titanlab/sweet/etc/sweet_rr.ini
sweet=/home/cristianl/projects/titanlab/sweet/sweet.r
#
# sweet param
rr_lscale=$1
nsamples=$2
#
# initialization
mkdir -p $dirout
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
ffout=$dirout/synrr_l$l\_n$n.nc
#
# run the script
echo "$sweet --config_file $ffconf --nsamples $nsamples --ffout $ffout --rr_lscale $rr_lscale"
$sweet --config_file $ffconf --nsamples $nsamples --ffout $ffout --rr_lscale $rr_lscale
#
echo "Normal exit"
exit 0


