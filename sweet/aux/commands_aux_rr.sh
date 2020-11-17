#!/bin/bash
seas=$1 # summer / winter
if [ "$seas" == "summer" ]; then
  date1="2018-06-01"
  date2="2020-07-24"
  date_filter_by_month="6,7,8"
elif [ "$seas" == "winter" ]; then
  date1="2018-06-01"
  date2="2020-07-24"
  date_filter_by_month="12,1,2"
fi
ffout_summ_stat_ell="ellipsis_"$date1"_"$date2".txt"
ffout_summ_stat_gam="gammapar_"$date1"_"$date2".txt"
ffin_template="/home/cristianl/data/MEPSpp_lcc_MEPS/RR-MEPS/%Y/%m/MEPSpp_RR-MEPS_lcc_%Y%m%d.nc"
spider="/home/cristianl/projects/spider/spider.r"
#
#-------------------------------------------------------------------------------
$spider $date1\T06 --date2 $date2\T06 --config_file gamma_parest.cfg --ffin_template $ffin_template --ffout_summ_stat $ffout_summ_stat_gam --date_filter_by_month $date_filter_by_month
$spider $date1\T06 --date2 $date2\T06 --config_file ellipsis.cfg --ffin_template $ffin_template --ffout_summ_stat $ffout_summ_stat_ell --date_filter_by_month $date_filter_by_month
#
#-------------------------------------------------------------------------------
exit 0 
