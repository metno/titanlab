 ./plot_synfields_rr.r --ffin_fields /home/cristianl/data/sweet/rr/synrr_l010000_n0100.nc --ffin_obs ../etc/obs_RR_20200724.txt --ffout synrr_l010000_00
 ./plot_synfields_rr.r --ffin_fields /home/cristianl/data/sweet/rr/synrr_l100000_n0100.nc --ffin_obs ../etc/obs_RR_20200724.txt --ffout synrr_l100000_00
 ./plot_synfields_rr.r --ffin_fields /home/cristianl/data/sweet/rr/synrr_l200000_n0100.nc --ffin_obs ../etc/obs_RR_20200724.txt --ffout synrr_l200000_00
convert +append synrr_l010000_001.png synrr_l010000_002.png synrr_l010000_003.png toprow.png
convert +append synrr_l100000_001.png synrr_l100000_002.png synrr_l100000_003.png mediumrow.png
convert +append synrr_l200000_001.png synrr_l200000_002.png synrr_l200000_003.png bottomrow.png
convert +append toprow.png mediumrow.png bottomrow.png fig_synrr.png

