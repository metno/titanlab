# SWEET
SWEET - spatially consistent stochastic weather generator

This program generates synthtic data used to test the spatial consistency test for daily precipitation and temperature.

Instructions for daily temperature
==================================

Simulations of a case of temperature inversion in Folldal (Norway).

Two domains are considered. 
A "big" domain of 140 km by 130 km centered over Folldal.
A "Folldal" domain of 40 km by 35 km nested into the big one.
The locations where we assume the temperatures have been measured are those taken from a real case in July 2020. 
All the observation locations have been considered and they are divided into two main classes: observation from professional stations and observations from amateur stations.

The temperature data are generated for all the observation locations following a regular temperature profile.
The temperatures decrease with elevation.
The temperature at the surface is set to 20 degC. 
The lapse rate is set to the adiabatic lapse rate -0.0065 degC/m.

The experiments consist on running the SCT over all the available observation with different SCT configurations.

We will study the sensitivity of the system to variations on three main points:

   * In Folldal, temperatures follow a different vertical profile, which is the Frei's vertical profile that allows for inversions. We will vary the characteristics of the inversions and specifically the inversion strenght (a).

   * We assume that gross measurement errors can be present and we introduce them according to a predefined probability of gross error. We will vary the specification of the probability of gross measurement error (pGE).

   * We will vary the number of amateur stations (n).



Generate 41 different situations with the same vertical profile outside Folldal but different profiles in Folldal (a=-20,...0,...20).

```
$>export SWEET_PATH=/home/cristianl/projects/titanlab/sweet
$>/home/cristianl/projects/titanlab/sweet/sweet.r --ffin_obs /home/cristianl/projects/titanlab/sweet/etc/obs_TAM_20200724.txt --config_file /home/cristianl/projects/titanlab/sweet/etc/sweet_tg.ini --ffout /home/cristianl/data/sweet/synsct_tg/syntg_n41.dat
```

Run the SCT over the configurations, with different settings:

```
$>/home/cristianl/projects/titanlab/sweet/driver_synsct_tg.sh
```

Run the evaluation program, which creates one image for each configuration:

```
$>/home/cristianl/projects/titanlab/sweet/driver_evasct_tg.sh
```

Instructions for daily precipitation
====================================

Generate random Gaussian fields with a prescribed length scale of 50 km and 10 ensemble members:

```
$>export SWEET_PATH=/home/cristianl/projects/titanlab/sweet
$>./driver_sweet_rr.sh 50000 10
```

Run the SCT over the configurations, with different settings (length(m) nsamples thin-sel Box-Cox(0.3 or 0.5)):

```
$>/home/cristianl/projects/titanlab/sweet/driver_synsct_rr.sh 50000 10 00 0.3
```

For each configuration, run evaluation program:

```
$>/home/cristianl/projects/titanlab/sweet/driver_evasct_rr.sh
```
