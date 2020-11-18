titanlib_path <- "/home/cristianl/projects/titanlib/build/extras"
#titanlib_path <- "/home/lineb/projects/titanlib/titanlib/build/extras"
dyn.load( file.path( titanlib_path, 
                     paste("SWIG/R/titanlib", .Platform$dynlib.ext, sep="")))
source(   file.path( titanlib_path,"SWIG/R/titanlib.R"))

#---------------------------------------------------------------
# Test with small vectors
lats = c(60, 60, 60, 60, 60)
lons = c(10, 10.005, 10.01, 10.015, 10.02)
elevs = c(0, 0, 0, 0, 0)
values = c(0, 1, 10, -10, -100)
#values = c(0, 0, 0, 0, -100)
obs_to_check = c(1, 1, 1, 1, 1)
background_values = c(0, 0, 0, 0, 0)
background_elab_type = "vertical_profile"
N = length(lats)
num_min = 3
num_max = 10
inner_radius = 20000
outer_radius = 50000
num_iterations = 10
num_min_prof = 0
min_elev_diff = 100
min_horizontal_scale = 10000
max_horizontal_scale = 100000
kth_closest_obs_horizontal_scale = 2
vertical_scale = 200
tpos_score = rep(1,N) * 16
tneg_score = rep(1,N) * 16
t_sod = rep(1,N) * 4
eps2 = rep(1,N) * 0.5
value_min = -50
value_max = 50
sig2o_min = 0.01
sig2o_max = 20
debug = T
res<-sct(lats, lons, elevs, values, obs_to_check, background_values, background_elab_type, num_min, num_max, inner_radius, outer_radius, num_iterations, num_min_prof, min_elev_diff, min_horizontal_scale, max_horizontal_scale, kth_closest_obs_horizontal_scale, vertical_scale, value_min, value_max, sig2o_min, sig2o_max, eps2, tpos_score, tneg_score, t_sod, debug)
print(res[[1]])

#--------------------------------------------------------
# Test with larger vectors
P = 30000
pGE = 0.3 # probability of gross error 0.3 = 30%
lats = runif(P, min = 55, max = 70)
lons = runif(P, min = 5, max = 30)
elevs = runif(P, min = 0, max = 2500)
# simple vertical profile
values <- 30 - 0.0065 * elevs
#values <- (103-1.333*lats) - 0.0065 * elevs
values[sample(1:P,ceiling(P*pGE))]<-runif(ceiling(P*pGE), min = -50, max = 50)
obs_to_check = rep(1,P)
background_values = 0
background_elab_type = "vertical_profile_Theil_Sen"
tpos_score = rep(3,P)
tneg_score = rep(3,P)
t_sod = rep(2,P)
eps2 = rep(0.5,P)
value_min = -50
value_max = 50
sig2o_min = 0.1
sig2o_max = 4
debug = F
num_min = 3
num_max = 50
inner_radius = 30000
outer_radius = 50000
num_iterations = 20
num_min_prof = 10
min_elev_diff = 500
min_horizontal_scale = 500
max_horizontal_scale = 10000
kth_closest_obs_horizontal_scale = 3
vertical_scale = 600

t0<-Sys.time()
res<-sct(lats, lons, elevs, values, obs_to_check, background_values, background_elab_type, num_min, num_max, inner_radius, outer_radius, num_iterations, num_min_prof, min_elev_diff, min_horizontal_scale, max_horizontal_scale, kth_closest_obs_horizontal_scale, vertical_scale, value_min, value_max, sig2o_min, sig2o_max, eps2, tpos_score, tneg_score, t_sod, debug)
t1<-Sys.time()
print(t1-t0)
flags<-res[[1]]
score<-res[[2]]
rep<-res[[3]]
sod<-res[[4]]
num_inner<-res[[5]]
horizontal_scale<-res[[6]]
an_inc<-res[[7]]
an_res<-res[[8]]
cv_res<-res[[9]]
innov<-res[[10]]
idi<-res[[11]]
idiv<-res[[12]]
sig2o<-res[[13]]
save.image("test_sct.rdata")
