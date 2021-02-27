  #.............................................................................. 
  # spatial consistency test
  p <- add_argument(p, "--sct",
                    help="do the sct",
                    flag=T)
  p <- add_argument(p, "--grid.sct",
                    help="nrow ncol (i.e. number_of_rows number_of_columns). SCT is performed independently over several boxes. The regular nrow-by-ncol grid is used to define those rectangular boxes where the SCT is performed.",
                    type="integer",
                    nargs=2,
                    default=c(20,20),
                    short="-gS")
  p <- add_argument(p, "--i.sct",
                    help="number of SCT iterations",
                    type="integer",
                    default=1,
                    short="-iS")
  p <- add_argument(p, "--background_elab_type.sct",
                    help="background elaboration type (\"vertical_profile\", \"mean_outer_circle\", \"external\")",
                    type="character",
                    default="vertical_profile")
  p <- add_argument(p, "--inner_radius.sct",
                    help="radius (m) of the inner circle",
                    type="numeric",
                    default=10000)
  p <- add_argument(p, "--outer_radius.sct",
                    help="radius (m) of the outer circle",
                    type="numeric",
                    default=100000)
  p <- add_argument(p, "--pmin.sct",
                    help="minimum number of neighbouring observations (within the outer circle) required to perform SCT",
                    type="integer",
                    default=5)
  p <- add_argument(p, "--pmin_Frei_vert_prof.sct",
                    help="minimum number of neighbouring observations (within the outer circle) required to compute the non-linear vertical profile as in the paper by Frei (2014)",
                    type="integer",
                    default=30)
  p <- add_argument(p, "--pmax.sct",
                    help="maximum number of neighbouring observations (within the outer circle) used in the SCT",
                    type="integer",
                    default=50)
  p <- add_argument(p, "--dz.sct",
                    help="minimum range of elevation in a box to run SCT [m]",
                    type="numeric",
                    default=30,
                    short="-zS")
  p <- add_argument(p, "--DhorMin.sct",
                    help=paste("OI, minimum allowed value for the horizontal de-correlation lenght (of the background error correlation) [m]"),
                    type="numeric",
                    default=10000,
                    short="-hS")
  p <- add_argument(p, "--DhorMax.sct",
                    help=paste("OI, maximum allowed value for the horizontal de-correlation lenght (of the background error correlation) [m]"),
                    type="numeric",
                    default=100000,
                    short="-hS")
  p <- add_argument(p, "--DhorKth.sct",
                    help=paste("OI, horizontal de-correlation lenght (of the background error correlation) is computed adaptively based on the distance to the closest observations, as specified by this value"),
                    type="numeric",
                    default=3)
  p <- add_argument(p, "--Dver.sct",
                    help="OI, vertical de-correlation lenght  (of the background error correlation) [m]",
                    type="numeric",
                    default=200,
                    short="-vS")
  p <- add_argument(p, "--eps2.sct",
                    help="OI, ratio between observation error variance and background error variance. eps2.sct is a vector of positive values (not NAs). If eps2.sct has the same length of the number of input files, then a provider dependent eps2 will be used in the SCT. Otherwise, the value of eps2.sct[1] will be used for all providers and any other eps2.sct[2:n] value will be ignored",
                    type="numeric",
                    default=NA,
                    nargs=Inf,
                    short="-eS")
  p <- add_argument(p, "--thr.sct",
                    help="SCT threshold. flag observation if: (obs-Cross_Validation_pred)^2/(varObs+varCVpred) > thr.sct. thr.sct is a vector of positive values (not NAs). If thr.sct has the same length of the number of input files, then a provider dependent threshold will be used in the SCT. Otherwise, the value of thr.sct[1] will be used for all providers and any other thr.sct[2:n] value will be ignored ",
                    type="numeric",
                    default=NA,
                    nargs=Inf,
                    short="-tS")
  p <- add_argument(p, "--thrpos.sct",
                    help="SCT threshold. flag observation if: (obs-Cross_Validation_pred)^2/(varObs+varCVpred) > thrpos.sct AND (obs-Cross_Validation_pred)>=0. thrpos.sct is a vector of positive values (not NAs). If thrpos.sct has the same length of the number of input files, then a provider dependent threshold will be used in the SCT. Otherwise, the value of thrpos.sct[1] will be used for all providers and any other thrpos.sct[2:n] value will be ignored ",
                    type="numeric",
                    default=NA,
                    nargs=Inf,
                    short="-tpS")
  p <- add_argument(p, "--thrneg.sct",
                    help="SCT threshold. flag observation if: (obs-Cross_Validation_pred)^2/(varObs+varCVpred) > thrneg.sct AND (obs-Cross_Validation_pred)<0.  thrneg.sct is a vector of positive values (not NAs). If thrneg.sct has the same length of the number of input files, then a provider dependent threshold will be used in the SCT. Otherwise, the value of thrneg.sct[1] will be used for all providers and any other thrneg.sct[2:n] value will be ignored ",
                    type="numeric",
                    default=NA,
                    nargs=Inf,
                    short="-tnS")
  p <- add_argument(p, "--thrsod.sct",
                    help="SCT Spatial Outlier Detection (SOD) threshold. thrsod.sct is a vector of positive values (not NAs). If thrsod.sct has the same length of the number of input files, then a provider dependent threshold will be used in the SCT. Otherwise, the value of thrsod.sct[1] will be used for all providers and any other thrsod.sct[2:n] value will be ignored ",
                    type="numeric",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--laf.sct",
                    help="use land area fraction in the OI correlation function (0-100%)",
                    flag=T,
                    short="-lS")
  p <- add_argument(p, "--lafmin.sct",
                    help="land area fraction influence in the OI correlation function",
                    type="numeric",
                    default=0.5,
                    short="-lmS")



  p <- add_argument(p, "--fast.sct",
                    help="faster spatial consistency test. Allow for flagging more than one observation simulataneously. Station by station mode, some more shortcuts are taken to speed up the elaboration",
                    flag=T,
                    short="-fS")
  p <- add_argument(p, "--smartbox.sct",
                    help="use smart boxes in the spatial consistency test for temperature",
                    flag=T)
  p <- add_argument(p, "--stn_by_stn.sct",
                    help="spatial consistency test, station by station mode",
                    flag=T)
  p <- add_argument(p, "--corr.sct",
                    help="correlation function to use (''gaussian'',''soar''",
                    type="character",
                    default="gaussian")
  p <- add_argument(p, "--box_o_nearest_halfwidth.sct",
                    help="half-width of the square box used to select the nearest observations",
                    type="numeric",
                    default=100000)
  p <- add_argument(p, "--succ_corr.sct",
                    help="successive correction step (yes/no)",
                    flag=T)
  p <- add_argument(p, "--o_errvar_min.sct",
                    help="minimum allowed value for the observation error variance",
                    type="numeric",
                    default=0.001)
  p <- add_argument(p, "--o_errvar_max.sct",
                    help="maximum allowed value for the observation error variance",
                    type="numeric",
                    default=4)
  p <- add_argument(p, "--xa_errvar_min.sct",
                    help="minimum allowed value for the analysis error variance",
                    type="numeric",
                    default=0.001)
  p <- add_argument(p, "--xa_errvar_max.sct",
                    help="maximum allowed value for the analysis error variance",
                    type="numeric",
                    default=4)
  p <- add_argument(p, "--fglab.sct",
                    help="method used to create the first-guess (\"linear\",\"Frei\",NA used together with usefg(e).sct )",
                    type="character",
                    default="Frei")
  p <- add_argument(p, "--fg_gamma.sct",
                    help="lapse rate value",
                    type="numeric",
                    default=-0.0065)
  p <- add_argument(p, "--transf.sct",
                    help="apply Box-Cox transformation before SCT (\"stn_by_stn.sct\")",
                    flag=T)
  p <- add_argument(p, "--break.sct",
                    help="break the loop if the number of flagged observations in the last iretation (by considering al the test) is euqual to or less than this value.",
                    type="numeric",
                    default=0)
