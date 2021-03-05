  #.............................................................................. 
  # cool check
  p <- add_argument(p, "--cool",
                    help="cool (Check fOr hOLes in the field) test",
                    flag=T)
  p <- add_argument(p, "--i.cool",
                    help="number of cool-test iterations",
                    type="integer",
                    default=1)
  p <- add_argument(p, "--thres.cool",
                    help="numeric vector with the thresholds used to define events (same units of the specified variable). A threshold transforms an observation into a binary event: observation is less than the threshold OR observations is greater or equal to it.",
                    type="numeric",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--condition.cool",
                    help="character vector specifying the conditions to apply at each of the thresholds (\"lt\"=less than; \"le\"=less or equal than; \"gt\"=greater than; \"ge\"=greater or equal than).",
                    type="character",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--n.cool",
                    help="minimum acceptable number of observations producing a ''hole'' in the field (specified as a function of the provider and the threshold). If a clump of connected cells originates from a small number of observations, then it cannot be properly resolved by the observational network. As a result, the observations causing those small-scale events are assumed to be affected by large representativeness errors and flagged as ''suspect''. Format: array (ntot[thres1],nprid1[thres1],nprid2[thres1],...,ntot[thres2],nprid1[thres2],nprid2[thres2],...)",
                    type="numeric",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--grid_res.cool",
                    help="resolution of the grid used to compute the field (same units as ).",
                    type="integer",
                    default=1000)
  p <- add_argument(p, "--dh_max.cool",
                    help="gridpoints having the nearest observation more than dh_max units apart are set to NAs.",
                    type="integer",
                    default=100000)
  p <- add_argument(p, "--break.cool",
                    help="break the loop if the number of flagged observations in the last iretation (by considering al the test) is euqual to or less than this value.",
                    type="numeric",
                    default=0)
