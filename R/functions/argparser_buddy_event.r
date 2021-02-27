  p <- add_argument(p, "--buddy_eve",
                    help="do the buddy check based on the definition of a binary event (yes/no)",
                    flag=T,
                    short="-Be")
  p <- add_argument(p, "--thr_eve.buddy_eve",
                    help=paste("threshold(s) used to define the binary event (same units as the variable). It is possible to specify more than one threshold, then titan executes more than one check. (same units of the specified variable). Consider an arbitrary observation, each threshold defines two events: (i) event='yes' when observed value is less than the threshold and (ii) event='no' when the observd value is greater or equal to the threhsold."),
                    type="numeric",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--dr.buddy_eve",
                    help="perform each test within a (2*dr)-by-(2*dr) square-box around each observation [m] (default is 3000m). This is a numeric vector with the same dimension of the vector specifying the thresholds",
                    type="numeric",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--i.buddy_eve",
                    help="number of iterations",
                    type="integer",
                    default=1,
                    short="-iBe")
  p <- add_argument(p, "--thr.buddy_eve",
                    help="threshold(s) used to perform the quality checks. This is a numeric vector with the same dimension of the vector specifying the thresholds defining the binary events. Consider the j-th elment of thr.buddy_eve, there are two mode of operations. Mode 'A', when thr.buddy_eve[j] is less than 1. Mode 'B', when thr.buddy_eve[j] is equal to or greater than 1. Mode 'A', thr.buddy_eve[j] specifies a percentage and an observation can be flagged as suspect for two reasons: if the observed event is 'yes' ('no') and the percentage of 'yes' ('no') among the buddies is equal to or less than thr.buddy_eve[j]. Mode 'B', thr.buddy_eve[j] specifies a number of observations and an observation can be flagged as suspect for two reasons: if the observed event is 'yes' ('no') and the number of 'yes' ('no') among the buddies is less than thr.buddy_eve[j]. The default values is 0.05, i.e. 5% of the neighbouring observations.",
                    type="numeric",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--n.buddy_eve",
                    help="consider an arbitrary observation, how many neighbouring observations do we need to call them 'buddies' and perform the check? We need a number greater than 'n'. This is a numeric vector with the same dimension of the vector specifying the thresholds defining the binary events. Default is set to 5.",
                    type="integer",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--dz.buddy_eve",
                    help="do not perform the check if at least one of the elevation differences between an observation and its buddies is equal to or greater than the threshold(s) 'dz'. This is a numeric vector with the same dimension of the vector specifying the thresholds defining the binary events. Default is 30 m.",
                    type="numeric",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--prio.buddy_eve",
                    help="specify priorities. This is a numeric vector, with the dimension of the number of providers. One positive value for each provider. The smaller the value, the greater the priority. Priorities are used only in the first round of the check, when each observation is compared only against other observations having a priority equal to or grater than its own.",
                    type="numeric",
                    default=NA,
                    nargs=Inf,
                    short="-prBe")
  p <- add_argument(p, "--break.buddy_eve",
                    help="break the loop if the number of flagged observations in the last iretation (by considering al the test) is euqual to or less than this value.",
                    type="numeric",
                    default=0)
