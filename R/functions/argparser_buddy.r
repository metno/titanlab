  #.............................................................................. 
  # Buddy-check
  p <- add_argument(p, "--buddy",
                    help="do the buddy check",
                    flag=T,
                    short="-Be")
  p <- add_argument(p, "--dr.buddy",
                    help="perform each test within a (2*dr)-by-(2*dr) square-box around each observation [m] (default is 3000m). This is a numeric vector with the same dimension of the vector specifying the thresholds",
                    type="numeric",
                    default=NA,
                    nargs=Inf,
                    short="-dB")
  p <- add_argument(p, "--i.buddy",
                    help="number of iterations",
                    type="integer",
                    default=1,
                    short="-iB")
  p <- add_argument(p, "--thr.buddy",
                    help="threshold(s) used to perform the quality checks. This is a numeric vector with the same dimension of the vector specifying the sizes of the square boxes. flag observation if: abs( obs - mean(buddies) ) / st_dev(buddies) > threshold. Default is 3.", 
                    type="numeric",
                    default=NA,
                    nargs=Inf,
                    short="-thB")
  p <- add_argument(p, "--sdmin.buddy",
                    help="minimum allowed value(s) for the standard deviation. This is a numeric vector with the same dimension of the vector specifying the sizes of the square boxes.",
                    type="numeric",
                    default=NA,
                    nargs=Inf)
  p <- add_argument(p, "--n.buddy",
                    help="consider an arbitrary observation, how many neighbouring observations do we need to call them 'buddies' and perform the check? We need a number greater than 'n'. This is a numeric vector with the same dimension of the vector specifying the sizes of the square boxes. Default is set to 5.",
                    default=NA,
                    nargs=Inf,
                    type="integer",
                    short="-nB")
  p <- add_argument(p, "--dz.buddy",
                    help="do not perform the check if at least one of the elevation differences between an observation and its buddies is equal to or greater than the threshold(s) 'dz'. This is a numeric vector with the same dimension of the vector specifying the sizes of the square boxes. Default is 30 m.",
                    type="numeric",
                    default=NA,
                    nargs=Inf,
                    short="-zB")
  p <- add_argument(p, "--prio.buddy",
                    help="specify priorities. This is a numeric vector, with the dimension of the number of providers. One positive value for each provider. The smaller the value, the greater the priority. Priorities are used only in the first round of the check, when each observation is compared only against other observations having a priority equal to or grater than its own.",
                    type="numeric",
                    default=NA,
                    nargs=Inf,
                    short="-prB")
  p <- add_argument(p, "--break.buddy",
                    help="break the loop if the number of flagged observations in the last iretation (by considering al the test) is euqual to or less than this value.",
                    type="numeric",
                    default=0)
  p <- add_argument(p, "--transf.buddy",
                    help="apply Box-Cox transformation",
                    flag=T)
