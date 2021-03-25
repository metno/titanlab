
# Isolation check

  p <- add_argument(p, "--dr.isol",
                    help="check for the number of observation in a dr-by-dr square-box around each observation [m]",
                    type="numeric",
                    default=25000,
                    short="-dI")

  p <- add_argument(p, "--n.isol",
                    help="threshold (number of neighbouring observations) for the identification of isolated observations.",
                    type="integer",
                    default=10,
                    short="-nI")

