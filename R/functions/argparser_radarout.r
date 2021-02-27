  #.............................................................................. 
  # radar-derived precipitation as output
  p <- add_argument(p, "--radarout",
                    help="include the radar-derived precipitation as output.",
                    flag=T,
                    short="-rado")
  p <- add_argument(p, "--radarout.prid",
                    help="provider identifier for the radar data",
                    type="numeric",
                    default=100,
                    short="-radop")
  p <- add_argument(p, "--radarout.aggfact",
                    help="aggregation factor for the radar-derived precipitation",
                    type="numeric",
                    default=3,
                    short="-radop")
  p <- add_argument(p, "--radarout.aggfun",
                    help="aggregation function for the radar-derived precipitation",
                    type="character",
                    default="mean")
  p <- add_argument(p, "--radarout.corep",
                    help="coefficient for the representativeness of radar-derived precipitation",
                    type="numeric",
                    default=1)
