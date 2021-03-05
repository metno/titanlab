# not really spatial data quality control

  p <- add_argument(p, "--nometa.code",
                    help="quality code returned in case of missing metadata",
                    type="numeric",
                    default=901)

  p <- add_argument(p, "--dem.code",
                    help="quality code returned in case of SCT fails",
                    type="numeric",
                    default=902)

  p <- add_argument(p, "--ccrrt.code",
                    help=paste("quality code returned in case of precipitation",
                               "and temperature crosscheck fails"),
                    type="numeric",
                    default=301)

  p <- add_argument(p, "--p.code",
    help="quality code returned in case of the check on plausible values fails",
                    type="numeric",
                    default=501)

  p <- add_argument(p, "--clim.code",
   help="quality code returned in case of the check on climatological values fails",
                    type="numeric",
                    default=502)

# statistics of deviations

  p <- add_argument(p, "--buddy.code",
    help="quality code returned in case of the buddy check fails",
                    type="numeric",
                    default=10)

  p <- add_argument(p, "--fgt.code",
   help="quality code returned in case of check against a first-guess field fails",
                    type="numeric",
                    default=11)

# statistics of deviations, spatial analysis

  p <- add_argument(p, "--sct.code",
                    help="quality code returned in case of SCT fails",
                    type="numeric",
                    default=1)

  p <- add_argument(p, "--sct_fg.code",
                    help="quality code returned in case of SCT fails",
                    type="numeric",
                    default=2)

# isolation

  p <- add_argument(p, "--isol.code",
          help="quality code returned in case of isolation check fails",
                    type="numeric",
                    default=800)

# blacklist/ keeplist

  p <- add_argument(p, "--black.code",
      help="quality code assigned to observations listed in the blacklist",
                    type="numeric",
                    default=999)

  p <- add_argument(p, "--keep.code",
      help="quality code assigned to observations listed in the keep-list",
                    type="numeric",
                    default=990)

