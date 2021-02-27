  p <- add_argument(p, "--nometa.code",
                    help="quality code returned in case of missing metadata",
                    type="numeric",
                    default=1,
                    short="-nometac")
  p <- add_argument(p, "--p.code",
    help="quality code returned in case of the check on plausible values fails",
                    type="numeric",
                    default=2,
                    short="-pcodec")
  p <- add_argument(p, "--clim.code",
   help="quality code returned in case of the check on climatological values fails",
                    type="numeric",
                    default=3,
                    short="-climc")
  p <- add_argument(p, "--buddy.code",
    help="quality code returned in case of the buddy check fails",
                    type="numeric",
                    default=4,
                    short="-buddyc")
  p <- add_argument(p, "--sct.code",
                    help="quality code returned in case of SCT fails",
                    type="numeric",
                    default=5,
                    short="-sctc")
  p <- add_argument(p, "--dem.code",
                    help="quality code returned in case of SCT fails",
                    type="numeric",
                    default=6,
                    short="-demc")
  p <- add_argument(p, "--isol.code",
          help="quality code returned in case of isolation check fails",
                    type="numeric",
                    default=7,
                    short="-isolc")
  p <- add_argument(p, "--fg.code",
   help="quality code returned in case of check against a first-guess field (deterministic) fails",
                    type="numeric",
                    default=8,
                    short="-fgc")
  p <- add_argument(p, "--fge.code",
   help="quality code returned in case of check against a first-guess field (ensemble) fails",
                    type="numeric",
                    default=10,
                    short="-fgc")
  p <- add_argument(p, "--ccrrt.code",
                    help=paste("quality code returned in case of precipitation",
                               "and temperature crosscheck fails"),
                    type="numeric",
                    default=11,
                    short="-ccrrtc")
  p <- add_argument(p, "--cool.code",
               help=paste("quality code returned in case of cool check fails"),
                    type="numeric",
                    default=15,
                    short="-coolc")
  p <- add_argument(p, "--buddy_eve.code",
    help="quality code returned in case of the buddy check event-based fails",
                    type="numeric",
                    default=13,
                    short="-buddyec")
  p <- add_argument(p, "--black.code",
      help="quality code assigned to observations listed in the blacklist",
                    type="numeric",
                    default=100,
                    short="-blackc")
  p <- add_argument(p, "--keep.code",
      help="quality code assigned to observations listed in the keep-list",
                    type="numeric",
                    default=100,
                    short="-keepc")

