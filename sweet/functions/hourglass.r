#' Illustration of SCT results by ''hourglasses'' as a contingency table
#'
#' Creates a plot of the spatial consistency test results. The graph has four panels and it is a visual representation of a contingency table. The points on the graphs rensembles the shape of an hourglass and this explain the name.
#'
#' @param ffout full file name of the output file
#' @param an_res analysis residuals, n-vector of numeric values (n=number of values)
#' @param cv_res cross-validation analysis residuals n-vector
#' @param ge gross error occurrences (truth), n-vector (1=GE, 0=ok)
#' @param dqc gross error occurrences (estimate from SCT), n-vector (1=GE, 0=ok)
#' @param sig2o observation error variances, n-vector 
#' @param sct sct-score, n-vector 
#' @param sod sod-score, n-vector 
#' @param par list of parameters. ''png_width'' and ''png_height''. ''undef''ined value. ''probs'' percentiles written on the text lines. ''sig2o'' values of observation error variance to use for plotting hyperbole. 
#'
#' @return None
#'
#' @references \url{Lussana, C., Uboldi, F. and Salvati, M.R. (2010), A spatial consistency test for surface observations from mesoscale meteorological networks. Q.J.R. Meteorol. Soc., 136: 1075-1088. https://doi.org/10.1002/qj.622}
#'
#' @examples
#' hourglass_contingencyTable()
#'
#' @export

hourglass_contingencyTable <- function( ffout  = NULL,
                                        an_res = NULL,
                                        cv_res = NULL,
                                        ge     = NULL,
                                        dqc    = NULL,
                                        sig2o  = NULL,
                                        sct    = NULL,
                                        sod    = NULL,
                                        par = list( png_width=1200,
                                                    png_height=1200,
                                                    undef=-999,
                                                    probs=c( 0.01, 0.10, 0.25, 0.5, 0.75, 0.9, 0.99),
                                                    sig2o=c(.1, 1, 10, 20)  )
                                      ) {
#==============================================================================
  # preliminary checks
  if ( (length(an_res) != length(cv_res)) | 
       (length(an_res) != length(ge)) | 
       (length(an_res) != length(dqc)) |
       (length(cv_res) != length(ge)) | 
       (length(cv_res) != length(dqc)) |
       (length(dqc) != length(ge))) {
    paste("Error: an_res cv_res ge dqc must all have the same length")
    return()
  } 
  # valid data
  aux <- an_res != par$undef & cv_res != par$undef & 
         is.finite(an_res) & is.finite(cv_res) & 
         !is.na(an_res) & !is.na(cv_res)
  ix <- which( aux )
  if (length(ix)==0) return()
  # contingency table
  ia <- which( aux & ge == 1 & dqc == 1) # hits
  ib <- which( aux & ge == 0 & dqc == 1) # false positives
  ic <- which( aux & ge == 1 & dqc == 0) # misses (false negatives?)
  id <- which( aux & ge == 0 & dqc == 0) # correct negatives
  # x and y axes must have the same range 
  xylim <- range( c( an_res[ix], cv_res[ix]), na.rm=T)
  if ( !is.null( ffout)) 
    png( file=ffout, width=par$png_width, height=par$png_height)
  par( mar=c(3,5,1,1))
  par( mfrow=c(2,2))
  #
  # -- a - hits ---------------------------------------------------------------
  plot( an_res[ix], cv_res[ix], 
        xlim=xylim, ylim=xylim, xlab="", ylab="", col="darkgray", axes=F)
  points( an_res[ia], cv_res[ia], pch=21, bg="gold")
  abline( h=0, v=0)
  axis( 1, cex.axis=2)
  axis( 2, cex.axis=2)
  n <- length( which( ge == 1 & dqc == 1))
  legend( x="bottomright", legend=paste( n, "hits (T1/F1)"), fill="gold", cex=2)
  lines( -1000:1000, -1000:1000, lwd=2, lty=2)
  lines( -1000:1000, (-1000:1000)*(1+1/argv$eps2), lwd=2, lty=3)
  abline( h=0, v=0)
  for (j in par$sig2o)
    lines( seq( -100, 100, by=0.1), 
           as.numeric(th)*j/seq(-100,100,by=0.1),
           lwd=2, lty=2, col="black")
  axis( 1, cex.axis=2)
  axis( 2, cex.axis=2)
  mtext( side=1, text="analysis residual", line=2.5, cex=2)
  mtext( side=2, text="cv-analysis residual", line=2.5, cex=2)
  box()
  if ( !is.null( sig2o)) 
    mtext( paste( "varo=", toString( round( as.numeric( quantile( sig2o[ia], probs=par$probs)), 2))), side=3, line=-1, adj=0)
  if ( !is.null( sct))
    mtext( paste( "sct=", toString( round( as.numeric( quantile( sct[ia], probs=par$probs)), 1))), side=3, line=-2, adj=0)
  if ( !is.null( sod))
    mtext( paste( "sod=", toString( round( as.numeric( quantile( sod[ia], probs=par$probs)), 1))), side=3, line=-3, adj=0)
  #
  # -- b - false positives ----------------------------------------------------
  plot( an_res[ix], cv_res[ix], 
        xlim=xylim, ylim=xylim, xlab="", ylab="", col="darkgray", axes=F)
  points( an_res[ib], cv_res[ib], pch=21, bg="pink")
  abline( h=0, v=0)
  axis(1,cex.axis=2)
  axis(2,cex.axis=2)
  n <- length( which( ge == 0 & dqc == 1))
  legend( x="bottomright", legend=paste( n, "false pos (T0/F1)"), fill="pink", cex=2)
  box()
  if ( !is.null( sig2o)) 
    mtext( paste( "varo=", toString( round( as.numeric( quantile( sig2o[ib], probs=par$probs)), 2))), side=3, line=-1, adj=0)
  if ( !is.null( sct))
    mtext( paste( "sct=", toString( round( as.numeric( quantile( sct[ib], probs=par$probs)), 1))), side=3, line=-2, adj=0)
  if ( !is.null( sod))
    mtext( paste( "sod=", toString( round( as.numeric( quantile( sod[ib], probs=par$probs)), 1))), side=3, line=-3, adj=0)
  #
  # -- c - false negatives ----------------------------------------------------
  plot( an_res[ix], cv_res[ix], 
        xlim=xylim, ylim=xylim, xlab="", ylab="", col="darkgray", axes=F)
  points( an_res[ic], cv_res[ic], pch=21, bg="cornflowerblue")
  abline( h=0, v=0)
  axis(1,cex.axis=2)
  axis(2,cex.axis=2)
  n <- length( which( ge == 1 & dqc == 0))
  legend( x="bottomright", legend=paste(n,"misses (T1/F0)"), fill="cornflowerblue", cex=2)
  box()
  if ( !is.null( sig2o)) 
    mtext( paste( "varo=", toString( round( as.numeric( quantile( sig2o[ic], probs=par$probs)), 2))), side=3, line=-1, adj=0)
  if ( !is.null( sct))
    mtext( paste( "sct=", toString( round( as.numeric( quantile( sct[ic], probs=par$probs)), 1))), side=3, line=-2, adj=0)
  if ( !is.null( sod))
    mtext( paste( "sod=", toString( round( as.numeric( quantile( sod[ic], probs=par$probs)), 1))), side=3, line=-3, adj=0)
  #
  # -- d - correct negatives --------------------------------------------------
  plot( an_res[ix], cv_res[ix], 
        xlim=xylim, ylim=xylim, xlab="", ylab="", col="darkgray", axes=F)
  points( an_res[id], cv_res[id], pch=21, bg="green")
  abline( h=0, v=0)
  axis(1,cex.axis=2)
  axis(2,cex.axis=2)
  n <- length( which( ge == 0 & dqc == 0))
  legend(x="bottomright",legend=paste(n,"corr neg (T0/F0)"),fill="green",cex=2)
  box()
  if ( !is.null( sig2o)) 
    mtext( paste( "varo=", toString( round( as.numeric( quantile( sig2o[id], probs=par$probs)), 2))), side=3, line=-1, adj=0)
  if ( !is.null( sct))
    mtext( paste( "sct=", toString( round( as.numeric( quantile( sct[id], probs=par$probs)), 1))), side=3, line=-2, adj=0)
  if ( !is.null( sod))
    mtext( paste( "sod=", toString( round( as.numeric( quantile( sod[id], probs=par$probs)), 1))), side=3, line=-3, adj=0)
  #
  # exit
  if ( !is.null( ffout)) {
    devnull <- dev.off()
    cat(paste("  written file",ffout,"\n"))
  }
}
