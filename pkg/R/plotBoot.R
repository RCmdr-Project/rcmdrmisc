#' Plot Bootstrap Distributions
#' 
#' @name plotBoot
#'
#' @aliases plotBoot plotBoot.boot
#'
#' @keywords hplot
#' 
#' @details
#' The function takes an object of class \code{"boot"} and creates an array of density estimates for the bootstrap distributions of the parameters.
#'
#' Creates an array of adaptive kernal density plots, using \code{\link[car]{densityPlot}} in the \pkg{car} package, showing the bootstrap distribution, point estimate ,and (optionally) confidence limits for each parameter.
#' 
#' @param object an object of class \code{"boot"}.
#' @param confint an object of class \code{"confint.boot"} (or an ordinary 2-column matrix) containing confidence limits for the parameters in \code{object}; if \code{NULL} (the default), these are computed from the first argument, using the defaults for \code{"boot"} objects.
#' @param \dots not used
#'
#' @return
#' Invisibly returns the object produced by \code{densityPlot}.
#'
#' @author John Fox
#'
#' @seealso \code{\link[car]{densityPlot}}
#'
#' @examples
#' \dontrun{
#' plotBoot(Boot(lm(prestige ~ income + education + type, data=Duncan)))
#' }
#'
#' @export
plotBoot <- function(object, confint=NULL, ...){
    UseMethod("plotBoot")
}

#' @rdname plotBoot
#' @export
plotBoot.boot <- function(object, confint=NULL, ...){
    mfrow <- function (n) {
        rows <- round(sqrt(n))
        cols <- ceiling(n/rows)
        c(rows, cols)
    }
    if (is.null(confint)) confint <- confint(object)
    t0 <- object$t0
    t <- object$t
    if (any(is.na(t))){
        t <- na.omit(t)
        warning("bootstrap samples with missing parameter values suppressed")
    }
    npars <- length(t0)
    pars <- names(t0)
    savepar <- par(mfrow=mfrow(npars), oma=c(0, 0, 2, 0), mar=c(5.1, 4.1, 2.1, 2.1))
    on.exit(par(savepar))
    for (i in 1:npars){
        car::densityPlot(t[, i], xlab=pars[i], method="adaptive")
        abline(v=t0[i], lty=2, col="blue")
        abline(v=confint[i, ], lty=2, col="magenta")
    }
    title(main="Bootstrap Distributions", outer=TRUE, line=0.5)
}
