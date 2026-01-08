#' Bin a Numeric Variable
#' 
#' @name binVariable
#'
#' @details Create a factor dissecting the range of a numeric variable into bins of equal width, (roughly) equal frequency, or at "natural" cut points.
#' The \code{\link[base]{cut}} function is used to create the factor.
#'
#' @keywords manip
#'
#' @param x numeric variable to be binned.
#' @param bins number of bins.
#' @param method one of \code{"intervals"} for equal-width bins; \code{"proportions"} for equal-count bins; \code{"natural"} for cut points between bins to be determined by a k-means clustering.
#' @param labels if \code{FALSE}, numeric labels will be used for the factor levels; if \code{NULL}, the cut points are used to define labels; otherwise a character vector of level names.
#'
#' @return A factor.
#'
#' @author Dan Putler, slightly modified by John Fox (5 Dec 04 & 5 Mar 13) with the original author's permission.
#'
#' @seealso \code{\link[base]{cut}}, \code{\link[stats]{kmeans}}.
#'
#' @examples
#' summary(binVariable(rnorm(100), method="prop", labels=letters[1:4]))
#'
#' @export
binVariable <- function (x, bins=4, method=c("intervals", "proportions", "natural"), labels=FALSE) {
    method <- match.arg(method)
    
    if(length(x) < bins) {
        stop("The number of bins exceeds the number of data values")
    }
    x <- if(method == "intervals") cut(x, bins, labels=labels)
         else if (method == "proportions") cut(x, quantile(x, probs=seq(0,1,1/bins), na.rm=TRUE), include.lowest = TRUE, labels=labels)
         else {
             xx <- na.omit(x)
             breaks <- c(-Inf, tapply(xx, KMeans(xx, bins)$cluster, max))
             cut(x, breaks, labels=labels)
         }
    as.factor(x)
}

#' Bin a Numeric Variable
#'
#' @name bin.var
#'
#' @keywords internal
#' 
#' @details \code{bin.var} is a synomym for \code{binVariable}, retained for backwards compatibility.
#'
#' @param ... arguments to be passed to \code{binVariable}.
#'
#' @export
bin.var <- function(...) binVariable(...)
