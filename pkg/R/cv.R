#' Coefficient of variation
#' 
#' @name cv
#'
#' @keywords misc
#'
#' @details
#' \code{numSummary} creates neatly formatted tables of means, standard deviations, coefficients of variation, skewness, kurtosis, and quantiles of numeric variables. \code{CV} computes the coefficient of variation.
#'
#' @param x data a numeric vector, matrix, or data frame.
#' @param na.rm if \code{TRUE} (the default) remove \code{NA}s before computing the coefficient of variation.
#'
#' @return \code{cv} returns the coefficient(s) of variation.
#'
#' @author John Fox
#'
#' @examples
#' data(Prestige)
#' print(cv(Prestige[,c("income", "education")]))
#'
#' @export
cv <- function(x, na.rm=TRUE){
    x <- as.matrix(x)
    if (is.numeric(x)) {
        mean <- colMeans(x, na.rm=na.rm)
        sd <- apply(as.matrix(x), 2, stats::sd, na.rm=na.rm)
        if (any(x <= 0, na.rm=na.rm)) warning("not all values are positive")
        cv <- sd/mean
        cv[mean <= 0] <- NA
    } else {
        stop("x is not numeric")
    }
    cv
}

#' @rdname cv
#' @keywords internal
#' @export
CV <- function(x, na.rm = TRUE){
    warning("CV is deprecated in RcmdrMisc package. Use cv instead.")
    cv(x = x, na.rm = na.rm)
}
