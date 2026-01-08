#' The Gumbel Distribution
#'
#' @name Gumbel
#'
#' @aliases Gumbel dgumbel pgumbel qgumbel rgumbel
#'
#' @keywords distribution
#' @details
#' Density, distribution function, quantile function and random generation for the Gumbel distribution with specified \code{location} and \code{scale} parameters.
#'
#' @param x vector of values of the variable.
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If \code{length(n)} > 1, the length is taken to be the number required.
#' @param location location parameter (default \code{0}); potentially a vector.
#' @param scale scale parameter (default \code{1}); potentially a vector.
#' @param lower.tail logical; if \code{TRUE} (the default) probabilities and quantiles correspond to \eqn{P(X \le x)}, if \code{FALSE} to  \eqn{P(X > x)}.
#'
#' @references
#' See \url{https://en.wikipedia.org/wiki/Gumbel_distribution} for details of the Gumbel distribution.
#'
#' @author John Fox
#'
#' @examples
#' x <- 100 + 5*c(-Inf, -1, 0, 1, 2, 3, Inf, NA)
#' dgumbel(x, 100, 5)
#' pgumbel(x, 100, 5)
#' p <- c(0, .25, .5, .75, 1, NA)
#' qgumbel(p, 100, 5)
#' summary(rgumbel(1e5, 100, 5))
#' 
#' @export
dgumbel <- function(x, location=0, scale=1){
    z <- (x - location)/scale
    d <- exp(-exp(-z))*exp(-z)/scale
    d[z == -Inf] <- 0
    d
}

#' @rdname Gumbel
#' @export
pgumbel <- function(q, location=0, scale=1, lower.tail=TRUE){
    p <- exp(-exp(- (q - location)/scale))
    if (lower.tail) p else 1 - p
}

#' @rdname Gumbel
#' @export
qgumbel <- function(p, location=0, scale=1, lower.tail=TRUE){
    if (!lower.tail) p <- 1 - p
    location - scale*log(-log(p))
}

#' @rdname Gumbel
#' @export
rgumbel <- function(n, location=0, scale=1){
    location - scale*log(-log(runif(n)))
}
