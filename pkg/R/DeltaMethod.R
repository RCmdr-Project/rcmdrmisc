#' Confidence Intervals by the Delta Method
#'
#' @name DeltaMethod
#' 
#' @aliases DeltaMethod print.DeltaMethod
#'
#' @keywords models
#'
#' @details \code{DeltaMethod} is a wrapper for the \code{\link[car]{deltaMethod}} function in the \pkg{car} package.
#' It computes the asymptotic standard error of an arbitrary, usually nonlinear, function of model coefficients, which are named \code{b0} (if there is an intercept in the model), \code{b1}, \code{b2}, etc., and based on the standard error, a confidence interval based on the normal distribution.
#'
#' @param model a regression model; see the \code{\link[car]{deltaMethod}} documentation.
#' @param g the expression --- that is, function of the coefficients --- to evaluate, as a character string.
#' @param level the confidence level, defaults to \code{0.95}.
#' @param x an object of class \code{"DeltaMethod"}.
#' @param ... optional arguments to pass to \code{print} to show the results.
#'
#' @return \code{DeltaMethod} returns an objects of class \code{"DeltaMethod"}, for which a \code{print} method is provided.
#'
#' @author John Fox
#'
#' @seealso \code{\link[car]{deltaMethod}} function in the \pkg{car} package.
#'
#' @examples
#' DeltaMethod(lm(prestige ~ income + education, data=Duncan), "b1/b2")
#' 
#' @export
DeltaMethod <- function(model, g, level=0.95){
    coefs <- coef(model)
    p <- length(coefs)
    nms <- if (names(coefs)[1] == "(Intercept)") paste0("b", 0:(p - 1)) else paste0("b", 1:p)
    res <- car::deltaMethod(model, g, level=level, parameterNames=nms)
    result <- list(test=res, coef=rbind(names(coefs), nms))
    class(result) <- "DeltaMethod"
    result
}

#' @rdname DeltaMethod
#' @export
print.DeltaMethod <- function(x, ...){
    coef <- x$coef
    par <- data.frame(t(coef))
    colnames(par) <- c("parameter", "name")
    print(par, row.names=FALSE)
    cat("\n")
    print(x$test)
    invisible(x)
}
