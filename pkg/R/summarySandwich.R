#' Linear Model Summary with Sandwich Standard Errors
#'
#' @name summarySandwich
#'
#' @aliases summarySandwich summarySandwich.lm
#' 
#' @keywords misc
#' 
#' @details
#' \code{summarySandwich} creates a summary of a \code{"lm"} object similar to the standard one, with sandwich estimates of the coefficient standard errors in the place of the usual OLS standard errors, also modifying as a consequence the reported t-tests and p-values for the coefficients.
#' Standard errors may be computed from a heteroscedasticity-consistent ("HC") covariance matrix for the coefficients (of several varieties), or from a heteroscedasticity-and-autocorrelation-consistent  ("HAC") covariance matrix.
#'
#' @param model a linear-model object.
#' @param type type of sandwich standard errors to be computed; see \code{\link[car]{hccm}} in the \pkg{car} package, and \code{\link[sandwich]{vcovHAC}} in the \pkg{sandwich} package, for details.
#' @param \dots arguments to be passed to \code{hccm} or \code{vcovHAC}.
#'
#' @return
#' an object of class \code{"summary.lm"}, with sandwich standard errors substituted for the usual OLS standard errors; the omnibus F-test is similarly adjusted.
#'
#' @author John Fox
#'
#' @seealso \code{\link[car]{hccm}}, \code{\link[sandwich]{vcovHAC}}.
#'
#' @examples
#' mod <- lm(prestige ~ income + education + type, data=Prestige)
#' summary(mod)
#' summarySandwich(mod)
#'
#' @export
summarySandwich <- function(model, ...){
    UseMethod("summarySandwich")
}

#' @rdname summarySandwich
#' @export
summarySandwich.lm <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4", "hac"), ...){
    s <- summary(model)
    c <- coef(s)
    type <- match.arg(type)
    v <- if (type != "hac") hccm(model, type=type, ...)
         else vcovHAC(model, ...)
    c[, 2] <- sqrt(diag(v))
    c[, 3] <- c[,1]/c[,2]
    c[, 4] <- 2*pt(abs(c[,3]), df=s$df[2], lower.tail=FALSE)
    colnames(c)[2] <- paste("Std.Err(", type, ")", sep="")
    s$coefficients <- c
    coefs <- names(coef(model))
    coefs <- coefs[coefs != "(Intercept)"]
    h <- linearHypothesis(model, coefs, vcov.=v)
    s$fstatistic <- c(value=h$F[2], numdf=length(coefs), dendf=s$df[2])
    s
}
