#' Normality Tests
#'
#' @name normalityTest
#'
#' @aliases normalityTest normalityTest.default normalityTest.formula
#'
#' @keywords htest
#' 
#' @details
#' Perform one of several tests of normality, either for a variable or for a variable by groups.
#' The \code{normalityTest} function uses the \code{\link[stats]{shapiro.test}} function or one of several functions in the \pkg{nortest} package.
#' If tests are done by groups, then adjusted p-values, computed by the Holm method, are also reported (see \code{\link[stats]{p.adjust}}).
#'
#' @param x numeric vector or formula.
#' @param formula one-sided formula of the form \code{~x} or two-sided formula of the form \code{x ~ groups}, where \code{x} is a numeric variable and \code{groups} is a factor.
#' @param data a data frame containing the data for the test.
#' @param test quoted name of the function to perform the test.
#' @param groups optional factor to divide the data into groups.
#' @param vname optional name for the variable; if absent, taken from \code{x}.
#' @param gname optional name for the grouping factor; if absent, taken from \code{groups}.
#' @param \dots any arguments to be passed down; the only useful such arguments are for the \code{\link[nortest]{pearson.test}} function in the \pkg{nortest} package.
#'
#' @return
#' If testing by groups, the function invisibly returns \code{NULL}; otherwise it returns an object of class \code{"htest"}, which normally would be printed.
#'
#' @author John Fox
#'
#' @seealso \code{\link[stats]{shapiro.test}}, \code{\link[nortest]{ad.test}}, \code{\link[nortest]{cvm.test}}, \code{\link[nortest]{lillie.test}}, \code{\link[nortest]{pearson.test}}, \code{\link[nortest]{sf.test}}.
#'
#' @examples
#' data(Prestige, package="car")
#' with(Prestige, normalityTest(income))
#' normalityTest(income ~ type, data=Prestige, test="ad.test")
#' normalityTest(~income, data=Prestige, test="pearson.test", n.classes=5)
#'
#' @export 
normalityTest <- function(x, ...){
    UseMethod("normalityTest")
}

#' @rdname normalityTest
#' @export
normalityTest.formula <- function(formula, test, data, ...){
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (missing(test)) test <- NULL
    if (ncol(mf) == 1) normalityTest(mf[, 1], test=test, vname=colnames(mf), ...)
    else if (ncol(mf) == 2) normalityTest(mf[, 1], test=test, groups=mf[, 2], vname=colnames(mf)[1], 
                                          gname=colnames(mf)[2], ...)
    else stop("the formula must specify one or two variables")
}

#' @rdname normalityTest
#' @export
normalityTest.default <- function(x, 
                                  test=c("shapiro.test", "ad.test", "cvm.test", "lillie.test", "pearson.test", "sf.test"),
                                  groups, vname, gname, ...){
    test <- match.arg(test)
    if (missing(vname)) vname <- deparse(substitute(x))
    if (missing(groups)){
        result <- do.call(test, list(x=x, ...))
        result$data.name <- vname
        result
    }
    else {
        if (!is.factor(groups)) stop("'groups' must be a factor.")
        {
            if (missing(gname)) gname <- deparse(substitute(groups))
            levels <- levels(groups)
            pvalues <- matrix(0, length(levels), 2)
            rownames(pvalues) <- levels
            cat("\n --------")
            for (level in levels){
                result <- do.call(test, list(x=x[groups == level], ...))
                result$data.name <- vname
                pvalues[level, 1] <- result$p.value
                cat("\n", gname, "=", level, "\n")
                print(result)
                cat(" --------")
            }
            pvalues[, 2] <- p.adjust(pvalues[, 1])
            pvals <- matrix("", length(levels), 2)
            colnames(pvals) <- c("unadjusted", "adjusted")
            rownames(pvals) <- levels
            pvals[, 1] <- format.pval(pvalues[, 1])
            pvals[, 2] <- format.pval(pvalues[, 2])
            cat("\n\n p-values adjusted by the Holm method:\n")
            print(pvals, quote=FALSE)
            return(invisible(NULL))
        }
    }
}
