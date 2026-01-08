#' Partial Correlations
#' 
#' @name partial.cor
#'
#' @keywords misc
#'
#' @details
#' Computes a matrix of partial correlations between each pair of variables controlling for the others.
#'
#' @param X data matrix.
#' @param tests show two-sided p-value and p-value adjusted for multiple testing by Holm's method for each partial correlation.
#' @param use observations to use to compute partial correlations, default is \code{"complete.obs"}.
#'
#' @return
#' Returns the matrix of partial correlations, optionally with adjusted and unadjusted p-values.
#'
#' @author John Fox
#'
#' @seealso \code{\link[stats]{cor}}
#'
#' @examples
#' data(DavisThin, package="car")
#' partial.cor(DavisThin)
#' partial.cor(DavisThin, tests=TRUE)
#'
#' @export
partial.cor <- function(X, tests=FALSE, use=c("complete.obs", "pairwise.complete.obs")){
    countValid <- function(X){
        X <- !is.na(X)
        t(X) %*% X
    }
    use <- match.arg(use)
    if (use == "complete.obs"){
        X <- na.omit(X)
        n <- nrow(X)
    }
    else n <- countValid(X) 
    R <- cor(X, use=use)
    RI <- solve(R)
    D <- 1/sqrt(diag(RI))
    R <- - RI * (D %o% D)
    diag(R) <- 0
    rownames(R) <- colnames(R) <- colnames(X)
    result <- list(R=R, n=n, P=NULL, P.unadj=NULL)
    if (tests){
        opt <- options(scipen=5)
        on.exit(options(opt))
        df <- n - ncol(X)
        f <- (R^2)*df/(1 - R^2)
        P <- P.unadj <- pf(f, 1, df, lower.tail=FALSE)
        p <- P[lower.tri(P)]
        adj.p <- p.adjust(p, method="holm")
        P[lower.tri(P)] <- adj.p
        P[upper.tri(P)] <- 0
        P <- P + t(P)
        P <- ifelse(P < 1e-04, 0, P)
        P <- format(round(P, 4))
        diag(P) <- ""
        P[c(grep("0.0000", P), grep("^ 0$", P))] <- "<.0001"
        P.unadj <- ifelse(P.unadj < 1e-04, 0, P.unadj)
        P.unadj <- format(round(P.unadj, 4))
        diag(P.unadj) <- ""
        P.unadj[c(grep("0.0000", P.unadj), grep("^ 0$", P.unadj))] <- "<.0001"
        result$P <- P
        result$P.unadj <- P.unadj
    }
    class(result) <- "partial.cor"
    result
}

#' @rdname partial.cor
#' @param x data matrix.
#' @param digits minimal number of _significant_ digits, see \code{\link[base]{print.default}}.
#' @param \dots arguments to pass down from the print method.
#' @export
print.partial.cor <- function(x, digits=max(3, getOption("digits") - 2), ...){
    cat("\n Partial correlations:\n")
    print(round(x$R, digits, ...))
    cat("\n Number of observations: ")
    n <- x$n
    if (all(n[1] == n)) cat(n[1], "\n")
    else{
        cat("\n")
        print(n)
    }
    if (!is.null(x$P)){
        cat("\n Pairwise two-sided p-values:\n")
        print(x$P.unadj, quote=FALSE)
        cat("\n Adjusted p-values (Holm's method)\n")
        print(x$P, quote=FALSE)
    }
    x
}
