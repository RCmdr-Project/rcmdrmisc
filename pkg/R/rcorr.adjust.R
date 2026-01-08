#' Compute Pearson or Spearman Correlations with p-Values
#'
#' @name rcorr.adjust
#'
#' @aliases rcorr.adjust print.rcorr.adjust
#'
#' @keywords htest
#'
#' @details
#' This function uses the \code{\link[Hmisc]{rcorr}} function in the \pkg{Hmisc} package to compute matrices of Pearson or Spearman correlations along with the pairwise p-values among the correlations.
#' The p-values are corrected for multiple inference using Holm's method (see \code{\link[stats]{p.adjust}}).
#' Observations are filtered for missing data, and only complete observations are used.
#'
#' @param x a numeric matrix or data frame, or an object of class \code{"rcorr.adjust"} to be printed.
#' @param type \code{"pearson"} or \code{"spearman"}, depending upon the type of correlations desired; the default is \code{"pearson"}.
#' @param use how to handle missing data: \code{"complete.obs"}, the default, use only complete cases; \code{"pairwise.complete.obs"}, use all cases with valid data for each pair.
#' @param \dots not used.
#'
#' @return
#' Returns an object of class \code{"rcorr.adjust"}, which is normally just printed.
#'
#' @author John Fox, adapting code from Robert A. Muenchen.
#'
#' @seealso \code{\link[Hmisc]{rcorr}}, \code{\link[stats]{p.adjust}}.
#'
#' @examples
#' data(Mroz)
#' print(rcorr.adjust(Mroz[,c("k5", "k618", "age", "lwg", "inc")]))
#' print(rcorr.adjust(Mroz[,c("k5", "k618", "age", "lwg", "inc")], type="spearman"))
#'
## the following function is adapted from a suggestion by Robert Muenchen
## uses rcorr in the Hmisc package
#' @export
rcorr.adjust <- function (x, type = c("pearson", "spearman"), 
            use = c("complete.obs", "pairwise.complete.obs")) {
    opt <- options(scipen = 5)
    on.exit(options(opt))
    type <- match.arg(type)
    use <- match.arg(use)
    x <- if (use == "complete.obs") 
      as.matrix(na.omit(x))
    else as.matrix(x)
    R <- rcorr(x, type = type)
    P <- P.unadj <- R$P
    p <- P[lower.tri(P)]
    adj.p <- p.adjust(p, method = "holm")
    P[lower.tri(P)] <- adj.p
    P[upper.tri(P)] <- 0
    P <- P + t(P)
    P <- ifelse(P < 1e-04, 0, P)
    P <- format(round(P, 4))
    diag(P) <- ""
    P[c(grep("0.0000", P), grep("^ 0$", P))] <- "<.0001"
    P[grep("0.000$", P)] <- "<.001"
    P.unadj <- ifelse(P.unadj < 1e-04, 0, P.unadj)
    P.unadj <- format(round(P.unadj, 4))
    diag(P.unadj) <- ""
    P.unadj[c(grep("0.0000$", P.unadj), grep("^ 0$", P.unadj))] <- "<.0001"
    P.unadj[grep("0.000$", P.unadj)] <- "<.001"
    result <- list(R = R, P = P, P.unadj = P.unadj, type = type)
    class(result) <- "rcorr.adjust"
    result
}


#' @rdname rcorr.adjust
#' @export
print.rcorr.adjust <- function(x, ...){
    cat("\n", if (x$type == "pearson") "Pearson" else "Spearman", "correlations:\n")
    print(round(x$R$r, 4))
    cat("\n Number of observations: ")
    n <- x$R$n
    if (all(n[1] == n)) cat(n[1], "\n")
    else{
        cat("\n")
        print(n)
    }
    cat("\n Pairwise two-sided p-values:\n")
    print(x$P.unadj, quote=FALSE)
    cat("\n Adjusted p-values (Holm's method)\n")
    print(x$P, quote=FALSE)
}
