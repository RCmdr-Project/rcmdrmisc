#' Reliability of a Composite Scale
#'
#' @name reliability
#'
#' @aliases reliability print.reliability
#' 
#' @keywords misc
#'
#' @details
#' Calculates Cronbach's alpha and standardized alpha (lower bounds on reliability) for a composite (summated-rating) scale.
#' Standardized alpha is for the sum of the standardized items.
#' In addition, the function calculates alpha and standardized alpha for the scale with each item deleted in turn, and computes the correlation between each item and the sum of the other items.
#'
#' @param S the covariance matrix of the items; normally, there should be at least 3 items and certainly no fewer than 2.
#' @param x reliability object to be printed.
#' @param digits number of decimal places.
#' @param \dots not used: for compatibility with the print generic."
#'
#' @return
#' an object of class reliability, which normally would be printed.
#'
#' @author John Fox
#'
#' @examples
#' data(DavisThin)
#' reliability(cov(DavisThin))
#'
#' @references
#' N. Cliff (1986) Psychological testing theory. Pp. 343--349 in S. Kotz and N. Johnson, eds., \emph{Encyclopedia of Statistical Sciences, Vol. 7}. Wiley.
#'
#' @seealso \code{\link[stats]{cov}}
#'
#' @export
reliability <- function(S){
    reliab <- function(S, R){
        k <- dim(S)[1]
        ones <- rep(1, k)
        v <- as.vector(ones %*% S %*% ones)
        alpha <- (k/(k - 1)) * (1 - (1/v)*sum(diag(S)))
        rbar <- mean(R[lower.tri(R)])
        std.alpha <- k*rbar/(1 + (k - 1)*rbar)
        c(alpha=alpha, std.alpha=std.alpha)
    }
    result <- list()
    if ((!is.numeric(S)) || !is.matrix(S) || (nrow(S) != ncol(S))
        || any(abs(S - t(S)) > max(abs(S))*1e-10) || nrow(S) < 2)
        stop("argument must be a square, symmetric, numeric covariance matrix")
    k <- dim(S)[1]
    s <- sqrt(diag(S))
    R <- S/(s %o% s)
    rel <- reliab(S, R)
    result$alpha <- rel[1]
    result$st.alpha <- rel[2]
    if (k < 3) {
        warning("there are fewer than 3 items in the scale")
        return(invisible(NULL))
    }
    rel <- matrix(0, k, 3)
    for (i in 1:k) {
        rel[i, c(1,2)] <- reliab(S[-i, -i], R[-i, -i])
        a <- rep(0, k)
        b <- rep(1, k)
        a[i] <- 1
        b[i] <- 0
        cov <- a %*% S %*% b
        var <- b %*% S %*% b
        rel[i, 3] <- cov/(sqrt(var * S[i,i]))
    }
    rownames(rel) <- rownames(S)
    colnames(rel) <- c("Alpha", "Std.Alpha", "r(item, total)")
    result$rel.matrix <- rel
    class(result) <- "reliability"
    result
}

#' @name reliability
#' @export
print.reliability <- function(x, digits=4, ...){
    cat(paste("Alpha reliability = ", round(x$alpha, digits), "\n"))
    cat(paste("Standardized alpha = ", round(x$st.alpha, digits), "\n"))
    cat("\nReliability deleting each item in turn:\n")
    print(round(x$rel.matrix, digits))
    invisible(x)
}
