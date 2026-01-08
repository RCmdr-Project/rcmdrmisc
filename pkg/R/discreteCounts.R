#' Frequency Distributions of Numeric Variables
#' 
#' @name discreteCounts
#'
#' @keywords univar
#' 
#' @details
#' Computes the frequency and percentage distribution of a descrete numeric variable or the distributions of the variables in a numeric matrix or data frame.
#'
#' @param x a discrete numeric vector, matrix, or data frame.
#' @param round.percents number of decimal places to round percentages; default is \code{2}.
#' @param name name for the variable; only used for vector argument \code{x}.
#' @param max.values maximum number of unique values (default is the smallest of twice the square root of the number of elements in \code{x}, 10 times the log10 of the number of elements, and \code{100}); if exceeded, an error is reported.
#'
#' @return For a numeric vector, invisibly returns the table of counts. For a matrix or data frame, invisibly returns \code{NULL}
#'
#' @author John Fox
#'
#' @seealso \code{\link{binnedCounts}}
#'
#' @examples
#' set.seed(12345) # for reproducibility
#' discreteCounts(data.frame(x=rpois(51, 2), y=rpois(51, 10)))
#'
#' @export
discreteCounts <- function(x, round.percents=2, name=deparse(substitute(x)), 
                           max.values=min(round(2*sqrt(length(x))), round(10*log10(length(x))), 100)){
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.matrix(x)) {
        names <- colnames(x)
        for (j in 1:ncol(x)){
            discreteCounts(x[, j], round.percents=round.percents, name=names[j], max.values=max.values)
            cat("\n")
        }
        return(invisible(NULL))
    }
    Count <- table(x)
    if ((nv <- length(Count)) > max.values) stop("number of unique values of ", name, ", ", nv, ", exceeds maximum, ", max.values)
    tot <- sum(Count)
    Percent <- round(100*Count/tot, round.percents)
    tot.percent <- round(sum(Percent), round.percents)
    table <- cbind(Count, Percent)
    table <- rbind(table, c(tot, tot.percent))
    rownames(table) <- c(names(Count), "Total")
    cat("Distribution of", name, "\n")
    print(table)
    return(invisible(Count))
}
