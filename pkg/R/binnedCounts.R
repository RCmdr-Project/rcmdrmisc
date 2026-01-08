#' Binned Frequency Distributions of Numeric Variables
#' 
#' @name binnedCounts
#'
#' @keywords univar
#'
#' @details
#' Bins a numeric variable, as for a histogram, and reports the count and percentage in each bin.
#' The computations are done by the \code{\link[graphics]{hist}} function, but no histogram is drawn.
#' If supplied a numeric matrix or data frame, the distribution of each column is printed.
#'
#' @param x a numeric vector, matrix, or data frame.
#' @param breaks specification of the breaks between bins, to be passed to the \code{\link[graphics]{hist}} function.
#' @param round.percents number of decimal places to round percentages; default is \code{2}.
#' @param name for the variable; only used for vector argument \code{x}.
#'
#' @return For a numeric vector, invisibly returns the vector of counts, named with the end-points of the corresponding bins. For a matrix or data frame, invisibly returns \code{NULL}
#'
#' @author John Fox
#'
#' @seealso \code{\link[graphics]{hist}}, \code{\link{discreteCounts}}
#'
#' @examples
#' with(Prestige, binnedCounts(income))
#' binnedCounts(Prestige[, 1:4])
#' 
#' @export
binnedCounts <- function(x, breaks="Sturges", round.percents=2, name=deparse(substitute(x))){
    if (is.data.frame(x)) x <- as.matrix(x)
    if (is.matrix(x)) {
        names <- colnames(x)
        for (j in 1:ncol(x)){
            binnedCounts(x[, j], breaks=breaks, name=names[j])
            cat("\n")
        }
        return(invisible(NULL))
    }
    dist <- hist(x, breaks=breaks, plot=FALSE)
    Count <- dist$counts
    breaks <- dist$breaks
    tot <- sum(Count)
    Percent <- round(100*Count/tot, round.percents)
    tot.percent <- round(sum(Percent), round.percents)
    names(Count) <- paste0(c("[", rep("(", length(breaks) - 2)), breaks[1:(length(breaks) - 1)], ", ", breaks[-1], "]")
    table <- cbind(Count, Percent)
    table <- rbind(table, c(tot, tot.percent))
    rownames(table)[nrow(table)] <- "Total"
    cat("Binned distribution of", name, "\n")
    print(table)
    return(invisible(Count))
}
