#' Row, Column, and Total Percentage Tables
#' 
#' @name colPercents
#'
#' @aliases colPercents rowPercents totPercents
#' 
#' @keywords misc
#'
#' @details
#' Percentage a matrix or higher-dimensional array of frequency counts by rows, columns, or total frequency.
#'
#' @param tab a matrix or higher-dimensional array of frequency counts.
#' @param digits number of places to the right of the decimal place for percentages.
#'
#' @return Returns an array of the same size and shape as \code{tab} percentaged by rows or columns, plus rows or columns of totals and counts, or by the table total.
#'
#' @examples
#' data(Mroz) # from car package
#' cat("\n\n column percents:\n")
#' print(colPercents(xtabs(~ lfp + wc, data=Mroz)))
#' cat("\n\n row percents:\n")
#' print(rowPercents(xtabs(~ hc + lfp, data=Mroz)))
#' cat("\n\n total percents:\n")
#' print(totPercents(xtabs(~ hc + wc, data=Mroz)))
#' cat("\n\n three-way table, column percents:\n")
#' print(colPercents(xtabs(~ lfp + wc + hc, data=Mroz)))
#'
#' @author John Fox
#' 
#' @export
colPercents <- function(tab, digits=1){
    dim <- length(dim(tab))
    if (is.null(dimnames(tab))){
        dims <- dim(tab)
        dimnames(tab) <- lapply(1:dim, function(i) 1:dims[i])
    }
    sums <- apply(tab, 2:dim, sum)
    per <- apply(tab, 1, function(x) x/sums)
    dim(per) <- dim(tab)[c(2:dim,1)]
    per <- aperm(per, c(dim, 1:(dim-1)))
    dimnames(per) <- dimnames(tab)
    per <- round(100*per, digits)
    result <- abind(per, Total=apply(per, 2:dim, sum), Count=sums, along=1)
    names(dimnames(result)) <- names(dimnames(tab))
    result
}

#' @rdname colPercents
#' @export
rowPercents <- function(tab, digits=1){
    dim <- length(dim(tab))
    if (dim == 2) return(t(colPercents(t(tab), digits=digits)))
    tab <- aperm(tab, c(2,1,3:dim))
    aperm(colPercents(tab, digits=digits), c(2,1,3:dim))
}

#' @rdname colPercents
#' @export
totPercents <- function(tab, digits=1){
    dim <- length(dim(tab))
    if (is.null(dimnames(tab))){
        dims <- dim(tab)
        dimnames(tab) <- lapply(1:dim, function(i) 1:dims[i])
    }
    tab <- 100*tab/sum(tab)
    tab <- cbind(tab, rowSums(tab))
    tab <- rbind(tab, colSums(tab))
    rownames(tab)[nrow(tab)] <- "Total"
    colnames(tab)[ncol(tab)] <- "Total"
    round(tab, digits=digits)
}
