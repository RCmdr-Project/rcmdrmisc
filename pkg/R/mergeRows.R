#' Function to Merge Rows of Two Data Frames
#' 
#' @name mergeRows
#'
#' @keywords manip
#'
#' @details
#' This function merges two data frames by combining their rows.
#'
#' @param X First data frame.
#' @param Y Second data frame.
#' @param common.only If \code{TRUE}, only variables (columns) common to the two data frame are included in the merged data set; the default is \code{FALSE}.
#' @param \dots Not used.
#'
#' @return
#' A data frame containing the rows from both input data frames.
#'
#' @author John Fox
#'
#' @seealso For column merges and more complex merges, see \code{\link[base]{merge}}.
#'
#' @examples
#' data(Duncan)
#' D1 <- Duncan[1:20,]
#' D2 <- Duncan[21:45,]
#' D <- mergeRows(D1, D2)
#' print(D)
#' dim(D)
#' 
#' @export
mergeRows <- function(X, Y, common.only=FALSE, ...){
    UseMethod("mergeRows")
}

#' @rdname mergeRows
#' @export
mergeRows.data.frame <- function(X, Y, common.only=FALSE, ...){
    cols1 <- names(X)
    cols2 <- names(Y)
    if (common.only){
        common <- intersect(cols1, cols2)
        rbind(X[, common], Y[, common])
    }
    else {
        all <- union(cols1, cols2)
        miss1 <- setdiff(all, cols1)
        miss2 <- setdiff(all, cols2)
        X[, miss1] <- NA
        Y[, miss2] <- NA
        rbind(X, Y)
    }
}
