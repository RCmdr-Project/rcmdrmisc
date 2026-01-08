#' Read a SAS b7dat Data Set
#'
#' @name readSAS
#'
#' @keywords manip
#'
#' @details
#' \code{readSAS} reads a SAS ``b7dat'' data set, stored in a file of type \code{.sas7bdat}, into an R data frame; it provides a front end to the \code{\link[haven]{read_sas}} function in the \pkg{haven} package.
#'
#' @param file path to a SAS b7dat file.
#' @param rownames if \code{TRUE} (the default is \code{FALSE}), the first column in the data set contains row names (which must be unique---i.e., no duplicates).
#' @param stringsAsFactors if \code{TRUE} (the default is \code{FALSE}) then columns containing character data are converted to factors.
#'
#' @return
#' a data frame.
#'
#' @author John Fox
#'
#' @seealso \code{\link[haven]{read_sas}}
#'
#' @export
readSAS <- function(file, rownames=FALSE, stringsAsFactors=FALSE){
    Data <- as.data.frame(haven::read_sas(file))
    if (rownames){
        check <- length(unique(col1 <- Data[[1]])) == nrow(Data)
        if (!check) warning ("row names are not unique, ignored")
        else {
            rownames(Data) <- col1
            Data[[1]] <- NULL
        }
    }
    if (stringsAsFactors){
        char.cols <- sapply(Data, class) == "character"
        if (any(char.cols)){
            for (col in names(Data)[char.cols]){
                fac <- Data[, col]
                fac[fac == ""] <- NA
                Data[, col] <- as.factor(fac)
            } 
        }
    }
    Data
}
