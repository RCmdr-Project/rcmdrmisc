#' Read a Stata Data Set
#' 
#' @name readStata
#'
#' @details
#' \code{readStata} reads a Stata data set, stored in a file of type \code{.dta}, into an R data frame; it provides a front end to the \code{\link[readstata13]{read.dta13}} function in the \pkg{readstata13} package.
#'
#' @keywords manip
#'
#' @param file path to a Stata \code{.dta} file.
#' @param rownames if \code{TRUE} (the default is \code{FALSE}), the first column in the data set contains row names, which should be unique.
#' @param stringsAsFactors if \code{TRUE} (the default is \code{FALSE}) then columns containing character data are converted to factors and factors are created from Stata value labels.
#' @param convert.dates if \code{TRUE} (the default) then Stata dates are converted to R dates.
#'
#' @return
#' a data frame.
#'
#' @author John Fox
#'
#' @seealso
#' \code{\link[readstata13]{read.dta13}}
#'
#' @export
readStata <- function(file, rownames=FALSE, stringsAsFactors=FALSE, convert.dates=TRUE){
    Data <- readstata13::read.dta13(file, convert.factors=stringsAsFactors, convert.dates=convert.dates)
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
