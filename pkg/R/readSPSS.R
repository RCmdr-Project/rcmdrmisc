#' Read an SPSS Data Set
#'
#' @name readSPSS
#'
#' @keywords manip
#'
#' @details
#' \code{readSPSS} reads an SPSS data set, stored in a file of type \code{.sav} or \code{.por}, into an R data frame; it provides a front end to the \code{\link[haven]{read_spss}} function in the \pkg{haven} package and the \code{\link[foreign]{read.spss}} function in the \pkg{foreign} package.
#'
#' @param file path to an SPSS \code{.sav} or \code{.por} file.
#' @param rownames if \code{TRUE} (the default is \code{FALSE}), the first column in the data set contains row names, which should be unique.
#' @param stringsAsFactors if \code{TRUE} (the default is \code{FALSE}) then columns containing character data are converted to factors and factors are created from SPSS value labels.
#' @param tolower change variable names to lowercase, default \code{TRUE}.
#' @param use.value.labels if \code{TRUE}, the default, variables with value labels in the SPSS data set will become either factors or character variables (depending on the \code{stringsAsFactors} argument) with the value labels as their levels or values. As for \code{\link[foreign]{read.spss}}, this is only done if there are at least as many labels as values of the variable (and values without a matching label are returned as \code{NA}).
#' @param use.haven use \code{\link[haven]{read_spss}} from the \pkg{haven} package to read the file, in preference to \code{\link[foreign]{read.spss}} from the \pkg{foreign} package; the default is \code{TRUE} for a \code{.sav} file and \code{FALSE} for a \code{.por} file.
#'
#' @return a data frame.
#'
#' @author John Fox
#'
#' @seealso \code{\link[haven]{read_spss}}, \code{\link[foreign]{read.spss}}
#'
#' @export
readSPSS <- function(file, rownames=FALSE, stringsAsFactors=FALSE, tolower=TRUE, use.value.labels=TRUE, use.haven=!por){
    filename <- rev(strsplit(file, "\\.")[[1]])
    por <- "por" == if (length(filename) > 1) filename[1] else ""
    Data <- if (use.haven) as.data.frame(haven::read_spss(file))
            else foreign::read.spss(file, to.data.frame=TRUE, use.value.labels=use.value.labels)
    if (rownames){
        col1 <- gsub("^\ *", "", gsub("\ *$", "", Data[[1]]))
        check <- length(unique(col1)) == nrow(Data)
        if (!check) warning ("row names are not unique, ignored")
        else {
            rownames(Data) <- col1
            Data[[1]] <- NULL
        }
    }
    if (use.haven && use.value.labels){
        na <- as.character(NA)
        n <- nrow(Data)
        for (col in names(Data)){
            var <- Data[, col]
            if (!is.null(labs <- attr(var, "labels"))){
                if (length(labs) < length(unique(var))) next
                nms <- names(labs)
                var2 <- rep(na, n)
                for (i in seq_along(labs)){
                    var2[var == labs[i]] <- nms[i]
                }
                Data[, col] <- var2
            }
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
    num.cols <- sapply(Data, is.numeric)
    if (use.haven && any(num.cols)){
        for (col in names(Data)[num.cols]) {
            Data[, col] <- as.numeric(Data[, col])
            Data[!is.finite(Data[, col]), col] <- NA
        }
    }
    if (tolower){
        names(Data) <- tolower(names(Data))
    }
    Data
}
