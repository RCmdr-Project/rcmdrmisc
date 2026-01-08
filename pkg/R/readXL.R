#' Read an Excel File
#' 
#' @name readXL
#'
#' @keywords manip
#'
#' @details
#' \code{readXL} reads an Excel file, either of type \code{.xls} or \code{.xlsx} into an R data frame; it provides a front end to the \code{\link[readxl]{read_excel}} function in the \pkg{readxl} package.
#' \code{\link[readxl]{excel_sheets}} is re-exported from the \pkg{readxl} package and reports the names of spreadsheets in an Excel file.
#'
#' @param file name of an Excel file including its path.
#' @param rownames if \code{TRUE} (the default is \code{FALSE}), the first column in the spreadsheet contains row names (which must be unique---i.e., no duplicates).
#' @param header if \code{TRUE} (the default), the first row in the spreadsheet contains column (variable) names.
#' @param na character string denoting missing data; the default is the empty string, \code{""}.
#' @param sheet number of the spreadsheet in the file containing the data to be read; the default is \code{1}.
#' @param stringsAsFactors if \code{TRUE} (the default is \code{FALSE}) then columns containing character data are converted to factors.
#'
#' @return a data frame.
#'
#' @author John Fox
#'
#' @seealso \code{\link[readxl]{read_excel}}, \code{\link[readxl]{excel_sheets}}.
#'
#' @export
readXL <- function(file, rownames=FALSE, header=TRUE, na="", sheet=1, 
                   stringsAsFactors=FALSE){
    Data <- readxl::read_excel(path=file, sheet=sheet, col_names=header, na=na)
    class(Data) <- "data.frame"
    if (rownames){
        check <- length(unique(col1 <- Data[[1]])) == nrow(Data)
        if (!check) warning ("row names are not unique, ignored")
        else {
            rownames(Data) <- col1
            Data[[1]] <- NULL
        }
    }
    colnames(Data) <- make.names(colnames(Data), unique=TRUE)
    if (stringsAsFactors){
        char <- sapply(Data, class) == "character"
        for (var in which(char)){
            Data[[var]] <- factor(Data[[var]])
        }
    }
    Data
}

#' @export
readxl::excel_sheets
