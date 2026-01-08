#' Reshape Repeated-Measures Data from Long to Wide Format
#'
#' @name reshapeL2W
#'
#' @keywords manip
#'
#' @details
#' A simple front-end to the standard R \code{\link[stats]{reshape}} function. The data are assumed to be in "long" format, with several rows for each subject.
#'
#' Between-subjects variables don't vary by occasions for each subject. Variables that aren't listed explicitly in the arguments to the function are assumed to be between-subjects variables, and a warning is printed if their values aren't invariant for each subject (see the \code{ignore} argument).
#'
#' Within-subjects factors vary by occasions for each subject, and it is assumed that the within-subjects design is regular, completely crossed, and balanced, so that the same combinations of within-subjects factors are observed for each subject.
#'
#' Occasion-varying variables, as their name implies, (potentially) vary by occasions for each subject, and include one or more "response" variables, possibly along with occasion-varying covariates; these variables can be factors as well as numeric variables.
#'
#' The data are reshaped so that there is one row per subject, with columns for the between-subjects variables, and each occasion-varying variable as multiple columns representing the combinations of levels of the within-subjects factors.
#' The names of the columns for the occasion-varying variables are composed from the combinations of levels of the within-subjects factors and from the names of the occasion-varying variables.  If a subject in the long form of the data set lacks any combination of levels of within-subjects factors, he or she is excluded (with a warning) from the wide form of the data.

#' @param data a data frame in long format.
#' @param within a character vector of names of the within-subjects factors in the long form of the data; there must be at least one within-subjects factor.
#' @param id the (character) name of the variable representing the subject identifier in the long form of the data set; that is, rows with the same \code{id} belong to the same subject.
#' @param varying a character vector of names of the occasion-varying variables in the long form of the data; there must be at least one such variable, and typically there will be just one, an occasion-varying response variable.
#' @param ignore an optional character vector of names of variables in the long form of the data to exclude from the wide data set.
#'
#' @return a data frame in "wide" format, with one row for each subject, columns representing the between subjects factors, and columns for the occasion-varying variable(s) for each combination of within-subjects factors.
#'
#' @author John Fox
#'
#' @seealso \code{\link[stats]{reshape}}, \code{\link[carData]{OBrienKaiser}}, \code{\link[carData]{OBrienKaiserLong}}.
#'
#' @examples
#' OBW <- reshapeL2W(OBrienKaiserLong, within=c("phase", "hour"), id="id", varying="score")
#' brief(OBW)
#' # should be the same as OBrienKaiser in the carData package:
#' all.equal(OBrienKaiser, OBW, check.attributes=FALSE)
#'
#' @export
reshapeL2W <- function(data, within, id, varying, ignore){

    ## create wide data set
    if (missing(ignore)) ignore <- NULL
    names <- colnames(data)
    all <- c(within, id, varying, ignore)
    bad <- all[!all %in% names]
    if (length(bad) > 0) stop("variables not in the data set: ", bad)
    duplicated <- unique(all[duplicated(all)])
    if (length(duplicated) > 0) stop(paste0("the following variables appear more than once: ", paste(duplicated, collapse=", ")))
    if (!is.null(ignore)){
        remove <- which(names(data) %in% ignore )
        data <- data[, -remove]
    }
    within.factors <- data[, within, drop=FALSE]
    within.var <- apply(within.factors, 1, function(x) paste(as.character(x), collapse="."))
    data <- cbind(data, within.var)
    occasions <- paste(within, collapse=".")
    names(data)[length(data)] <- occasions
    occasions.1 <- paste0(occasions, ".1")
    result <- reshape(data, timevar=occasions, idvar=id, v.names=varying,  direction="wide", 
                      drop=if (length(within) > 1) within)
    
    ## create names for the repeated-measures columns
    
    rownames(result) <- result[, id]
    result <- result[, - which(colnames(result) %in% c(id, occasions.1))]
    
    ## within.levels <- lapply(within.factors[, rev(within), drop=FALSE], levels)
    ## grid <- expand.grid(within.levels)
    ## repeated.names <- apply(grid, 1, function(x) paste(rev(x), collapse="."))
    
    all.repeated.cols <- NULL
    for (var in varying){
        repeated.cols <- grep(paste0("^", var, "."), names(result))
        ## nms <- if (length(varying) > 1) paste0(repeated.names, ".", var) else repeated.names
        ## names(result)[repeated.cols] <- make.names(nms)
        all.repeated.cols <- c(all.repeated.cols, repeated.cols)
    }
    
    ## remove cases with incomplete repeated measures
    bad <- apply(result[, all.repeated.cols], 1, function(x) anyNA(x))
    n.bad <- sum(bad)
    if (n.bad > 0){
        warning(n.bad, " ", if (n.bad == 1) "case" else "cases",  
                " removed due to missing repeated measures")
        result <- result[!bad, ]
    }
    
    result
}
