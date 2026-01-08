#' Reshape Repeated-Measures Data from Wide to Long Format
#' 
#' @name reshapeW2L
#'
#' @keywords manip
#'
#' @details
#' The data are assumed to be in "wide" format, with a single row for each subject, and different columns for values of one or more repeated-measures variables classified by one or more within-subjects factors.
#'
#' Between-subjects variables don't vary by occasions for each subject. Variables that aren't listed explicitly in the arguments to the function are assumed to be between-subjects variables.
#' The values of these variables are duplicated in each row pertaining to a given subject.
#'
#' Within-subjects factors vary by occasions for each subject, and it is assumed that the within-subjects design is regular, completely crossed, and balanced, so that the same combinations of within-subjects factors are observed for each subject.
#' There are typically one or two within-subject factors.
#'
#' Occasion-varying variables, as their name implies, (potentially) vary by occasions for each subject, and include one or more "response" variables, possibly along with occasion-varying covariates; these variables can be factors as well as numeric variables.
#' Each occasion-varying variable is encoded in multiple columns of the wide form of the data and in a single column in the long form. There is typically one occasion-varying variable, a response variable.
#'
#' There is one value of each occasion-varying variable for each combination of levels of the within-subjects factors.
#' Thus, the number of variables in the wide data for each occasion-varying variable must be equal to the product of levels of the within-subjects factors, with the levels of the within-subjects factors varying most quickly from right to left in the \code{within} argument.
#'
#' @param data wide version of data set.
#' @param within a character vector of names for the crossed within-subjects factors to be created in the long form of the data.
#' @param levels a named list of character vectors, each element giving the names of the levels for a within-subjects factor; the names of the list elements are the names of the within-subjects factor, given in the \code{within} argument.
#' @param varying a named list of the names of variables in the wide data set specifying the occasion-varying variables to be created in the long data set; each element in the list is named for an occasion-varying variable and is a character vector of column names in the wide data for that occasion-varying variable.
#' @param ignore a character vector of names of variables in the wide data to be dropped in the long form of the data.
#' @param id the (character) name of the subject ID variable to be created in the long form of the data, default \code{"id"}.
#'
#' @return a data frame in "long" format, with multiple rows for each subject (equal to the number of combinations of levels of the within-subject factors) and one column for each between-subjects and occasion-varying variable.
#'
#' @author John Fox
#'
#' @seealso \code{\link{reshapeL2W}}, \code{\link[stats]{reshape}}, \code{\link[carData]{OBrienKaiser}}, \code{\link[carData]{OBrienKaiserLong}}.
#'
#' @examples
#' OBrienKaiserL <- reshapeW2L(OBrienKaiser, within=c("phase", "hour"),
#'    levels=list(phase=c("pre", "post", "fup"), hour=1:5),
#'    varying=list(score=c("pre.1", "pre.2", "pre.3", "pre.4", "pre.5",
#'                         "post.1", "post.2", "post.3", "post.4", "post.5",
#'                         "fup.1", "fup.2", "fup.3", "fup.4", "fup.5")))
#' brief(OBrienKaiserL, c(15, 15))
#' m1 <- Tapply(score ~ phase + hour + treatment + gender, mean, data=OBrienKaiserL)
#' m2 <- Tapply(score ~ phase + hour + treatment + gender, mean, data=OBrienKaiserLong)
#' all.equal(m1, m2) # should be equal
#'
#' OBrienKaiserL2 <- reshapeW2L(OBrienKaiser, within="phase",
#'    levels=list(phase=c("pre", "post", "fup")),
#'    ignore=c("pre.2", "pre.3", "pre.4", "pre.5",
#'             "post.2", "post.3", "post.4", "post.5",
#'             "fup.2", "fup.3", "fup.4", "fup.5"),
#'    varying=list(score=c("pre.1", "post.1", "fup.1")))
#' brief(OBrienKaiserL2, c(6, 6))
#' m1 <- Tapply(score ~ phase + treatment + gender, mean, data=OBrienKaiserL2)
#' m2 <- Tapply(score ~ phase + treatment + gender, mean, data=subset(OBrienKaiserLong, hour==1))
#' all.equal(m1, m2) # should be equal
#'
#' @export
reshapeW2L <- function(data, within, levels, varying, ignore, id="id"){
    
    ## process variable names
    if (missing(ignore)) ignore <- NULL
    all <- colnames(data)
    use <- setdiff(all, ignore)
    all.varying <- unlist(varying)
    between <- setdiff(use, all.varying)
    levs <- expand.grid(rev(levels))
    
    ## create empty output long data set
    m <- nrow(levs)
    n <- nrow(data) * m
    out <- data.frame(id=character(0), stringsAsFactors=FALSE)
    names(out)[1] <- id
    for (bet in between){
        b <- data[[bet]]
        out[[bet]] <- if (is.factor(b)) factor(NULL, levels=levels(b)) else vector(0, mode=mode(b))
    }  
    for (win in within){
        out[[win]] <- factor(NULL, levels[[win]])
    }
    for (var in names(varying)){
        v <- data[[varying[[var]][1]]]
        out[[var]] <- if (is.factor(v)) factor(NULL, levels=levels(v)) else vector(0, mode=mode(v))
    }
    out[1:n, ] <- NA
    
    ## fill output data set by cases in the wide data set
    for (i in 1:nrow(data)){
        j <- ((i - 1)*m + 1):(i*m)
        out[j, id] <- as.character(i)
        out[j, between] <- data[i, between]
        out[j, rev(within)] <- levs
        for (var in names(varying)){
            out[j, var] <- unlist(data[i, varying[[var]]])
        }
    }
    
    ## create row names
    rownames(out) <- paste0(out[[id]], ".", 1:m)
    
    out
}
