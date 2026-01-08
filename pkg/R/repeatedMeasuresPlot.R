#' Plot Means for Repeated-Measures ANOVA Designs
#' 
#' @name repeatedMeasuresPlot
#'
#' @keywords hplot
#'
#' @details
#' Creates a means plot for a repeated-measures ANOVA design with one or two within-subjects factor and zero or more between-subjects factors, for data in "wide" format.
#'
#' @param data a data frame in wide format.
#' @param within a character vector with the names of the data columns containing the repeated measures.
#' @param within.names a character vector with one or two elements, of names of the within-subjects factor(s).
#' @param within.levels a named list whose elements are character vectors of level names for the within-subjects factors, with names corresponding to the names of the within-subjects factors; the product of the numbers of levels should be equal to the number of repeated-measures columns in \code{within}.
#' @param between.names a column vector of names of the between-subjects factors (if any).
#' @param response.name optional quoted name for the response variable, defaults to \code{"score"}.
#' @param trace optional quoted name of the (either within- or between-subjects) factor to define profiles of means in each panel of the graph; the default is the within-subjects factor with the smaller number of levels, if there are two, or not used if there is one.
#' @param xvar optional quoted name of the factor to define the horizontal axis of each panel; the default is the within-subjects factor with the larger number of levels.
#' @param pch vector of symbol numbers to use for the profiles of means (i.e., levels of the \code{trace} factor); for the meaning of the defaults, see \code{\link[graphics]{points}} and \code{\link[graphics]{par}}.
#' @param lty vector  of line-type numbers to use for the profiles of means.
#' @param col vector of colors for the profiles of means; the default is given by \code{palette()}, starting at the second color.
#' @param plot.means if \code{TRUE} (the default), draw a plot of means by the factors.
#' @param print.tables if \code{TRUE} (the default is \code{FALSE}), print tables of means and standard deviations of the response by the factors.
#'
#' @return
#' A \code{"trellis"} object, which normally is just "printed" (i.e., plotted).
#'
#' @author John Fox
#'
#' @seealso \code{\link[car]{Anova}}, \code{\link[carData]{OBrienKaiser}}
#'
#' @examples
#' repeatedMeasuresPlot(
#'    data=OBrienKaiser,
#'    within=c("pre.1", "pre.2", "pre.3", "pre.4", "pre.5",
#'             "post.1", "post.2", "post.3", "post.4", "post.5",
#'             "fup.1", "fup.2", "fup.3", "fup.4", "fup.5"),
#'    within.names=c("phase", "hour"),
#'    within.levels=list(phase=c("pre", "post", "fup"),
#'    hour = c("1", "2", "3", "4", "5")),
#'    between.names=c("gender", "treatment"),
#'    response.name="improvement",
#'    print.tables=TRUE
#' )
#'
#' repeatedMeasuresPlot(data=OBrienKaiser,
#'    within=c("pre.1", "pre.2", "pre.3", "pre.4", "pre.5",
#'             "post.1", "post.2", "post.3", "post.4", "post.5",
#'             "fup.1", "fup.2", "fup.3", "fup.4", "fup.5"),
#'    within.names=c("phase", "hour"),
#'    within.levels=list(phase=c("pre", "post", "fup"), hour = c("1", "2", "3", "4", "5")),
#'    between.names=c("gender", "treatment"),
#'    trace="gender") # note that gender is between subjects
#'
#' repeatedMeasuresPlot(
#'    data=OBrienKaiser,
#'    within=c("fup.1", "fup.2", "fup.3", "fup.4", "fup.5"),
#'    within.names="hour",
#'    within.levels=list(hour = c("1", "2", "3", "4", "5")),
#'    between.names=c("treatment", "gender"),
#'    response.name="improvement")
#'
#' @export
repeatedMeasuresPlot <- function(data, within, within.names, within.levels, between.names=NULL,
                                 response.name="score", trace, xvar, pch=15:25, lty=1:6,
                                 col=palette()[-1], plot.means=TRUE,
                                 print.tables=FALSE){
    
    if (!(plot.means || print.tables)) stop("nothing to do (neither print tables nor plot means)!")
    
    if (missing(trace)) trace <- NA
    if (missing(xvar)) xvar <- NA

    reshapeW2L <- function(data){
        timevar <- paste(within.names, collapse=".")
        long <- reshape(data, varying=within, v.names=response.name, 
                        timevar=timevar, 
                        direction="long")
        n.levels <- sapply(within.levels, length)
        n.within <- length(within.names)
        if (n.within > 2 || n.within < 1) stop("there must be 1 or 2 within factors")
        if (prod(n.levels) != length(within)){
            stop("the number of repeated measures, ", length(within), 
                 ", is not equal to the product of the numbers of levels of the within factors, ",
                 prod(n.levels))
        }
        if (length(within.names) != length(within.levels)){
            stop("the number of within factors, ", length(within.names),
                 ", is not equal to the number of sets of within-factor levels, ", 
                 length(within.levels))
        }
        if (n.within == 2){
            long[[within.names[1]]] <- factor(within.levels[[within.names[1]]][1 + ((long[[timevar]] - 1) %/% n.levels[2])],
                                              levels=within.levels[[within.names[1]]])
            long[[within.names[2]]] <- factor(within.levels[[within.names[2]]][1 + ((long[[timevar]] - 1) %% n.levels[2])],
                                              levels=within.levels[[within.names[2]]])
        } else{
            long[[within.names]] <- factor(within.levels[[1]][long[[timevar]]], 
                                           levels=within.levels[[1]])
        }
        long
    }
    
    computeMeans <- function(data){
        formula <- paste(response.name, " ~", paste(c(within.names, between.names), collapse="+"))
        meanTable <- Tapply(formula, mean, data=data)
        sdTable <- Tapply(formula, sd, data=data)
        means <- meanTable
        if(length(dim(means)) > 1){
            means <- as.data.frame(ftable(means))
            names(means)[ncol(means)] <- response.name
        } else {
            means <- data.frame(factor(names(means), levels=levels(data[, within.names])), means)
            names(means) <- c(within.names, response.name)
        }
        list(means=means, meanTable=meanTable, sdTable=sdTable)
    }
    
    rmPlot <- function(data) {
        n.levels <-sapply(data[,-ncol(data), drop = FALSE], function(x)
            length(levels(x)))
        n.factors <- length(n.levels)
        fnames <- names(data)[-ncol(data), drop = FALSE]
        if (is.na(trace)) {
            wnames <- if (!is.na(xvar)) within.names[!(within.names == xvar)] else within.names
            trace <- if (length(wnames) > 0) wnames[which.min(n.levels[wnames])] else NULL
        }
        if (is.na(xvar)) {
            wnames <- if (!is.na(trace)) within.names[!(within.names == trace)] else within.names
            xvar <- wnames[which.max(n.levels[wnames])]
        } 
        if (length(within.names) == 1 && length(xvar) == 0){
            xvar <- within.names
            trace <- NULL
        }
        if (!is.null(trace) && trace == xvar) trace <- NULL
        form <- paste(response.name,
                      " ~",
                      xvar,
                      if (n.factors > 1 + !is.null(trace))
                          "|",
                      paste(setdiff(fnames, c(trace, xvar)), collapse = "+"))
        tr.levels <- n.levels[trace]
        if (!is.null(trace)) {
            xyplot(
                as.formula(form),
                groups = if (!is.null(trace))
                             data[[trace]]
                         else
                             1,
                type = "b",
                lty = lty[1:tr.levels],
                pch = pch[1:tr.levels],
                col = col[1:tr.levels],
                cex = 1.25,
                strip = function(...)
                    strip.default(strip.names = c(TRUE, TRUE), ...),
                data = data,
                ylab = paste("mean", response.name),
                key = list(
                    title = trace,
                    cex.title = 1,
                    text = list(levels(data[[trace]])),
                    lines = list(lty = lty[1:tr.levels], col = col[1:tr.levels]),
                    points = list(
                        pch = pch[1:tr.levels],
                        col = col[1:tr.levels],
                        cex = 1.25
                    )
                )
            )
        } else {
            xyplot(
                as.formula(form),
                type = "b",
                lty = lty[1],
                pch = pch[1],
                col = col[1],
                cex = 1.25,
                strip = function(...)
                    strip.default(strip.names = c(TRUE, TRUE), ...),
                data = data,
                ylab = paste("mean", response.name)
            )
        }
    }
    
    Long <- reshapeW2L(data)
    Means <- computeMeans(Long)
    if (print.tables){
        cat("\n Means of", response.name, "\n")
        if (length(dim(Means$meanTable)) > 1) print(ftable(Means$meanTable))
        else print(Means$meanTable)
        cat("\n\n Standard deviations of", response.name, "\n")
        if (length(dim(Means$sdTable)) > 1) print(ftable(Means$sdTable))
        else print(Means$sdTable)
    }
    if (plot.means) rmPlot(Means$means) else invisible(NULL)
}
