#' Dot Plots
#' 
#' @name Dotplot
#'
#' @details
#' Dot plot of numeric variable, either using raw values or binned, optionally classified by a factor. Dot plots are useful for visualizing the distribution of a numeric variable in a small data set.
#'
#' If the \code{by} argument is specified, then one dot plot is produced for each level of \code{by}; these are arranged vertically and all use the same scale for \code{x}.
#' An attempt is made to adjust the size of the dots to the space available without making them too big.
#'
#' @keywords hplot
#'
#' @param x a numeric variable.
#' @param  by optionally a factor (or character or logical variable) by which to classify \code{x}.
#' @param bin if \code{TRUE} (the default is \code{FALSE}), the values of \code{x} are binned, as in a histogram, prior to plotting.
#' @param breaks breaks for the bins, in a form acceptable to the \code{\link[graphics]{hist}} function; the default is \code{"Sturges"}.
#' @param xlim optional 2-element numeric vector giving limits of the horizontal axis.
#' @param xlab optional character string to label horizontal axis.
#'
#' @return Returns \code{NULL} invisibly.
#'
#' @author John Fox
#'
#' @seealso \code{\link[graphics]{hist}}
#'
#' @examples
#' data(Duncan)
#' with(Duncan, Dotplot(education))
#' with(Duncan, Dotplot(education, bin=TRUE))
#' with(Duncan, Dotplot(education, by=type))
#' with(Duncan, Dotplot(education, by=type, bin=TRUE))
#'
#' @export
Dotplot <- function(x, by, bin=FALSE, breaks, xlim, xlab=deparse(substitute(x))){
    dotplot <- function(x, by, bin=FALSE, breaks, xlim,
                        xlab=deparse(substitute(x)),
                        main="", correction=1/3, correction.char=1, y.max){
        bylab <- if (!missing(by)) deparse(substitute(by))
        if (bin) hist <- hist(x, breaks=breaks, plot=FALSE)
        if (missing(by)){
            y <- if (bin) hist$counts else table(x)
            x <- if (bin) hist$mids else sort(unique(x))
            plot(range(x), 0:1, type="n", xlab=xlab, ylab="", main=main, axes=FALSE,
                 xlim=xlim)
            y.limits <- par("usr")[3:4]
            char.height <- correction.char*par("cxy")[2]
            axis(1, pos=0)
            if (missing(y.max)) y.max <- max(y)
            abline(h=0)
            cex <- min(((y.limits[2] - y.limits[1])/char.height)/
                       y.max, 2)
            for (i in 1:length(y)){
                if (y[i] == 0) next
                points(rep(x[i], y[i]), cex*correction*char.height*seq(1, y[i]), pch=16, cex=cex,
                       xpd=TRUE)
            }
            return(invisible(NULL))
        }
        else{
            if (missing(xlim)) xlim <- range(x)
            levels <- levels(by)
            n.groups <- length(levels)
            save.par <- par(mfrow=c(n.groups, 1))
            on.exit(par(save.par))
            if (bin){
                for(level in levels){
                                        # compute histograms by level to find maximum count
                    max.count <- 0
                    hist.level <- hist(x[by == level], breaks=hist$breaks, plot=FALSE)
                    max.count <- max(max.count, hist.level$counts)
                }
                for (level in levels){
                    mainlabel <- paste(bylab, "=", level)
                    dotplot(x[by == level], xlab=xlab, main=mainlabel, 
                            bin=TRUE, breaks=hist$breaks, xlim=xlim, correction=1/2, 
                            correction.char=0.5, y.max=max.count)
                }
            }
            else {
                y <- table(x, by)
                for (level in levels){
                    mainlabel <- paste(bylab, "=", level)
                    dotplot(x[by == level], xlab=xlab, main=mainlabel,
                            xlim=xlim, correction=1/2, correction.char=0.5, y.max=max(y))
                }
            }
        }
    }
    if (!is.numeric(x)) stop("x must be a numeric variable")
    if (!missing(by) && !is.factor(by)) {
        bylab <- deparse(substitute(by))
        if (!(is.character(by) || is.logical(by))) stop("by must be a factor, character, or logical")
        by <- as.factor(by)
    }
    force(xlab)
    if (missing(by)){
        x <- na.omit(x)
    }
    else{
        keep <- complete.cases(x, by)
        x <- x[keep]
        by <- by[keep]
    }
    if (missing(xlim)) xlim <- range(x)
    force(xlab)
    if (missing(breaks)) breaks <- "Sturges"
    if (missing(by)) dotplot(x=x, bin=bin, breaks=breaks, xlim=xlim, xlab=xlab)
    else dotplot(x=x, by=by, bin=bin, breaks=breaks, xlim=xlim, xlab=xlab)
}
