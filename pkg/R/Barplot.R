#' Bar Plots
#'
#' @name Barplot
#'
#' @keywords hplot
#' 
#' @details
#' Create bar plots for one or two factors scaled by frequency or precentages.
#' In the case of two factors, the bars can be divided (stacked) or plotted in parallel (side-by-side).
#' This function is a front end to \code{\link[graphics]{barplot}} in the \pkg{graphics} package.
#'
#' @param x a factor (or character or logical variable).
#' @param by optionally, a second factor (or character or logical variable).
#' @param scale either \code{"frequency"} (the default) or \code{"percent"}.
#' @param conditional if \code{TRUE} then percentages are computed separately for each value of \code{x} (i.e., conditional percentages of \code{by} within levels of \code{x}); if \code{FALSE} then total percentages are graphed; ignored if \code{scale="frequency"}.
#' @param style for two-factor plots, either \code{"divided"} (the default) or \code{"parallel"}.
#' @param col if \code{by} is missing, the color for the bars, defaulting to \code{"gray"}; otherwise colors for the levels of the \code{by} factor in two-factor plots, defaulting to colors provided by \code{\link[colorspace]{rainbow_hcl}} in the \pkg{colorspace} package.
#' @param xlab an optional character string providing a label for the horizontal axis.
#' @param legend.title an optional character string providing a title for the legend.
#' @param ylab an optional character string providing a label for the vertical axis.
#' @param main an optional main title for the plot.
#' @param legend.pos position of the legend, in a form acceptable to the \code{\link[graphics]{legend}} function; the default, \code{"above"}, puts the legend above the plot.
#' @param label.bars if \code{TRUE} (the default is \code{FALSE}) show values of frequencies or percents in the bars.
#' @param ... arguments to be passed to the \code{\link[graphics]{barplot}} function.
#'
#' @return Invisibly returns the horizontal coordinates of the centers of the bars.
#'
#' @author John Fox
#'
#' @seealso \code{\link[graphics]{barplot}}, \code{\link[graphics]{legend}}, \code{\link[colorspace]{rainbow_hcl}}
#'
#' @examples
#' with(Mroz, Barplot(wc))
#' with(Mroz, Barplot(wc, col="lightblue", label.bars=TRUE))
#' with(Mroz, Barplot(wc, by=hc))
#' with(Mroz, Barplot(wc, by=hc, scale="percent", label.bars=TRUE))
#' with(Mroz, Barplot(wc, by=hc, style="parallel", scale="percent", legend.pos="center"))
#' 
#' @export
Barplot <- function(x, by, scale=c("frequency", "percent"), 
                    conditional=TRUE,
                    style=c("divided", "parallel"),
                    col=if (missing(by)) "gray" else rainbow_hcl(length(levels(by))),
                    xlab=deparse(substitute(x)), 
                    legend.title=deparse(substitute(by)), ylab=scale, main=NULL,
                    legend.pos="above", label.bars=FALSE, ...){
    find.legend.columns <- function(n, target=min(4, n)){
        rem <- n %% target
        if (rem != 0 && rem < target/2) target <- target - 1
        target
    }
    force(xlab)
    force(legend.title)
    if (!is.factor(x)) {
        if (!(is.character(x) || is.logical(x))) stop("x must be a factor, character, or logical")
        x <- as.factor(x)
    }
    if (!missing(by) && !is.factor(by)) {
        if (!(is.character(by) || is.logical(by))) stop("by must be a factor, character, or logical")
        by <- as.factor(by)
    }
    scale <- match.arg(scale)
    style <- match.arg(style)
    if (legend.pos == "above"){
        mar <- par("mar")
        mar[3] <- mar[3] + 2
        old.mar <- par(mar=mar)
        on.exit(par(old.mar))
    }
    if (missing(by)){
        y <- table(x)
        if (scale == "percent") y <- 100*y/sum(y)
        mids <- barplot(y, xlab=xlab, ylab=ylab, col=col, main=main, ...)
        if(label.bars){
            labels <- if (scale == "percent") paste0(round(y), "%") else y
            text(mids, y, labels, pos=1, offset=0.5)
        }
    }
    else{
        nlevels <- length(levels(by))
        col <- col[1:nlevels]
        y <- table(by, x)
        if (scale == "percent") {
            y <- if (conditional) 100*apply(y, 2, function(x) x/sum(x))
                 else 100*y/sum(y)
        }
        if (legend.pos == "above"){
            legend.columns <- find.legend.columns(nlevels)
            top <- 4 + ceiling(nlevels/legend.columns)
            xpd <- par(xpd=TRUE)
            on.exit(par(xpd=xpd), add=TRUE)
            mids <- barplot(y, xlab=xlab, ylab=ylab,
                            col=col, 
                            beside = style == "parallel", ...)
            usr <- par("usr")
            legend.x <- usr[1]
            legend.y <- usr[4] + 1.2*top*strheight("x")
            legend.pos <- list(x=legend.x, y=legend.y)
            title(main=main, line=mar[3] - 1)
            legend(legend.pos, title=legend.title, legend=levels(by), 
                   fill=col,
                   ncol=legend.columns, inset=0.05)
        }
        else mids <- barplot(y, xlab=xlab, ylab=ylab, main=main,
                             legend.text=levels(by), col=col, 
                             args.legend=list(x=legend.pos, title=legend.title, inset=0.05, bg="white"),
                             beside = style == "parallel", ...)
        if (label.bars){
            yy <- if (is.matrix(mids)) as.vector(y) else as.vector(apply(y, 2, cumsum))
            labels <- if (scale == "percent") paste0(round(as.vector(y)), "%") else as.vector(y)
            xx <- if (is.vector(mids)) rep(mids, each=nrow(y)) else as.vector(mids)
            text(xx, yy, labels, pos=1, offset=0.5)
        }
    }
    return(invisible(mids))
}
