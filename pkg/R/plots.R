#' Plot a Histogram
#' 
#' @name Hist
#'
#' @keywords hplot
#' 
#' @details
#' This function is a wrapper for the \code{\link[graphics]{hist}} function in the \code{base} package, permitting percentage scaling of the vertical axis in addition to frequency and density scaling.
#'
#' @param x a vector of values for which a histogram is to be plotted.
#' @param groups a factor (or character or logical variable) to create histograms by group with common horizontal and vertical scales.
#' @param scale the scaling of the vertical axis: \code{"frequency"} (the default), \code{"percent"}, or \code{"density"}.
#' @param xlab x-axis label, defaults to name of variable.
#' @param ylab y-axis label, defaults to value of \code{scale}.
#' @param main main title for graph, defaults to empty.
#' @param breaks see the \code{breaks} argument for \code{\link[graphics]{hist}}.
#' @param ... arguments to be passed to \code{hist}.
#'
#' @return This function is primarily called for its side effect ---  plotting a histogram or histograms --- but it also invisibly returns an object of class \code{\link[graphics]{hist}} or a list of \code{hist} objects.
#'
#' @author John Fox
#'
#' @seealso \code{\link[graphics]{hist}}
#'
#' @examples
#' data(Prestige, package="car")
#' Hist(Prestige$income, scale="percent")
#' with(Prestige, Hist(income, groups=type))
#'
#' @export
Hist <- function(x, groups, scale=c("frequency", "percent", "density"), xlab=deparse(substitute(x)), ylab=scale, main="", breaks="Sturges", ...){
    xlab # evaluate
    scale <- match.arg(scale)
    ylab
    if (!missing(groups)){
        groupsName <- deparse(substitute(groups))
        if (!is.factor(groups)){
            if (!(is.character(groups) || is.logical(groups)))
                warning("groups variable is not a factor, character, or logical")
            groups <- as.factor(groups)
        }
        counts <- table(groups)
        if (any(counts == 0)){
            levels <- levels(groups)
            warning("the following groups are empty: ", paste(levels[counts == 0], collapse=", "))
        }
        levels <- levels(groups)
        hists <- lapply(levels, function(level) if (counts[level] != 0)  
                                                    hist(x[groups == level], plot=FALSE, breaks=breaks)
                                                else list(breaks=NA))
        range.x <- range(unlist(lapply(hists, function(hist) hist$breaks)), na.rm=TRUE)
        n.breaks <- max(sapply(hists, function(hist) length(hist$breaks)))
        breaks. <- seq(range.x[1], range.x[2], length=n.breaks)
        hists <- lapply(levels, function(level) if (counts[level] != 0) 
                                                    hist(x[groups == level], plot=FALSE, breaks=breaks.)
                                                else list(counts=0, density=0))
        names(hists) <- levels
        ylim <- if (scale == "frequency"){
                    max(sapply(hists, function(hist) max(hist$counts)))
                }
                else if (scale == "density"){
                    max(sapply(hists, function(hist) max(hist$density)))
                }
                else {
                    max.counts <- sapply(hists, function(hist) max(hist$counts))
                    tot.counts <- sapply(hists, function(hist) sum(hist$counts))
                    ylims <- tot.counts*(max(max.counts[tot.counts != 0]/tot.counts[tot.counts != 0]))
                    names(ylims) <- levels
                    ylims
                }
        save.par <- par(mfrow=n2mfrow(sum(counts != 0)), oma = c(0, 0, if (main != "") 1.5 else 0, 0))
        on.exit(par(save.par))
        for (level in levels){
            if (counts[level] == 0) next
            if (scale != "percent") Hist(x[groups == level], scale=scale, xlab=xlab, ylab=ylab, 
                                         main=paste(groupsName, "=", level), breaks=breaks., ylim=c(0, ylim), ...)
            else Hist(x[groups == level], scale=scale, xlab=xlab, ylab=ylab, 
                      main=paste(groupsName, "=", level), breaks=breaks., ylim=c(0, ylim[level]), ...)
        }
        if (main != "") mtext(side = 3, outer = TRUE, main, cex = 1.2)
        return(invisible(hists))
    }
    x <- na.omit(x)
    if (scale == "frequency") {
        hist <- hist(x, xlab=xlab, ylab=ylab, main=main, breaks=breaks, ...)
    }
    else if (scale == "density") {
        hist <- hist(x, freq=FALSE, xlab=xlab, ylab=ylab, main=main, breaks=breaks, ...)
    }
    else {
        n <- length(x)
        hist <- hist(x, axes=FALSE, xlab=xlab, ylab=ylab, main=main, breaks=breaks, ...)
        axis(1)
        max <- ceiling(10*par("usr")[4]/n)
        at <- if (max <= 3) (0:(2*max))/20
              else (0:max)/10
        axis(2, at=at*n, labels=at*100)
    }
    box()
    abline(h=0)
    invisible(hist)
}

#' Index Plots
#'
#' @name indexplot
#'
#' @keywords hplot
#'
#' @details
#' Index plots with point identification.
#'
#' @param x a numeric variable, a matrix whose columns are numeric variables, or a numeric data frame; if \code{x} is a matrix or data frame, plots vertically aligned index plots for the columns.
#' @param labels point labels; if \code{x} is a data frame, defaults to the row names of \code{x}, otherwise to the case index.
#' @param groups an optional grouping variable, typically a factor (or character or logical variable).
#' @param id.method method for identifying points; see \code{\link[car]{showLabels}}.
#' @param type to be passed to \code{\link{plot}}.
#' @param id.n number of points to identify; see \code{\link[car]{showLabels}}.
#' @param ylab label for vertical axis; if missing, will be constructed from \code{x}; for a data frame, defaults to the column names.
#' @param legend see \code{\link[graphics]{legend}}) giving location of the legend if \code{groups} are specified; if \code{legend=FALSE}, the legend is suppressed.
#' @param title title for the legend; may normally be omitted.
#' @param col vector of colors for the \code{groups}.
#' @param \dots to be passed to \code{plot}.
#'
#' @return Returns labelled indices of identified points or (invisibly) \code{NULL} if no points are identified or if there are multiple variables with some missing data.
#'
#' @author John Fox
#'
#' @seealso \code{\link[car]{showLabels}}, \code{\link[graphics]{plot.default}}
#'
#' @examples
#' with(Prestige, indexplot(income, id.n=2, labels=rownames(Prestige)))
#' with(Prestige, indexplot(Prestige[, c("income", "education", "prestige")],
#'                groups = Prestige$type, id.n=2))
#'
#' @export
indexplot <- function(x, groups, labels=seq_along(x), id.method="y", type="h", id.n=0, ylab, legend="topright", title, col=palette(), ...){
    if (is.data.frame(x)) {
        if (missing(labels)) labels <- rownames(x)
        x <- as.matrix(x)
    }
    if (!missing(groups)){
        if (missing(title)) title <- deparse(substitute(groups))
        if (!is.factor(groups)) groups <- as.factor(groups)
        groups <- addNA(groups, ifany=TRUE)
        grps <- levels(groups)
        grps[is.na(grps)] <- "NA"
        levels(groups) <- grps
        if (length(grps) > length(col)) stop("too few colors to plot groups")
    }
    else {
        grps <- NULL
        legend <- FALSE
    }
    if (is.matrix(x)){
        ids <- NULL
        mfrow <- par(mfrow=c(ncol(x), 1))
        on.exit(par(mfrow)) 
        if (missing(labels)) labels <- 1:nrow(x)
        if (is.null(colnames(x))) colnames(x) <- paste0("Var", 1:ncol(x))
        for (i in 1:ncol(x)) {
            id <- indexplot(x[, i], groups=groups, labels=labels, id.method=id.method, type=type, id.n=id.n,
                            ylab=if (missing(ylab)) colnames(x)[i] else ylab, legend=legend, title=title, ...)
            ids <- union(ids, id)
            legend <- FALSE
        }
        if (is.null(ids) || any(is.na(x))) return(invisible(NULL)) else {
                                                                       ids <- sort(ids)
                                                                       names(ids) <- labels[ids]
                                                                       if (any(is.na(names(ids))) || all(ids == names(ids))) names(ids) <- NULL
                                                                       return(ids)
                                                                   }
    }
    if (missing(ylab)) ylab <- deparse(substitute(x))
    plot(x, type=type, col=if (is.null(grps)) col[1] else col[as.numeric(groups)],
         ylab=ylab, xlab="Observation Index", ...)
    if (!isFALSE(legend)){
        legend(legend, title=title, bty="n",
               legend=grps, col=palette()[1:length(grps)], lty=1, horiz=TRUE, xpd=TRUE)
    }
    if (par("usr")[3] <= 0) abline(h=0, col='gray')
    ids <- showLabels(seq_along(x), x, labels=labels, method=id.method, n=id.n)
    if (is.null(ids)) return(invisible(NULL)) else return(ids)
}

#' Plot a one or more lines
#'
#' @name lineplot
#'
#' @keywords hplot
#'
#' @details
#' This function plots lines for one or more variables against another variable, typically time series against time.
#'
#' @param x variable giving horizontal coordinates.
#' @param \dots one or more variables giving vertical coordinates.
#' @param legend plot legend? Default is \code{TRUE} if there is more than one variable to plot and \code{FALSE} is there is just one.
#'
#' @return
#' Produces a plot; returns \code{NULL} invisibly.
#'
#' @author John Fox
#'
#' @examples
#' data(Bfox)
#' Bfox$time <- as.numeric(rownames(Bfox))
#' with(Bfox, lineplot(time, menwage, womwage))
#'
#' @export
lineplot <- function(x, ..., legend){
    xlab <- deparse(substitute(x))
    y <- cbind(...)
    m <- ncol(y)
    legend <- if (missing(legend)) m > 1
    if (legend && m > 1) {
        mar <- par("mar")
        top <- 3.5 + m
        old.mar <- par(mar=c(mar[1:2], top, mar[4]))
        on.exit(par(old.mar))
    }
    if (m > 1) matplot(x, y, type="b", lty=1, xlab=xlab, ylab="")
    else plot(x, y, type="b", pch=16, xlab=xlab, ylab=colnames(y))
    if (legend && ncol(y) > 1){
        xpd <- par(xpd=TRUE)
        on.exit(par(xpd), add=TRUE)
        ncols <- length(palette())
        cols <- rep(1:ncols, 1 + m %/% ncols)[1:m]
        usr <- par("usr")
        legend(usr[1], usr[4] + 1.2*top*strheight("x"), 
               legend=colnames(y), col=cols, lty=1, pch=as.character(1:m))
    }
    return(invisible(NULL))
}

#' Plot a probability density, mass, or distribution function.
#' 
#' @name plotDistr
#'
#' @keywords hplot
#'
#' @details
#' This function plots a probability density, mass, or distribution function, adapting the form of the plot as appropriate.
#'
#' @param x horizontal coordinates
#' @param p vertical coordinates
#' @param discrete is the random variable discrete?
#' @param cdf is this a cumulative distribution (as opposed to mass) function?
#' @param regions for continuous distributions only, if non-\code{NULL}, a list of regions to fill with color \code{col}; each element of the list is a pair of \code{x} values with the minimum and maximum horizontal coordinates of the corresponding region. 
#' @param col color for plot, \code{col} may be a single value or a vector.
#' @param legend plot a legend of the regions (default \code{TRUE}).
#' @param legend.pos position for the legend (see \code{\link[graphics]{legend}}, default \code{"topright"}).
#' @param \dots arguments to be passed to \code{plot}.
#'
#' @return Produces a plot; returns \code{NULL} invisibly.
#'
#' @author John Fox
#'
#' @examples
#' x <- seq(-4, 4, length=100)
#' plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)", main="Standard Normal Density")
#' plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)", main="Standard Normal Density",
#'           region=list(c(1.96, Inf), c(-Inf, -1.96)), col=c("red", "blue"), new = TRUE)
#' plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)", main="Standard Normal Density",
#'           region=list(c(qnorm(0), qnorm(.025)), c(qnorm(.975), qnorm(1)))) # same
#'
#' x <- 0:10
#' plotDistr(x, pbinom(x, 10, 0.5), xlab="successes", discrete=TRUE, cdf=TRUE,
#'           main="Binomial Distribution Function, p=0.5, n=10")
#'
#' @export
plotDistr <- function(x, p, discrete=FALSE, cdf=FALSE, regions=NULL, col="gray", 
                      legend=TRUE, legend.pos="topright", ...){
    if (discrete){
        if (cdf){
            plot(x, p, ..., type="n")
            abline(h=0:1, col="gray")
            lines(x, p, ..., type="s")
        }
        else {
            plot(x, p, ..., type="h")
            points(x, p, pch=16)
            abline(h=0, col="gray")
        }
    }
    else{
        if (cdf){
            plot(x, p, ..., type="n")
            abline(h=0:1, col="gray")
            lines(x, p, ..., type="l")
        }
        else{
            plot(x, p, ..., type="n")
            abline(h=0, col="gray")
            lines(x, p, ..., type="l")
        }
        if (!is.null(regions)){
            col <- rep(col, length=length(regions))
            for (i in 1:length(regions)){
                region <- regions[[i]]
                which.xs <- (x >= region[1] & x <= region[2])
                xs <- x[which.xs]
                ps <- p[which.xs]
                xs <- c(xs[1], xs, xs[length(xs)])
                ps <- c(0, ps, 0)
                polygon(xs, ps, col=col[i])
            }
            if (legend){
                if (length(unique(col)) > 1){
                    legend(legend.pos, title = if (length(regions) > 1) "Regions" else "Region", 
                           legend=sapply(regions, function(region){ 
                               paste(round(region[1], 2), "to", round(region[2], 2))
                           }),
                           col=col, pch=15, pt.cex=2.5, inset=0.02)
                }
                else
                {
                    legend(legend.pos, title = if (length(regions) > 1) "Regions" else "Region", 
                           legend=sapply(regions, function(region){ 
                               paste(round(region[1], 2), "to", round(region[2], 2))
                           }), inset=0.02)
                }
            }
        }
    }
    return(invisible(NULL))
}

#' Plot Means for One or Two-Way Layout
#' 
#' @name plotMeans
#'
#' @keywords hplot
#'
#' @details
#' Plots cell means for a numeric variable in each category of a factor or in each combination of categories of two factors, optionally along with error bars based on cell standard errors or standard deviations.
#'
#' @param response Numeric variable for which means are to be computed.
#' @param factor1 Factor defining horizontal axis of the plot.
#' @param factor2 If present, factor defining profiles of means.
#' @param error.bars If \code{"se"}, the default, error bars around means give plus or minus one standard error of the mean; if \code{"sd"}, error bars give plus or minus one standard deviation; if \code{"conf.int"}, error bars give a confidence interval around each mean; if \code{"none"}, error bars are suppressed.
#' @param level level of confidence for confidence intervals; default is .95
#' @param xlab Label for horizontal axis.
#' @param ylab Label for vertical axis.
#' @param legend.lab Label for legend.
#' @param legend.pos Position of legend; if \code{"farright"} (the default), extra space is left at the right of the plot.
#' @param main Label for the graph.
#' @param pch Plotting characters for profiles of means.
#' @param lty Line types for profiles of means.
#' @param col Colours for profiles of means.
#' @param connect connect profiles of means, default \code{TRUE}.
#' @param \ldots arguments to be passed to \code{plot}.
#'
#' @return The function invisibly returns \code{NULL}.
#'
#' @examples
#' data(Moore)
#' with(Moore, plotMeans(conformity, fcategory, partner.status, ylim=c(0, 25)))
#'
#' @author John Fox
#'
#' @seealso \code{\link[stats]{interaction.plot}}
#'
#' @export
plotMeans <- function(response, factor1, factor2, error.bars = c("se", "sd", "conf.int", "none"),
                      level=0.95, xlab=deparse(substitute(factor1)), ylab=paste("mean of", deparse(substitute(response))),
                      legend.lab=deparse(substitute(factor2)), 
                      legend.pos=c("farright", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"),
                      main="Plot of Means",
                      pch=1:n.levs.2, lty=1:n.levs.2, col=palette(), connect=TRUE, ...){
    if (!is.numeric(response)) stop("Argument response must be numeric.")
    xlab # force evaluation
    ylab
    legend.lab
    legend.pos <- match.arg(legend.pos)
    error.bars <- match.arg(error.bars)
    if (!is.factor(factor1)) {
        if (!(is.character(factor1) || is.logical(factor1))) 
            stop("Argument factor1 must be a factor, character, or logical.")
        factor1 <- as.factor(factor1)
    }
    if (missing(factor2)){
        valid <- complete.cases(factor1, response)
        factor1 <- factor1[valid]
        response <- response[valid]
        means <- tapply(response, factor1, mean)
        sds <- tapply(response, factor1, sd)
        ns <- tapply(response, factor1, length)
        if (error.bars == "se") sds <- sds/sqrt(ns)
        if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
        sds[is.na(sds)] <- 0
        yrange <-  if (error.bars != "none") c( min(means - sds, na.rm=TRUE), max(means + sds, na.rm=TRUE)) else range(means, na.rm=TRUE)
        levs <- levels(factor1)
        n.levs <- length(levs)
        plot(c(1, n.levs), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main, ...)
        points(1:n.levs, means, type=if (connect) "b" else "p", pch=16, cex=2)
        box()
        axis(2)
        axis(1, at=1:n.levs, labels=levs)
        if (error.bars != "none") arrows(1:n.levs, means - sds, 1:n.levs, means + sds,
                                         angle=90, lty=2, code=3, length=0.125)
    }
    else {
        if (!is.factor(factor2)) {
            if (!(is.character(factor2) || is.logical(factor2))) 
                stop("Argument factor2 must be a factor, charcter, or logical.")
            factor2 <- as.factor(factor2)
        }        
        valid <- complete.cases(factor1, factor2, response)
        factor1 <- factor1[valid]
        factor2 <- factor2[valid]
        response <- response[valid]
        means <- tapply(response, list(factor1, factor2), mean)
        sds <- tapply(response, list(factor1, factor2), sd)
        ns <- tapply(response, list(factor1, factor2), length)
        if (error.bars == "se") sds <- sds/sqrt(ns)
        if (error.bars == "conf.int") sds <- qt((1 - level)/2, df=ns - 1, lower.tail=FALSE) * sds/sqrt(ns)
        sds[is.na(sds)] <- 0
        yrange <-  if (error.bars != "none") c( min(means - sds, na.rm=TRUE), max(means + sds, na.rm=TRUE)) else range(means, na.rm=TRUE)
        levs.1 <- levels(factor1)
        levs.2 <- levels(factor2)
        n.levs.1 <- length(levs.1)
        n.levs.2 <- length(levs.2)
        if (length(pch) == 1) pch <- rep(pch, n.levs.2)
        if (length(col) == 1) col <- rep(col, n.levs.2)
        if (length(lty) == 1) lty <- rep(lty, n.levs.2)
        expand.x.range <- if (legend.pos == "farright") 1.4 else 1
        if (n.levs.2 > length(col)) stop(sprintf("Number of groups for factor2, %d, exceeds number of distinct colours, %d."), n.levs.2, length(col))		
        plot(c(1, n.levs.1 * expand.x.range), yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE, main=main, ...)
        box()
        axis(2)
        axis(1, at=1:n.levs.1, labels=levs.1)
        for (i in 1:n.levs.2){
            points(1:n.levs.1, means[, i], type=if (connect) "b" else "p", pch=pch[i], cex=2, col=col[i], lty=lty[i])
            if (error.bars != "none") arrows(1:n.levs.1, means[, i] - sds[, i],
                                             1:n.levs.1, means[, i] + sds[, i], angle=90, code=3, col=col[i], lty=lty[i], length=0.125)
        }
        if (legend.pos == "farright"){
            x.posn <- n.levs.1 * 1.1
            y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3,4)])
                                        #            text(x.posn, y.posn, legend.lab, adj=c(0, -.5))
            legend(x.posn, y.posn, levs.2, pch=pch, col=col, lty=lty, title=legend.lab)
        }
        else legend(legend.pos, levs.2, pch=pch, col=col, lty=lty, title=legend.lab, inset=0.02)
    }
    invisible(NULL)
}
