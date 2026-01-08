#' Draw a Piechart With Percents or Counts in the Labels
#'
#' @name piechart
#'
#' @keywords hplot
#'
#' @details
#' \code{piechart} is a front-end to the standard R \code{\link[graphics]{pie}} function, with the capability of adding percents or counts to the pie-segment labels.
#'
#' @param x a factor or other discrete variable; the segments of the pie correspond to the unique values (levels) of \code{x} and are proportional to the frequency counts in the various levels.
#' @param scale parenthetical numbers to add to the pie-segment labels; the default is \code{"percent"}.
#' @param col colors for the segments; the default is provided by the \code{\link[colorspace]{rainbow_hcl}} function in the \pkg{colorspace} package.
#' @param \dots further arguments to be passed to \code{\link[graphics]{pie}}.
#'
#' @author John Fox
#'
#' @seealso \code{\link[graphics]{pie}}, \code{\link[colorspace]{rainbow_hcl}}
#'
#' @examples
#' with(Duncan, piechart(type))
#'
#' @export
piechart <- function(x, scale=c("percent", "frequency", "none"), 
                     col=rainbow_hcl(nlevels(x)), ...){
    scale <- match.arg(scale)
    if (!is.factor(x)) x <- as.factor(x)
    labels <- levels(x)
    tab <- table(x)
    labels <- if (scale == "percent") {
                  tab <- 100*tab/sum(tab)
                  paste0(labels, " (", round(tab), "%)")
              } else if (scale == "frequency") paste0(labels, " (", tab, ")")
              else labels
    pie(tab, labels=labels, col=col, ...)
}
