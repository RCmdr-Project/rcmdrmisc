#' Stepwise Model Selection
#'
#' @name stepwise
#'
#' @keywords models
#'
#' @details
#' This function is a front end to the \code{\link[MASS]{stepAIC}} function in the \pkg{MASS} package.
#'
#' @param mod a model object of a class that can be handled by \code{stepAIC}.
#' @param direction if \code{"backward/forward"} (the default), selection starts with the full model and eliminates predictors one at a time, at each step considering whether the criterion will be improved by adding back in a variable removed at a previous step; if \code{"forward/backwards"}, selection starts with a model including only a constant, and adds predictors one at a time, at each step considering whether the criterion will be improved by removing a previously added variable; \code{"backwards"} and \code{"forward"} are similar without the reconsideration at each step.
#' @param criterion for selection. Either \code{"BIC"} (the default) or \code{"AIC"}. Note that \code{stepAIC} labels the criterion in the output as \code{"AIC"} regardless of which criterion is employed.
#' @param \dots arguments to be passed to \code{stepAIC}.
#'
#' @return The model selected by \code{stepAIC}.
#'
#' @author John Fox
#'
#' @seealso \code{\link[MASS]{stepAIC}}
#'
#' @references
#' W. N. Venables and B. D. Ripley \emph{Modern Applied Statistics Statistics with S, Fourth Edition} Springer, 2002.
#'
#' @examples
#' ## adapted from stepAIC in MASS
#' ## Assigning bwt to the global environment is required to run this example within
#' ## the browser-based help system. In other contexts, standard assignment can be used.
#' if (require(MASS)){
#'    data(birthwt)
#'    bwt <<- with(birthwt, {
#'       race <- factor(race, labels = c("white", "black", "other"))
#'       ptd <- factor(ptl > 0)
#'       ftv <- factor(ftv)
#'       levels(ftv)[-(1:2)] <- "2+"
#'       data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0), ptd,
#'                  ht = (ht > 0), ui = (ui > 0), ftv)
#'    })
#'    birthwt.glm <- glm(low ~ ., family = binomial, data = bwt)
#'    print(stepwise(birthwt.glm, trace = FALSE))
#'    print(stepwise(birthwt.glm, direction="forward/backward"))
#' }
#' 
#' ## wrapper for stepAIC in the MASS package
#' @export
stepwise <- function(mod, 
                     direction=c("backward/forward", "forward/backward", "backward", "forward"), 
                     criterion=c("BIC", "AIC"), ...){
    criterion <- match.arg(criterion)
    direction <- match.arg(direction)
    cat("\nDirection: ", direction)
    cat("\nCriterion: ", criterion, "\n\n")
    k <- if (criterion == "BIC") log(nrow(model.matrix(mod))) else 2
    rhs <- paste(c("~", deparse(formula(mod)[[3]])), collapse="")
    rhs <- gsub(" ", "", rhs)
    if (direction == "forward" || direction == "forward/backward")
        mod <- update(mod, . ~ 1)
    if (direction == "backward/forward" || direction == "forward/backward") direction <- "both"
    lower <- ~ 1
    upper <- eval(parse(text=rhs))   
    stepAIC(mod, scope=list(lower=lower, upper=upper), direction=direction, k=k, ...)
}
