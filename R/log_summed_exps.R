#' log-sum-exp
#'
#' @param x a numeric vector
#'
#' @return the logarithm of the sum of exponentials of \code{x}
#'
#' @examples
#' log_summed_exps(7)
#' log_summed_exps(c(1, 2, 3))
#'
#'
#' @export
log_summed_exps <- function(x) {
  ##definition of log summed exps:
  max.x <- max(x)
  lse <- max.x + log(sum(exp(x - max.x)))
  return(lse)
}
