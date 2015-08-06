#' Calculate components of a five-number summary
#'
#' The five number summary of a sample is the minimum, first quartile,
#' median, third quartile, and maximum.
#'
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @inheritParams ggplot2::stat_identity
#' @return A data frame with additional columns:
#'   \item{ymin}{minimum}
#'   \item{lower}{lower hinge, 25\% quantile}
#'   \item{middle}{median, 50\% quantile}
#'   \item{upper}{upper hinge, 75\% quantile}
#'   \item{ymax}{maximum}
#' @seealso \code{\link{stat_boxplot}}
#' @export
stat_fivenumber <- function(mapping = NULL, data = NULL, geom = "boxplot",
                            position = "dodge", na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE, ...) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatFivenumber,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    stat_params = list(
      na.rm = na.rm
    ),
    params = list(...)
  )
}

#' @rdname stat_fivenumber
#' @format NULL
#' @usage NULL
#' @export
StatFivenumber <- ggproto("StatFivenumber", Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, na.rm = FALSE, ...) {
    qs <- c(0, 0.25, 0.5, 0.75, 1)

    data <-
      remove_missing(data, na.rm, c("x", "y", "weight"), name = "stat_boxplot",
                     finite = TRUE)

    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
      stats <- as.numeric(stats::coef(mod))
    } else {
      stats <- as.numeric(stats::quantile(data$y, qs))
    }
    names(stats) <-
      c("ymin", "lower", "middle", "upper", "ymax")
    as.data.frame(as.list(stats))
  }
)
