#' Tufte's Box Blot
#'
#' Edward Tufte's revision of the box plot erases the box and
#' replaces it with a single point and the whiskers.
#'
#' @section Aesthetics:
#' \itemize{
#' \item x [required]
#' \item y [required]
#' \item colour
#' \item size
#' \item linetype
#' \item shape
#' \item fill
#' \item alpha
#' }
#'
#' @references Tufte, Edward R. (2001) The Visual Display of
#' Quantitative Information, Chapter 6.
#'
#' McGill, R., Tukey, J. W. and Larsen, W. A. (1978) Variations of
#'     box plots. The American Statistician 32, 12-16.
#'
#' @seealso \code{\link{geom_boxplot}}
#' @inheritParams ggplot2::geom_point
#' @param outlier.colour colour for outlying points
#' @param outlier.shape shape of outlying points
#' @param outlier.size size of outlying points
#' @param fatten a multiplicative factor to fatten the middle point
#' (or line) by
#' @param median.type One of \code{'box'}, \code{'line'}, or \code{'box'}. If \code{median.type='point'},
#' then use whitespace to represent the central quartiles and a point at the median.
#' If \code{median.type='box'}, then use a box to represent the standard error of the median. This
#' is similar to what the \code{notch} option does in a standard boxplot.
#' the same thing as the notch does in a standard boxplot.
#' If \code{median.type='line'}, the use offset lines to represent the central quartile and whitespace at the median
#' @family geom tufte
#' @export
#'
#' @examples
#' p <- ggplot(mtcars, aes(factor(cyl), mpg))
#' ## with only a point
#' p + geom_tufteboxplot()
#' ## with a middle box
#' p + geom_tufteboxplot(median.type='box', fatten=1)
#' ## using lines
#' p + geom_tufteboxplot(median.type='line')
#'
geom_tufteboxplot <-
  function(mapping = NULL, data = NULL,
           stat = "fivenumber", position = "dodge",
           fatten = 4,
           median.type = "point",
           show.legend = NA, inherit.aes = TRUE, ...) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomTufteboxplot,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      geom_params = list(
        fatten = fatten,
        median.type = median.type
      ),
      params = list(...)
    )
  }

GeomTufteboxplot <- ggproto("GeomTufteboxplot", Geom,

  draw_group = function(self, data, scales, coordinates,
                        fatten = 4, median.type = c("point", "line"),
                        line.offset = unit(0.01, "npc"),
                        line.width = 1,
                        line.gapsize = unit(0.01, "npc"),
                        ...) {
    median.type <- match.arg(median.type)
    common <-
      data.frame(
        colour = data$colour,
        linetype = data$linetype,
        fill = alpha(data$fill, data$alpha),
        group = NA,
        stringsAsFactors = FALSE
      )

    whiskers <-
      data.frame(
        x = data$x,
        xend = data$x,
        y = c(data$upper, data$lower),
        yend = c(data$ymax, data$ymin),
        size = data$size,
        alpha = NA,
        common,
        stringsAsFactors = FALSE
      )

    if (median.type == "line") {
      # draw two vertical lines for the central quartiles and two short horizontal lines to connect them back to the whiskers
      middata <-
        data.frame(
          x = data$x,
          xend = data$x,
          y = c(data$upper, data$middle),
          yend = c(data$middle, data$lower),
          size = data$size * line.width,
          common,
          stringsAsFactors = FALSE
        )

      middle_grob <-
        GeomSegment$draw(middata, scales, coordinates, ...)

      # need to edit the grob directly since the offset and gap are
      # best measured in terms of graphical units rather than the scale
      # of x and y
      # shift line left or right
      middle_grob$x0 <- middle_grob$x0 + rep(line.offset, 2)
      middle_grob$x1 <- middle_grob$x1 + rep(line.offset, 2)
      # make a gap by shifting the endpoints of the line
      middle_grob$y0 <- middle_grob$y0 - unit.c(unit(0, "native"),
                                                line.gapsize * 0.5)
      middle_grob$y1 <- middle_grob$y1 + unit.c(line.gapsize * 0.5,
                                                unit(0, "native"))
    } else if (median.type == "point") {
      midpoint <- data.frame(
        y = data$middle,
        x = data$x,
        size = data$size * fatten,
        common,
        stringsAsFactors = FALSE
      )
      middle_grob <- GeomPoint$draw(midpoint, scales, coordinates, ...)
    } else {
      stop("`median.type` must be one of \"point\" or \"line\".")
    }
    grid::grobTree(GeomSegment$draw(whiskers, scales, coordinates, ...),
                   middle_grob)
  },

  draw_legend = draw_key_pointrange,
  default_aes = aes(
    colour = "black", size = 0.5, linetype = 1, shape = 19, fontsize = 10,
    fill = "gray20", alpha = NA, stroke = 1),
  required_aes =  c("x", "lower", "upper", "middle", "ymin", "ymax")
)
