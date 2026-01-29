
#' @export
StatFastribbon <- ggproto(
  "StatFastribbon",
  ggdist:::StatLineribbon,

  compute_panel = function(self, data, scales, .width = c(0.5, 0.8, 0.95), na.rm = FALSE, ...) {
    if (nrow(data) == 0) return(data.frame())

    dt <- data.table::as.data.table(data)

    if (na.rm) {
      dt <- dt[!is.na(y)]
    }

    if (!"group" %in% names(dt)) dt[, group := 1L]

    # Sort by x then by y to prepare for c++ code
    data.table::setkey(dt, group, x, y)

    res_list <- compute_ribbon_stats(
      dt$x,
      dt$y,
      dt$group,
      .width
    )

    out <- data.table::setDT(res_list)

    out[, level := factor(level, levels = unique(level))]

    # Add dummy columns required by ggdist/ggplot2
    out[, `:=`(
      f = NA_real_,
      pdf = NA_real_,
      density = NA_real_
    )]

    return(out)
  }
)

#' @inheritParams ggdist::stat_lineribbon
#' @export
stat_fastribbon <- function(mapping = NULL, data = NULL, geom = "lineribbon",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, .width = c(0.50, 0.80, 0.95), ...) {
  ggplot2::layer(
    stat = StatFastribbon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, .width = .width, ...)
  )
}
