
StatFastribbon <- ggproto(
  "StatFastribbon",
  ggdist:::StatLineribbon,

  compute_panel = function(self, data, scales, .width = c(0.5, 0.8, 0.95), na.rm = FALSE, ...) {
    if (nrow(data) == 0) return(data.frame())

    dt <- as.data.table(data)

    lower_probs <- (1 - .width) / 2
    upper_probs <- 1 - lower_probs
    probs <- sort(unique(c(0.5, lower_probs, upper_probs)))

    setorder(dt, group, x)
    dt[, segment_id := .GRP, by = .(group, x)]

    dt[, draw_id := rowid(segment_id)]

    meta_map <- unique(dt[, .(segment_id, group, x)])

    poisoned_segments <- integer(0)
    if (!na.rm) {
      poisoned_segments <- dt[is.na(y), unique(segment_id)]
    }

    wide_dt <- dcast(dt, draw_id ~ segment_id, value.var = "y", fill = NA)

    mat <- as.matrix(wide_dt[, -1])

    qs <- matrixStats::colQuantiles(mat, probs = probs, na.rm = TRUE)

    if (length(poisoned_segments) > 0) {
      qs[poisoned_segments, ] <- NA
    }

    median_idx <- match(0.5, probs)
    medians <- qs[, median_idx]

    ordered_widths <- sort(.width, decreasing = TRUE)

    results <- lapply(ordered_widths, function(w) {
      lp <- (1 - w) / 2
      up <- 1 - lp

      data.frame(
        x = meta_map$x,
        group = meta_map$group,
        y = medians,
        ymin = qs[, match(lp, probs)],
        ymax = qs[, match(up, probs)],
        .width = w,
        level = factor(w),
        f = NA_real_,
        pdf = NA_real_,
        density = NA_real_
      )
    })

    out <- do.call(rbind, results)
    out[order(out$.width, decreasing = TRUE), ]
  }
)

stat_fastribbon <- function(mapping = NULL, data = NULL, geom = "lineribbon",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, .width = c(0.50, 0.80, 0.95), ...) {
  layer(
    stat = StatFastribbon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, .width = .width, ...)
  )
}
