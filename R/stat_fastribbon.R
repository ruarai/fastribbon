require(ggplot2)
require(ggdist)


StatFastribbon <- ggproto(
  "StatFastribbon",
  ggdist:::StatLineribbon,

  compute_panel = function(self, data, scales, .width = c(0.5, 0.8, 0.95), na.rm = FALSE, ...) {
    if (nrow(data) == 0) return(data.frame())

    lower_probs <- (1 - .width) / 2
    upper_probs <- 1 - lower_probs
    probs <- sort(unique(c(0.5, lower_probs, upper_probs)))

    xs <- unique(data$x)

    data <- data |>
      arrange(x) |>
      group_by(group, x) |>
      mutate(draw_id = row_number(),
             x = match(x, xs)) |>
      ungroup() |>
      mutate(col_id = paste(group, x, sep = "___"))

    poisoned_cols <- data |>
      filter(is.na(y)) |>
      pull(col_id) |>
      unique()

    wide_data <- data |>
      pivot_wider(
        id_cols = draw_id,
        names_from = col_id,
        values_from = y,
        values_fill = NA
      )

    mat <- as.matrix(select(wide_data, -draw_id))

    col_names <- colnames(mat)
    meta_split <- strsplit(col_names, "___")

    meta_df <- data.frame(
      group = as.integer(sapply(meta_split, `[`, 1)),
      x     = as.numeric(sapply(meta_split, `[`, 2))
    )

    # Always do na.rm here to ensure we get results for groups
    # with fewer samples
    qs <- matrixStats::colQuantiles(mat, probs = probs, na.rm = TRUE)

    # But post-hoc re-make NAs here instead
    if (!na.rm && length(poisoned_cols) > 0) {
      is_poisoned <- colnames(mat) %in% poisoned_cols
      qs[is_poisoned, ] <- NA
    }

    median_idx <- match(0.5, probs)
    medians <- qs[, median_idx]

    results <- list()
    ordered_widths <- sort(.width, decreasing = TRUE)

    for (i in seq_along(ordered_widths)) {
      w <- ordered_widths[i]

      lp <- (1 - w) / 2
      up <- 1 - lp

      l_idx <- match(lp, probs)
      u_idx <- match(up, probs)

      res_df <- data.frame(
        x = xs[meta_df$x],
        group = meta_df$group,
        y = medians,
        ymin = qs[, l_idx],
        ymax = qs[, u_idx],
        .width = w,
        level = factor(w),
        f = NA_real_,
        pdf = NA_real_,
        density = NA_real_
      )

      results[[i]] <- res_df
    }

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
