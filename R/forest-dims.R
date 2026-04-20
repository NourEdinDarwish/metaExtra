#' Calculate forest plot dimensions
#'
#' Extracts the physical dimensions of a forest plot from the `meta` package
#' from its internal grid layout. Works with any forest plot configuration
#' including all layouts (`"meta"`, `"BMJ"`, `"RevMan5"`, `"JAMA"`,
#' `"subgroup"`) and subgroup analyses.
#'
#' The package internally constructs a [grid::grid.layout()] with exact column
#' widths (measured from text grobs) and uniform row heights. This function
#' captures the rendered plot, extracts that layout, and sums its physical
#' dimensions. No heuristic row counting is involved.
#'
#' @param x A `meta` object (e.g., from [meta::metacont()], [meta::metabin()],
#'   [meta::metagen()]).
#' @param ... Arguments passed to the corresponding `forest` method.
#' @param units One of `"in"` (inches), `"cm"` (centimetres), or `"mm"`
#'   (millimetres).
#'
#' @return A list with `width` and `height` in the chosen unit.
#'
#' @export
forest_dims <- function(x, ..., units = c("in", "cm", "mm")) {
  units <- rlang::arg_match0(units, c("in", "cm", "mm"))
  grid_unit <- c(`in` = "inches", cm = "cm", mm = "mm")[units]

  old_dev <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  on.exit({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev)
  })

  gtree <- grid::grid.grabExpr(meta::forest(x, ...))

  # The main viewport's layout sits at the vpTree parent
  layout <- gtree$childrenvp[[1]]$parent$layout

  # Widths: exact per-column units, sum directly
  width <- grid::convertWidth(sum(layout$widths), grid_unit, valueOnly = TRUE)

  # Heights: single unit recycled across nrow — expand then sum
  height <- grid::convertHeight(
    sum(rep(layout$heights, layout$nrow)),
    grid_unit,
    valueOnly = TRUE
  )

  # Hardcoded padding to prevent clipping of axis labels
  extra_width <- 0.2
  extra_height <- 0.7

  list(
    width = width + extra_width,
    height = height + extra_height
  )
}
