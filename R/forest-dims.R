#' Calculate forest plot dimensions
#'
#' Extracts the exact width and height of a `meta` forest plot from its internal
#' grid layout. This is primarily used to determine the optimal dimensions for
#' saving the forest plot to disk.
#'
#' Rather than using guesswork or manual row counting, `forest_dims()` captures
#' the forest plot as a true graphics object and extracts the precise width and
#' height directly from its underlying structure.
#'
#' Because it mathematically measures the actual rendered components, it is
#' highly robust. It works seamlessly with any forest plot configuration.
#'
#' @param x An object of class `meta` (e.g., from [meta::metacont()],
#'   [meta::metabin()], or [meta::metagen()]).
#' @param ... Additional arguments passed on to the underlying `forest` method
#'   (e.g., [meta::forest.meta()], [meta::forest.metabind()],
#'   [meta::forest.metacum()], or [meta::forest.metainf()]).
#' @param units Units of the returned `width` and `height`. One of `"in"`,
#'   `"cm"`, or `"mm"`. Defaults to `"in"`.
#' @return A named list with elements:
#' * `width`: The width of the forest plot in the specified units.
#' * `height`: The height of the forest plot in the specified units.
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
  extra_width <- from_inches(0.3, units)
  extra_height <- from_inches(0.8, units)

  list(
    width = width + extra_width,
    height = height + extra_height
  )
}
