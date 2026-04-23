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
#' * `width`: Forest plot width.
#' * `height`: Forest plot height.
#' * `units`: Units of `width` and `height`.
#' @examples
#' # Create a simple meta-analysis object
#' m <- meta::metagen(
#'   TE = c(0.5, 0.8, 0.3),
#'   seTE = c(0.2, 0.3, 0.15),
#'   studlab = c("Study A", "Study B", "Study C")
#' )
#'
#' # Get dimensions in inches (default)
#' forest_dims(m)
#'
#' # Get dimensions in centimetres
#' forest_dims(m, units = "cm")
#' @export
forest_dims <- function(x, ..., units = c("in", "cm", "mm")) {
  units <- rlang::arg_match0(units, c("in", "cm", "mm"))
  grid_unit <- c(`in` = "inches", cm = "cm", mm = "mm")[[units]]

  # Capture ... as unevaluated expressions so that NSE arguments (e.g.,
  # sortvar = TE) are forwarded as-is to meta::forest(), which resolves
  # them via match.call() + catch().
  dots_exprs <- rlang::enexprs(...)
  user_env <- rlang::caller_env()
  call_expr <- rlang::expr(meta::forest(!!x, !!!dots_exprs))

  # convertWidth()/convertHeight() below require an open graphics device to
  # resolve grid unit conversions, so we open a null PDF device (writes
  # nowhere) for that purpose.
  #
  # Crucially, this device must be opened BEFORE grid.grabExpr(), not after.
  # This placement is entirely unrelated to convertWidth() — it's to fix a
  # separate bug where grid.grabExpr() creates an unwanted "Rplots.pdf" file
  # when running tests via the RStudio "Test" button or R CMD check.
  #
  # Here is how that bug happens:
  # grid.grabExpr() saves the current device at start (cd <- dev.cur()) and
  # restores it on exit via dev.set(cd). When no device is open before the
  # call, cd = 1 (the "null device"). grid.grabExpr() opens its own offscreen
  # device, evaluates, grabs the display list, then on cleanup: closes the
  # offscreen device and calls dev.set(1). At this exact moment there are ZERO
  # open devices. Calling dev.set(1) with no devices open triggers R to
  # automatically open a new device via getOption("device").
  #
  # Which device getOption("device") auto-opens depends on the environment.
  # In an interactive RStudio console the default device is "RStudioGD" (the
  # plot pane) — harmless, no file is written. In a non-interactive subprocess
  # (RStudio "Test" button) the default device is pdf(), which without a file
  # argument writes to "Rplots.pdf" — an unwanted side-effect.
  #
  # Opening pdf(file = NULL) here first ensures grid.grabExpr() sees
  # dev.cur() >= 2 (our null PDF), not 1. On exit it calls dev.set(2), which
  # harmlessly restores our null PDF instead of triggering the auto-open.
  #
  # Note: our own on.exit below does NOT suffer from this problem because the
  # `if (old_dev > 1)` guard skips dev.set() when old_dev is 1 (the null
  # device), so we never call dev.set(1) ourselves.
  old_dev <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  on.exit(
    {
      grDevices::dev.off()
      if (old_dev > 1) grDevices::dev.set(old_dev)
    },
    add = TRUE
  )

  # Suppress grid.newpage hooks so that external hooks (e.g., the hook
  # registered by R CMD check to annotate each plot page with help("topic")
  # labels) don't inject extra viewports into the captured gTree.
  old_hooks <- getHook("grid.newpage")
  setHook("grid.newpage", NULL, "replace")
  on.exit(setHook("grid.newpage", old_hooks, "replace"), add = TRUE)

  gtree <- grid::grid.grabExpr(eval(call_expr, envir = user_env))

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
    height = height + extra_height,
    units = units
  )
}
