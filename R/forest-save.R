#' Save a forest plot to disk
#'
#' Saves a `meta` forest plot to disk with dimensions automatically calculated
#' from the forest plot's internal grid layout via [forest_dims()].
#'
#' The auto-calculated dimensions are exposed to the `width` and `height`
#' arguments as the special variables `.width` and `.height`. You can reference
#' these in mathematical expressions to easily adjust the forest plot's size
#' relative to its optimal dimensions:
#'
#' ```r
#' # Auto dimensions (default)
#' forest_save(m, filename = "plot.png")
#'
#' # 20% taller and 2 units wider than the auto-calculated dimensions
#' forest_save(m, filename = "plot.png", width = .width + 2, height = .height * 1.2)
#'
#' # Explicit override
#' forest_save(m, filename = "plot.png", width = 10, height = 8)
#' ```
#'
#' @param x An object of class `meta` (e.g., from [meta::metacont()],
#'   [meta::metabin()], or [meta::metagen()]).
#' @param filename File name to create on disk.
#' @param ... Additional arguments passed on to the underlying `forest` method
#'   (e.g., [meta::forest.meta()], [meta::forest.metabind()],
#'   [meta::forest.metacum()], or [meta::forest.metainf()]).
#' @param device Device to use. Can either be a device function (e.g. [png]), or
#'   one of `"eps"`, `"ps"`, `"tex"` (pictex), `"pdf"`, `"jpeg"`, `"tiff"`,
#'   `"png"`, `"bmp"`, `"svg"` or `"wmf"` (Windows only). If `NULL` (default),
#'   the device is guessed based on the `filename` extension.
#' @param path Path of the directory to save the forest plot to: `path` and
#'   `filename` are combined to create the fully qualified file name. Defaults
#'   to the working directory.
#' @param width,height Forest plot size in units expressed by the `units`
#'   argument. Defaults to `.width` and `.height`, which are the auto-calculated
#'   dimensions from [forest_dims()]. You can use `.width` and `.height` in
#'   expressions (e.g., `height = .height * 1.2`) to adjust the forest plot
#'   size. If set to `NA`, the size of the current graphics device is used.
#' @param units One of the following units in which the `width` and `height`
#'   arguments are expressed: `"in"`, `"cm"`, `"mm"` or `"px"`. Defaults to
#'   `"in"`.
#' @param dpi Plot resolution. Defaults to `300`.
#' @param device_args A named list of additional arguments passed directly to
#'   the underlying graphics device (e.g., `list(bg = "transparent", pointsize =
#'   12)`).
#' @return A named list (returned invisibly) with elements:
#' * `file`: Full file path.
#' * `width`: Forest plot width.
#' * `height`: Forest plot height.
#' * `units`: Units of `width` and `height`.
#' * `dpi`: Forest plot resolution.
#' @export
forest_save <- function(
  x,
  filename,
  ...,
  device = NULL,
  path = NULL,
  width = .width,
  height = .height,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  device_args = NULL
) {
  units <- rlang::arg_match0(units, c("in", "cm", "mm", "px"))

  # Capture width/height expressions before evaluation
  width_expr <- rlang::enexpr(width)
  height_expr <- rlang::enexpr(height)

  # Always measure in inches (grid's natural unit), then convert to user's units
  dims_in <- forest_dims(x, ..., units = "in")
  .width <- from_inches(dims_in$width, units, dpi = dpi)
  .height <- from_inches(dims_in$height, units, dpi = dpi)

  # Make .width/.height available for user expressions
  eval_env <- list2env(
    list(.width = .width, .height = .height),
    parent = rlang::caller_env()
  )

  width <- eval(width_expr, envir = eval_env)
  height <- eval(height_expr, envir = eval_env)

  checkmate::assert_list(device_args, null.ok = TRUE, .var.name = "device_args")

  # Save
  plot_fn <- function() meta::forest(x, ...)

  rlang::inject(save_plot(
    plot = plot_fn,
    filename = filename,
    device = device,
    path = path,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    !!!device_args
  ))
}
