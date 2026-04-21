#' Save a forest plot with auto-calculated dimensions
#'
#' A thin wrapper around `meta::forest()` that saves the forest plot to a file
#' with correct dimensions. All arguments in `...` are forwarded to the
#' underlying `forest` method, so the transition from interactive plotting to
#' file output is trivial — just add `filename`.
#'
#' @details ## `.width` and `.height`
#'
#' The default dimensions are auto-calculated by [forest_dims()] and made
#' available as `.width` and `.height`. You can reference these in your own
#' expressions for `width` and `height`:
#'
#' ```r
#' # Auto dimensions (default)
#' forest_save(m, filename = "plot.png")
#'
#' # 20% taller
#' forest_save(m, filename = "plot.png", height = .height * 1.2)
#'
#' # Explicit override
#' forest_save(m, filename = "plot.png", width = 10, height = 8)
#' ```
#'
#' @param x A `meta` object (e.g., from [meta::metacont()], [meta::metabin()],
#'   [meta::metagen()]).
#' @param ... Arguments passed to the corresponding `forest` method (e.g.,
#'   `meta::forest.meta()`).
#' @param filename File name to create on disk.
#' @param device Device to use. Can either be a device function (e.g. [png]), or
#'   one of `"eps"`, `"ps"`, `"tex"` (pictex), `"pdf"`, `"jpeg"`, `"tiff"`,
#'   `"png"`, `"bmp"`, `"svg"` or `"wmf"` (Windows only). If `NULL` (default),
#'   the device is guessed based on the `filename` extension.
#' @param path Path of the directory to save the forest plot to: `path` and
#'   `filename` are combined to create the fully qualified file name. Defaults
#'   to the working directory.
#' @param width,height Forest plot size in units expressed by the `units`
#'   argument. Defaults to `.width` and `.height`, which are the auto-calculated
#'   dimensions from [forest_dims()]. If `NA`, uses the size of the current
#'   graphics device. You can use `.width` and `.height` in expressions (e.g.,
#'   `height = .height * 1.2`).
#' @param units One of the following units in which the `width` and `height`
#'   arguments are expressed: `"in"`, `"cm"`, `"mm"` or `"px"`.
#' @param dpi Forest plot resolution.
#' @param device_args A named list of additional arguments passed to the
#'   graphics device function (e.g., `list(bg = "white", pointsize = 12)`).
#'
#' @return A named list (returned invisibly) with elements:
#' * `file`: Full file path.
#' * `width`: Forest plot width.
#' * `height`: Forest plot height.
#' * `units`: Unit of `width` and `height`.
#' * `dpi`: Forest plot resolution.
#'
#' @export
forest_save <- function(
  x,
  ...,
  filename,
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
