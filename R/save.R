#' Save a plot to a file
#'
#' Adapted from [ggplot2::ggsave()]. A general-purpose plot-saving function that
#' works with any R plotting system. Accepts `ggplot` objects, `grid` objects,
#' or any plotting call wrapped in a function. Unlike [ggplot2::ggsave()], which
#' only supports `ggplot` and `grid` objects, `save_plot()` can capture any
#' plotting call (e.g., base R) by wrapping it in a function (e.g., `function()
#' plot(1:10)`).
#'
#' @param plot Plot to save. A `ggplot` object, a `grid` object, or any plotting
#'   call (e.g., base R) wrapped in a function (e.g., `function() plot(1:10)`).
#' @param filename File name to create on disk.
#' @param device Device to use. Can either be a device function (e.g. [png]), or
#'   one of `"eps"`, `"ps"`, `"tex"` (pictex), `"pdf"`, `"jpeg"`, `"tiff"`,
#'   `"png"`, `"bmp"`, `"svg"` or `"wmf"` (Windows only). If `NULL` (default),
#'   the device is guessed based on the `filename` extension.
#' @param path Path of the directory to save plot to: `path` and `filename` are
#'   combined to create the fully qualified file name. Defaults to the working
#'   directory.
#' @param width,height Plot size in units expressed by the `units` argument. If
#'   not supplied, uses the size of the current graphics device.
#' @param units One of the following units in which the `width` and `height`
#'   arguments are expressed: `"in"`, `"cm"`, `"mm"` or `"px"`.
#' @param dpi Plot resolution.
#' @param ... Other arguments passed on to the graphics device function, as
#'   specified by `device`.
#'
#' @return A named list (returned invisibly) with elements:
#' * `file`: Full file path.
#' * `width`: Plot width.
#' * `height`: Plot height.
#' * `units`: Unit of `width` and `height`.
#' * `dpi`: Plot resolution.
#'
#' @seealso [ggplot2::ggsave()]
#'
#' @noRd
save_plot <- function(
  plot,
  filename,
  device = NULL,
  path = NULL,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  ...
) {
  filename <- validate_path(path, filename)

  dev <- validate_device(device, filename, dpi = dpi)
  dim <- plot_dim(c(width, height), units = units, dpi = dpi)

  old_dev <- grDevices::dev.cur()
  dev(filename = filename, width = dim[1], height = dim[2], ...)
  on.exit(utils::capture.output({
    grDevices::dev.off()
    # restore old device unless null device
    if (old_dev > 1) grDevices::dev.set(old_dev)
  }))

  if (is.function(plot)) {
    plot()
  } else {
    if (!rlang::is_bare_list(plot)) {
      plot <- list(plot)
    }
    lapply(plot, grid::grid.draw)
  }

  # Convert back to user's units for the return value
  dim_user <- from_inches(dim, units, dpi = dpi)

  invisible(list(
    file   = filename,
    width  = dim_user[1],
    height = dim_user[2],
    units  = units,
    dpi    = dpi
  ))
}

validate_path <- function(path, filename, call = rlang::caller_env()) {
  if (length(filename) > 1 && is.character(filename)) {
    cli::cli_warn(
      c(
        "{.arg filename} must have length 1, not {length(filename)}.",
        "!" = "Only the first, {.file {filename[1]}}, will be used."
      ),
      call = call
    )
    filename <- filename[1]
  }

  if (!checkmate::test_string(filename, min.chars = 1)) {
    cli::cli_abort(
      paste0(
        "{.arg filename} must be a string, ",
        "not {.obj_type_friendly {filename}}."
      ),
      call = call
    )
  }
  if (!checkmate::test_string(path, null.ok = TRUE)) {
    cli::cli_abort(
      paste0(
        "{.arg path} must be a string or {.code NULL}, ",
        "not {.obj_type_friendly {path}}."
      ),
      call = call
    )
  }

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  } else {
    path <- dirname(filename)
  }

  # Happy path: directory exists
  if (dir.exists(path)) {
    return(filename)
  }

  # Always attempt to create directory
  dir.create(path, recursive = TRUE)

  # Check if creation was successful
  if (dir.exists(path)) {
    cli::cli_alert_success("Created directory: {.path {path}}.")
    return(filename)
  }

  # Only strictly error if creation failed (permissions, invalid path, etc.)
  cli::cli_abort(
    c(
      "Cannot find or create directory {.path {path}}."
    ),
    call = call
  )
}

plot_dim <- function(
  dim = c(NA, NA),
  units = "in",
  dpi = 300,
  call = rlang::caller_env()
) {
  units <- rlang::arg_match0(units, c("in", "cm", "mm", "px"))

  dim <- to_inches(dim, units, dpi = dpi)

  if (anyNA(dim)) {
    if (length(grDevices::dev.list()) == 0) {
      default_dim <- c(7, 7)
    } else {
      default_dim <- grDevices::dev.size()
    }
    dim[is.na(dim)] <- default_dim[is.na(dim)]
    dim_f <- prettyNum(from_inches(dim, units, dpi = dpi), digits = 3) # nolint

    cli::cli_inform("Saving {dim_f[1]} x {dim_f[2]} {units} image")
  }

  dim
}

to_inches <- function(x, units, dpi = 300) {
  x / c(`in` = 1, cm = 2.54, mm = 2.54 * 10, px = dpi)[units]
}

from_inches <- function(x, units, dpi = 300) {
  x * c(`in` = 1, cm = 2.54, mm = 2.54 * 10, px = dpi)[units]
}

validate_device <- function(
  device,
  filename = NULL,
  dpi = 300,
  call = rlang::caller_env()
) {
  force(filename)
  force(dpi)

  if (is.function(device)) {
    args <- formals(device)
    call_args <- list()
    if ("file" %in% names(args)) {
      call_args$file <- filename
      call_args["filename"] <- list(NULL)
    }
    if ("res" %in% names(args)) {
      call_args$res <- dpi
    }
    if ("units" %in% names(args)) {
      call_args$units <- "in"
    }
    dev <- function(...) {
      args <- utils::modifyList(list(...), call_args)
      rlang::inject(device(!!!args))
    }
    return(dev)
  }

  eps <- function(filename, ...) {
    grDevices::postscript(
      file = filename,
      ...,
      onefile = FALSE,
      horizontal = FALSE,
      paper = "special"
    )
  }
  if (requireNamespace("ragg", quietly = TRUE)) {
    png_dev <- absorb_grdevice_args(ragg::agg_png)
    jpeg_dev <- absorb_grdevice_args(ragg::agg_jpeg)
    tiff_dev <- absorb_grdevice_args(ragg::agg_tiff)
  } else {
    png_dev <- grDevices::png
    jpeg_dev <- grDevices::jpeg
    tiff_dev <- grDevices::tiff
  }
  devices <- list(
    eps = eps,
    ps = eps,
    tex = function(filename, ...) grDevices::pictex(file = filename, ...),
    pdf = function(filename, ..., version = "1.4") {
      grDevices::pdf(file = filename, ..., version = version)
    },
    svg = function(filename, ...) {
      rlang::check_installed("svglite", reason = "to save as SVG.")
      svglite::svglite(file = filename, ...)
    },
    # win.metafile() doesn't have `bg` arg so we need to absorb it before
    # passing `...`
    emf = function(..., bg = NULL) grDevices::win.metafile(...),
    wmf = function(..., bg = NULL) grDevices::win.metafile(...),
    png = function(...) png_dev(..., res = dpi, units = "in"),
    jpg = function(...) jpeg_dev(..., res = dpi, units = "in"),
    jpeg = function(...) jpeg_dev(..., res = dpi, units = "in"),
    bmp = function(...) grDevices::bmp(..., res = dpi, units = "in"),
    tiff = function(...) tiff_dev(..., res = dpi, units = "in"),
    tif = function(...) tiff_dev(..., res = dpi, units = "in")
  )

  if (is.null(device)) {
    device <- tolower(tools::file_ext(filename))
    if (identical(device, "")) {
      cli::cli_abort(
        c(
          "Can't save to {filename}.",
          i = paste0(
            "Either supply {.arg filename} with a file extension ",
            "or supply {.arg device}."
          )
        ),
        call = call
      )
    }
  }

  if (!is.character(device) || length(device) != 1) {
    cli::cli_abort(
      paste0(
        "{.arg device} must be a string or function, ",
        "not {.obj_type_friendly {device}}."
      ),
      call = call
    )
  }

  dev <- devices[[device]]
  if (is.null(dev)) {
    cli::cli_abort("Unknown graphics device {.val {device}}", call = call)
  }
  dev
}

absorb_grdevice_args <- function(f) {
  function(..., type, antialias) {
    if (!missing(type) || !missing(antialias)) {
      cli::cli_warn(
        paste0(
          "Using ragg device as default. ",
          "Ignoring {.arg type} and {.arg antialias} arguments"
        )
      )
    }
    f(...)
  }
}
