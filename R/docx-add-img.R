#' Add an image to a Word document
#'
#' Adds an image to a Word document with optional captioning and auto-numbering.
#' Images larger than the page margins are automatically scaled down to fit
#' within the available page space while preserving their aspect ratio.
#'
#' @param x An `rdocx` object, created with [officer::read_docx()].
#' @param src Path to the image file.
#' @param width,height Image size in units expressed by the `units` argument.
#' @param units One of the following units in which the `width` and `height`
#'   arguments are expressed: `"in"`, `"cm"`, `"mm"` or `"px"`. Default is
#'   `"in"`.
#' @param dpi Plot resolution. Only used when `units = "px"`. Default is 300.
#' @param caption The text to display as the image caption.
#' @param caption_pos Position of the caption relative to the image. One of
#'   `"below"` or `"above"`. Default is `"below"`.
#' @param autonum Auto-numbering (e.g., to automatically generate "Figure 1: "),
#'   created with [officer::run_autonum()].
#' @param fp_t Text formatting properties for the caption text, created with
#'   [officer::fp_text()]. Note that this does not affect the styling of the
#'   auto-numbering (use the `prop` argument in [officer::run_autonum()] for
#'   that).
#' @param fp_p Paragraph formatting properties applied to the entire paragraph
#'   (image, caption, and auto-numbering), created with [officer::fp_par()].
#'
#' @return The modified `rdocx` object.
#'
#' @export
docx_add_img <- function(
  x,
  src,
  width,
  height,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  caption = NULL,
  caption_pos = c("below", "above"),
  autonum = NULL,
  fp_t = NULL,
  fp_p = NULL
) {
  # 1. Validation
  # Core Arguments
  checkmate::assert_class(x, "rdocx", .var.name = "x")
  checkmate::assert_string(src, min.chars = 1, .var.name = "src")
  checkmate::assert_file_exists(src, .var.name = "src")

  # Dimensions & Units #nolint
  # Allow NA - will be resolved using device size or defaults (like save_plot)
  checkmate::assert_number(
    width,
    lower = 0,
    finite = TRUE,
    na.ok = TRUE,
    .var.name = "width"
  )
  checkmate::assert_number(
    height,
    lower = 0,
    finite = TRUE,
    na.ok = TRUE,
    .var.name = "height"
  )
  units <- rlang::arg_match0(units, c("in", "cm", "mm", "px"))
  checkmate::assert_number(dpi, lower = 0, finite = TRUE, .var.name = "dpi")

  # Caption & Content #nolint
  checkmate::assert_string(caption, null.ok = TRUE, .var.name = "caption")
  caption_pos <- rlang::arg_match0(caption_pos, c("below", "above"))

  # Styling & Metadata Objects #nolint
  checkmate::assert_class(
    autonum,
    "run_autonum",
    null.ok = TRUE,
    .var.name = "autonum"
  )
  checkmate::assert_class(fp_t, "fp_text", null.ok = TRUE, .var.name = "fp_t")
  checkmate::assert_class(fp_p, "fp_par", null.ok = TRUE, .var.name = "fp_p")

  # 2. Handle NA dimensions and convert to inches (same logic as plot_dim)
  dim <- c(width, height)
  dim <- to_inches(dim, units, dpi = dpi)

  if (anyNA(dim)) {
    if (length(grDevices::dev.list()) == 0) {
      default_dim <- c(7, 7)
    } else {
      default_dim <- grDevices::dev.size()
    }
    dim[is.na(dim)] <- default_dim[is.na(dim)]
  }

  width_in <- dim[1]
  height_in <- dim[2]

  # 3. Get document dimensions and calculate available space
  doc_dims <- officer::docx_dim(x)

  # Available width = Page Width - Left - Right
  avail_width <- doc_dims$page[["width"]] -
    doc_dims$margins[["left"]] -
    doc_dims$margins[["right"]]

  # Available height = Page Height - Top - Bottom
  # Note: header/footer in $margins are distances from edge to header/footer,
  # but margin.top/margin.bottom determine the main text body limits.
  avail_height <- doc_dims$page[["height"]] -
    doc_dims$margins[["top"]] -
    doc_dims$margins[["bottom"]]

  # 4. Auto-resize if necessary (maintaining aspect ratio)
  # Calculate scaling factor to fit within available dimensions
  # We use min(1, ...) to ensure we only scale down, not up
  width_ratio <- avail_width / width_in
  height_ratio <- avail_height / height_in

  scale_factor <- min(width_ratio, height_ratio, 1)

  width_in <- width_in * scale_factor
  height_in <- height_in * scale_factor

  # 5. Prepare Content
  # Create caption content
  caption_chunk <- if (!is.null(caption)) {
    officer::ftext(caption, prop = fp_t)
  } else {
    NULL
  }

  # Create image content
  img_chunk <- officer::external_img(
    src = src,
    width = width_in,
    height = height_in
  )

  # 6. Assemble Paragraph
  raw_chunks <- if (caption_pos == "above") {
    list(autonum, caption_chunk, img_chunk)
  } else {
    list(img_chunk, autonum, caption_chunk)
  }

  # Remove NULLs
  chunks <- purrr::compact(raw_chunks)

  fpar_obj <- if (is.null(fp_p)) {
    rlang::inject(officer::fpar(!!!chunks))
  } else {
    rlang::inject(officer::fpar(!!!chunks, fp_p = !!fp_p))
  }

  # 7. Add to Document
  x <- officer::body_add_fpar(x, fpar_obj)
  # TODO want to know if I should use fpar here or make options, currently I
  # think it just use default options of word for this line which is a good
  # thing, also if add option for number of lines Add empty line for spacing
  x <- officer::body_add_par(x, "")

  x
}
