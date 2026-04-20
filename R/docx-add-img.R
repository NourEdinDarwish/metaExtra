#' Add an external image to a Word document
#'
#' Adds an external image to an `officer` Word document, with options for
#' sizing, captioning, and auto-numbering. It automatically resizes the image
#' if it exceeds the document's page margins.
#'
#' @param x An `officer::read_docx` object.
#' @param src Path to the image file.
#' @param width Width of the image.
#' @param height Height of the image.
#' @param units Unit for width/height. One of `"in"`, `"cm"`, `"mm"`, `"px"`.
#'   Default is `"in"`.
#' @param dpi Plot resolution (dots per inch). Used only if `units = "px"`.
#'   Default is 300.
#' @param caption The caption text (string).
#' @param caption_pos Position of the caption relative to the image. `"above"`
#'   or `"below"`.
#' @param autonum An `officer::run_autonum` object for figure numbering.
#' @param fp_text An `officer::fp_text` object for styling caption text.
#' @param fp_par An `officer::fp_par` object for styling the paragraph.
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
  fp_text = NULL,
  fp_par = NULL
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
  checkmate::assert_class(fp_text, "fp_text", null.ok = TRUE, .var.name = "fp_text")
  checkmate::assert_class(fp_par, "fp_par", null.ok = TRUE, .var.name = "fp_par")

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
    officer::ftext(caption, prop = fp_text)
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

  fpar_obj <- if (is.null(fp_par)) {
    rlang::inject(officer::fpar(!!!chunks))
  } else {
    rlang::inject(officer::fpar(!!!chunks, fp_p = !!fp_par))
  }

  # 7. Add to Document
  x <- officer::body_add_fpar(x, fpar_obj)
  # TODO want to know if I should use fpar here or make options, currently I
  # think it just use default options of word for this line which is a good
  # thing, also if add option for number of lines Add empty line for spacing
  x <- officer::body_add_par(x, "")

  x
}
