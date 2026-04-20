#' Read Excel files with multiple header rows
#'
#' @description
#' Imports data from modern Excel files (.xlsx, .xlsm, .xlsb), automatically
#' handling multi-row merged headers by collapsing them into single column
#' names. It reads the raw data using `openxlsx2` and then converts types using
#' `readr::type_convert`.
#'
#' @details
#' This function is designed for "messy" Excel tables where headers span
#' multiple rows (often using merged cells). It collapses these rows into a
#' single unique variable name (e.g., "Demographics_Age").
#'
#' **Note:** This function supports modern XML-based Excel formats (.xlsx,
#' .xlsm, .xlsb).
#' It does **not** support the legacy binary .xls format.
#'
#' @inheritParams openxlsx2::wb_load
#' @param sheets ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Sheets to read. Default is `1`, which reads the first sheet.
#' @param n_headers A single integer indicating the number of rows at the top
#'   of the selected data to treat as the header. Defaults to 1.
#' @param sep Separator used to collapse multi-row headers. Defaults to "_".
#' @param simplify Logical. If `TRUE` (default), the function returns a single
#'   data frame if only one sheet is read. If `FALSE`, it always returns a list
#'   of data frames.
#' @inheritDotParams openxlsx2::wb_to_df -sheet -col_names -convert
#'   -fill_merged_cells -types -check_names
#' @return A data frame (if one sheet and `simplify = TRUE`) or a list of data
#'   frames.
#' @export
read_excel_multi_headers <- function(
  file,
  sheets = 1,
  n_headers = 1,
  sep = "_",
  simplify = TRUE,
  ...
) {
  # 1. Argument Validation and Cleaning

  # Capture dots to inspect and filter them
  dots <- rlang::list2(...)

  # List of arguments that we must control internally
  blocked_args <- c(
    "sheet",
    "col_names",
    "convert",
    "fill_merged_cells",
    "types",
    "check_names"
  )

  # Check if user tried to pass any blocked arguments
  if (any(names(dots) %in% blocked_args)) {
    bad_args <- intersect(names(dots), blocked_args)
    cli::cli_warn(c(
      "!" = "The following arguments in {.arg ...} are ignored because they are controlled internally:",
      "*" = "{.pkg {bad_args}}"
    ))

    # Remove blocked arguments from the list so they don't override our settings
    dots <- dots[!names(dots) %in% blocked_args]
  }

  # Strict type checks
  checkmate::assert_count(
    n_headers,
    positive = TRUE,
    .var.name = "n_headers"
  )
  checkmate::assert_string(sep, .var.name = "sep")

  # 2. Workbook Setup

  # Load workbook once. wb_load will throw an error if the file doesn't exist.
  wb <- openxlsx2::wb_load(file)
  all_sheet_names <- openxlsx2::wb_get_sheet_names(wb)

  # 3. Determine sheets to process using tidyselect
  
  # Prepare choice vector for tidyselect (named vector of indices)
  sheet_choices <- rlang::set_names(seq_along(all_sheet_names), all_sheet_names)
  
  # Evaluate selection
  selected_idx <- tidyselect::eval_select(
    rlang::enquo(sheets),
    data = sheet_choices
  )
  
  if (length(selected_idx) == 0) {
    cli::cli_abort("No sheets matched the selection criteria.")
  }
  
  sheets_to_process <- all_sheet_names[selected_idx]

  # 4. Processing Loop

  cleaned_data_list <- purrr::set_names(sheets_to_process) |>
    purrr::map(function(sheet_name) {
      # Use tryCatch to handle errors per-sheet without stopping the whole
      # process
      tryCatch(
        {
          # A. Read Raw Data
          # We use rlang::exec to pass the arguments.
          # We pass the 'dots' list using `!!!` (splice).
          # We DO NOT pass `...` here, because `...` contains the dirty/blocked
          # args.
          raw_df <- rlang::exec(
            openxlsx2::wb_to_df,
            file = wb,
            sheet = sheet_name,
            col_names = FALSE, # Hardcoded overrides
            convert = FALSE,
            fill_merged_cells = TRUE,
            !!!dots, # Cleaned user arguments
            .env = rlang::current_env()
          )

          if (is.null(raw_df)) {
            cli::cli_abort("Sheet is empty.")
          }

          # B. Check Bounds
          if (nrow(raw_df) < n_headers) {
            cli::cli_abort(
              "Not enough rows. Read {nrow(raw_df)}, but header requires {n_headers}."
            )
          }

          # C. Create Headers
          # Slice top N rows, trim whitespace, and collapse merged cells
          new_headers <- raw_df |>
            dplyr::slice(1:n_headers) |>
            dplyr::summarise(dplyr::across(dplyr::everything(), function(col) {
              col[is.na(col)] <- ""
              col <- trimws(col)
              unique_consecutive <- rle(col)$values
              final_parts <- unique_consecutive[unique_consecutive != ""]
              paste(final_parts, collapse = sep)
            })) |>
            unlist(use.names = FALSE)

          # Ensure headers are valid and unique (tidyverse-style name repair)
          new_headers <- vctrs::vec_as_names(
            new_headers,
            repair = "unique",
            quiet = TRUE
          )

          # D. Final Data Construction
          # Slice data body and convert types
          data_df <- raw_df |>
            dplyr::slice((n_headers + 1):dplyr::n()) |>
            purrr::set_names(new_headers)

          suppressMessages(readr::type_convert(data_df))
        },
        error = function(e) {
          # Warn about the failure and return NULL
          cli::cli_warn(c(
            "!" = "Skipping sheet {.val {sheet_name}} due to error.",
            "i" = e$message
          ))
          NULL
        }
      )
    })

  # 5. Finalizing Results

  # Remove sheets that returned NULL (failed)
  cleaned_data_list <- purrr::compact(cleaned_data_list)

  # Check if all sheets failed
  if (length(cleaned_data_list) == 0) {
    cli::cli_abort("All selected sheets failed to import.")
  }

  # Return single data frame if only one sheet AND simplify is requested
  # Note: if user selected multiple sheets but only one succeeded, we still
  # respect simplify=TRUE which seems consistent.
  if (isTRUE(simplify) && length(cleaned_data_list) == 1) {
    cleaned_data_list[[1]]
  } else {
    cleaned_data_list
  }
}
