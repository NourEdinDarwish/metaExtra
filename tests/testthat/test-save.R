test_that("save_plot works with function plot", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  result <- save_plot(
    function() plot(1:10),
    tmp,
    width = 5,
    height = 4
  )

  expect_true(file.exists(tmp))
  expect_type(result, "list")
  expect_equal(unname(result$width), 5)
  expect_equal(unname(result$height), 4)
  expect_equal(result$units, "in")
})

test_that("save_plot works with ggplot object", {
  skip_if_not_installed("ggplot2")
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp))
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  result <- save_plot(p, tmp, width = 5, height = 5)
  expect_true(file.exists(tmp))
  expect_equal(result$file, tmp)
  expect_equal(result$dpi, 300)
})

test_that("save_plot works with grob object", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  g <- grid::circleGrob()
  result <- save_plot(g, tmp, width = 5, height = 5)
  expect_true(file.exists(tmp))
  expect_equal(result$file, tmp)
})

test_that("save_plot works with list of grobs", {
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp))
  grobs <- list(grid::circleGrob(), grid::rectGrob())
  result <- save_plot(grobs, tmp, width = 5, height = 5)
  expect_true(file.exists(tmp))
})

test_that("save_plot restores previous graphics device", {
  grDevices::png(tempfile())
  grDevices::png(tempfile())
  on.exit(graphics.off(), add = TRUE)

  old_dev <- grDevices::dev.cur()
  save_plot(
    function() plot(1),
    tempfile(fileext = ".png"),
    width = 5,
    height = 5
  )
  expect_identical(old_dev, grDevices::dev.cur())
})

test_that("save_plot respects units argument", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  result <- save_plot(
    function() plot(1),
    tmp,
    width = 10,
    height = 10,
    units = "cm"
  )
  expect_equal(result$units, "cm")
  expect_equal(unname(result$width), 10)
  expect_equal(unname(result$height), 10)
})

test_that("save_plot works with px units", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  result <- save_plot(
    function() plot(1),
    tmp,
    width = 800,
    height = 600,
    units = "px",
    dpi = 100
  )
  expect_equal(result$units, "px")
  expect_equal(unname(result$width), 800)
  expect_equal(unname(result$height), 600)
  expect_equal(result$dpi, 100)
})

# validate_path -----------------------------------------------------------

test_that("validate_path creates missing directory", {
  tmp_dir <- tempfile()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  tmp_file <- "test.png"

  suppressMessages({
    result <- save_plot(
      function() plot(1),
      filename = tmp_file,
      path = tmp_dir,
      width = 4,
      height = 4
    )
  })
  expect_true(file.exists(file.path(tmp_dir, tmp_file)))
})

test_that("validate_path errors on bad filename", {
  expect_error(
    save_plot(function() plot(1), filename = 123),
    "must be a string"
  )
})

test_that("validate_path warns on multiple filenames", {
  withr::with_tempdir({
    expect_warning(
      save_plot(
        function() plot(1),
        filename = c("a.png", "b.png"),
        width = 4,
        height = 4
      ),
      "must have length 1"
    )
  })
})

test_that("validate_path errors on bad path", {
  expect_error(
    save_plot(function() plot(1), filename = "a.png", path = 123),
    "must be a string"
  )
})

# plot_dim ----------------------------------------------------------------

test_that("plot_dim uses 7x7 if no graphics device open", {
  suppressMessages(expect_equal(plot_dim(), c(7, 7)))
})

test_that("plot_dim uses current device size", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  grDevices::png(tmp, width = 10, height = 10, units = "in", res = 300)
  on.exit(capture.output(grDevices::dev.off()), add = TRUE)

  expect_message(out <- plot_dim(), "10 x 10")
  expect_equal(out, c(10, 10))
})

test_that("plot_dim informs when dimensions are guessed", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  expect_message(
    save_plot(function() plot(1), tmp),
    "Saving"
  )
})

# validate_device ---------------------------------------------------------

test_that("validate_device errors on no extension and no device", {
  tmp <- tempfile()
  expect_error(
    save_plot(function() plot(1), tmp),
    "Either supply `filename` with a file"
  )
})

test_that("validate_device errors on invalid device argument", {
  tmp <- tempfile()
  expect_error(
    save_plot(function() plot(1), tmp, device = 123),
    "must be a string or function"
  )
})

test_that("validate_device errors on unknown device string", {
  tmp <- tempfile()
  expect_error(
    save_plot(function() plot(1), tmp, device = "unknown_dev"),
    "Unknown graphics device"
  )
})

test_that("validate_device accepts device string for bad extension", {
  tmp <- tempfile(fileext = ".test")
  on.exit(unlink(tmp))
  save_plot(function() plot(1), tmp, device = "png", width = 4, height = 4)
  expect_true(file.exists(tmp))
})

test_that("validate_device accepts device function", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  save_plot(function() plot(1), tmp, device = png, width = 4, height = 4)
  expect_true(file.exists(tmp))
})

test_that("validate_device works with eps, pdf, svg", {
  tmp_eps <- tempfile(fileext = ".eps")
  on.exit(unlink(tmp_eps), add = TRUE)
  save_plot(function() plot(1), tmp_eps, width = 4, height = 4)
  expect_true(file.exists(tmp_eps))

  tmp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_pdf), add = TRUE)
  save_plot(function() plot(1), tmp_pdf, width = 4, height = 4)
  expect_true(file.exists(tmp_pdf))

  skip_if_not_installed("svglite")
  tmp_svg <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp_svg), add = TRUE)
  save_plot(function() plot(1), tmp_svg, width = 4, height = 4)
  expect_true(file.exists(tmp_svg))
})

test_that("device as custom function gets file argument", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  custom_dev <- function(file, width, height, ...) {
    grDevices::png(filename = file, width = 800, height = 800, ...)
  }

  save_plot(function() plot(1), tmp, device = custom_dev, width = 4, height = 4)
  expect_true(file.exists(tmp))
})

# absorb_grdevice_args ----------------------------------------------------

test_that("absorb_grdevice_args warns on type/antialias", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  expect_warning(
    save_plot(function() plot(1), tmp, type = "cairo", width = 4, height = 4),
    "Using ragg device as default"
  )
})
