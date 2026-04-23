# forest_dims -------------------------------------------------------------

test_that("forest_dims returns a list with width and height", {
  m <- meta::metagen(
    TE = c(0.5, 0.8, 0.3),
    seTE = c(0.2, 0.3, 0.15),
    studlab = c("Study A", "Study B", "Study C")
  )

  dims <- forest_dims(m)

  expect_type(dims, "list")
  expect_named(dims, c("width", "height"))
  expect_gt(dims$width, 0)
  expect_gt(dims$height, 0)
})

test_that("forest_dims returns inches by default", {
  m <- meta::metagen(
    TE = c(0.5, 0.8),
    seTE = c(0.2, 0.3),
    studlab = c("Study A", "Study B")
  )

  dims_in <- forest_dims(m)
  dims_cm <- forest_dims(m, units = "cm")

  # cm values should be ~2.54x larger than inches
  expect_equal(
    unname(dims_cm$width),
    unname(dims_in$width) * 2.54,
    tolerance = 0.01
  )
  expect_equal(
    unname(dims_cm$height),
    unname(dims_in$height) * 2.54,
    tolerance = 0.01
  )
})

test_that("forest_dims respects units argument", {
  m <- meta::metagen(
    TE = c(0.5, 0.8),
    seTE = c(0.2, 0.3),
    studlab = c("Study A", "Study B")
  )

  dims_cm <- forest_dims(m, units = "cm")
  dims_mm <- forest_dims(m, units = "mm")

  # mm values should be 10x larger than cm
  expect_equal(
    unname(dims_mm$width),
    unname(dims_cm$width) * 10,
    tolerance = 0.01
  )
  expect_equal(
    unname(dims_mm$height),
    unname(dims_cm$height) * 10,
    tolerance = 0.01
  )
})

test_that("forest_dims errors on invalid units", {
  m <- meta::metagen(
    TE = c(0.5),
    seTE = c(0.2),
    studlab = c("Study A")
  )

  expect_error(forest_dims(m, units = "px"))
  expect_error(forest_dims(m, units = "invalid"))
})

test_that("forest_dims passes ... to forest()", {
  m <- meta::metagen(
    TE = c(0.5, 0.8, 0.3),
    seTE = c(0.2, 0.3, 0.15),
    studlab = c("Study A", "Study B", "Study C")
  )

  # Using a different layout option should produce different dimensions
  dims_default <- forest_dims(m)
  dims_no_overall <- forest_dims(m, overall = FALSE)

  # Removing elements should change the height
  expect_false(identical(dims_default$height, dims_no_overall$height))
})

test_that("forest_dims restores previous graphics device", {
  grDevices::png(tempfile())
  on.exit(graphics.off(), add = TRUE)

  old_dev <- grDevices::dev.cur()

  m <- meta::metagen(
    TE = c(0.5),
    seTE = c(0.2),
    studlab = c("Study A")
  )
  forest_dims(m)

  expect_identical(old_dev, grDevices::dev.cur())
})
