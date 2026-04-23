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
  expect_equal(dims_cm$width, dims_in$width * 2.54, tolerance = 0.01)
  expect_equal(dims_cm$height, dims_in$height * 2.54, tolerance = 0.01)
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
  expect_equal(dims_mm$width, dims_cm$width * 10, tolerance = 0.01)
  expect_equal(dims_mm$height, dims_cm$height * 10, tolerance = 0.01)
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

test_that("forest_dims works with NSE arguments in ...", {
  m <- meta::metagen(
    TE = c(0.5, 0.8, 0.3),
    seTE = c(0.2, 0.3, 0.15),
    studlab = c("Study A", "Study B", "Study C")
  )

  # sortvar = TE uses NSE — should not error
  expect_no_error(forest_dims(m, sortvar = TE))
  expect_no_error(forest_dims(m, sortvar = -TE))

  # Result should still be a valid dimensions list
  dims <- forest_dims(m, sortvar = TE)
  expect_type(dims, "list")
  expect_named(dims, c("width", "height"))
  expect_gt(dims$width, 0)
  expect_gt(dims$height, 0)
})

test_that("forest_dims resolves NSE in the caller's environment", {
  m <- meta::metagen(
    TE = c(0.5, 0.8, 0.3),
    seTE = c(0.2, 0.3, 0.15),
    studlab = c("Study A", "Study B", "Study C")
  )

  # A variable defined here (test scope) should be found — it does NOT
  # exist in the meta object, so this can only work if eval starts in
  # the user's environment
  my_order <- c(3, 1, 2)
  expect_no_error(forest_dims(m, sortvar = my_order))

  # A variable defined inside a wrapper function should also be found
  wrapper <- function(meta_obj) {
    local_order <- c(2, 3, 1)
    forest_dims(meta_obj, sortvar = local_order)
  }
  expect_no_error(wrapper(m))

  # Prove that meta-object columns take priority: define a local `TE`

  # with the WRONG length (4 values vs 3 studies). If our local `TE`
  # leaked through, meta::forest would throw a length-mismatch error.
  # No error proves the meta-object's `TE` (length 3) was used.
  TE <- c(1, 2, 3, 4)
  expect_no_error(forest_dims(m, sortvar = TE))
})

test_that("forest_dims does not leak internal variables into NSE scope", {
  m <- meta::metagen(
    TE = c(0.5, 0.8, 0.3),
    seTE = c(0.2, 0.3, 0.15),
    studlab = c("Study A", "Study B", "Study C")
  )

  # `grid_unit` exists inside forest_dims() (a local variable set to
  # "inches"). If eval() accidentally ran in the function's frame, this
  # would silently resolve. An error proves eval stays in the user's env.
  expect_error(forest_dims(m, sortvar = grid_unit), "not found")
})
