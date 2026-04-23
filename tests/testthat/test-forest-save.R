# forest_save -------------------------------------------------------------

test_that("forest_save saves a file with auto dimensions", {
  m <- meta::metagen(
    TE = c(0.5, 0.8, 0.3),
    seTE = c(0.2, 0.3, 0.15),
    studlab = c("Study A", "Study B", "Study C")
  )

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))

  result <- forest_save(m, tmp)

  expect_true(file.exists(tmp))
  expect_type(result, "list")
  expect_named(result, c("file", "width", "height", "units", "dpi"))
  expect_equal(result$file, tmp)
  expect_gt(result$width, 0)
  expect_gt(result$height, 0)
})

test_that("forest_save auto dimensions match forest_dims", {
  m <- meta::metagen(
    TE = c(0.5, 0.8),
    seTE = c(0.2, 0.3),
    studlab = c("Study A", "Study B")
  )

  dims <- forest_dims(m)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp))

  result <- forest_save(m, tmp)

  expect_equal(result$width, dims$width, tolerance = 0.01)
  expect_equal(result$height, dims$height, tolerance = 0.01)
})

test_that("forest_save supports .width/.height expressions", {
  m <- meta::metagen(
    TE = c(0.5, 0.8),
    seTE = c(0.2, 0.3),
    studlab = c("Study A", "Study B")
  )

  dims <- forest_dims(m)
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp))

  result <- forest_save(m, tmp, width = .width + 2, height = .height * 1.5)

  expect_equal(result$width, dims$width + 2, tolerance = 0.01)
  expect_equal(result$height, dims$height * 1.5, tolerance = 0.01)
})

test_that("forest_save respects explicit width/height", {
  m <- meta::metagen(
    TE = c(0.5),
    seTE = c(0.2),
    studlab = c("Study A")
  )

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))

  result <- forest_save(m, tmp, width = 10, height = 8)

  expect_equal(result$width, 10)
  expect_equal(result$height, 8)
})

test_that("forest_save respects units argument", {
  m <- meta::metagen(
    TE = c(0.5),
    seTE = c(0.2),
    studlab = c("Study A")
  )

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))

  result <- forest_save(m, tmp, width = 20, height = 15, units = "cm")

  expect_equal(result$units, "cm")
  expect_equal(result$width, 20)
  expect_equal(result$height, 15)
})

test_that("forest_save works with different devices", {
  m <- meta::metagen(
    TE = c(0.5),
    seTE = c(0.2),
    studlab = c("Study A")
  )

  tmp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_pdf), add = TRUE)
  result <- forest_save(m, tmp_pdf)
  expect_true(file.exists(tmp_pdf))

  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)
  result <- forest_save(m, tmp_png)
  expect_true(file.exists(tmp_png))
})

test_that("forest_save passes ... to forest()", {
  m <- meta::metagen(
    TE = c(0.5, 0.8, 0.3),
    seTE = c(0.2, 0.3, 0.15),
    studlab = c("Study A", "Study B", "Study C")
  )

  tmp1 <- tempfile(fileext = ".pdf")
  tmp2 <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(tmp1, tmp2)))

  r1 <- forest_save(m, tmp1)
  r2 <- forest_save(m, tmp2, overall = FALSE)

  # Different forest options should yield different file sizes or dimensions
  expect_false(identical(r1$height, r2$height))
})
