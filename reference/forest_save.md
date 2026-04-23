# Save a forest plot to disk

Saves a `meta` forest plot to disk with dimensions automatically
calculated from the forest plot's internal grid layout via
[`forest_dims()`](https://nouredindarwish.github.io/metaExtra/reference/forest_dims.md).

## Usage

``` r
forest_save(
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
)
```

## Arguments

- x:

  An object of class `meta` (e.g., from
  [`meta::metacont()`](https://rdrr.io/pkg/meta/man/metacont.html),
  [`meta::metabin()`](https://rdrr.io/pkg/meta/man/metabin.html), or
  [`meta::metagen()`](https://rdrr.io/pkg/meta/man/metagen.html)).

- filename:

  File name to create on disk.

- ...:

  Additional arguments passed on to the underlying `forest` method
  (e.g.,
  [`meta::forest.meta()`](https://rdrr.io/pkg/meta/man/forest.meta.html),
  [`meta::forest.metabind()`](https://rdrr.io/pkg/meta/man/forest.metabind.html),
  [`meta::forest.metacum()`](https://rdrr.io/pkg/meta/man/forest.metacum.html),
  or
  [`meta::forest.metainf()`](https://rdrr.io/pkg/meta/man/forest.metainf.html)).

- device:

  Device to use. Can either be a device function (e.g.
  [png](https://rdrr.io/r/grDevices/png.html)), or one of `"eps"`,
  `"ps"`, `"tex"` (pictex), `"pdf"`, `"jpeg"`, `"tiff"`, `"png"`,
  `"bmp"`, `"svg"` or `"wmf"` (Windows only). If `NULL` (default), the
  device is guessed based on the `filename` extension.

- path:

  Path of the directory to save the forest plot to: `path` and
  `filename` are combined to create the fully qualified file name.
  Defaults to the working directory.

- width, height:

  Forest plot size in units expressed by the `units` argument. Defaults
  to `.width` and `.height`, which are the auto-calculated dimensions
  from
  [`forest_dims()`](https://nouredindarwish.github.io/metaExtra/reference/forest_dims.md).
  You can use `.width` and `.height` in expressions (e.g.,
  `height = .height * 1.2`) to adjust the forest plot size. If set to
  `NA`, the size of the current graphics device is used.

- units:

  One of the following units in which the `width` and `height` arguments
  are expressed: `"in"`, `"cm"`, `"mm"` or `"px"`. Defaults to `"in"`.

- dpi:

  Plot resolution. Defaults to `300`.

- device_args:

  A named list of additional arguments passed directly to the underlying
  graphics device (e.g., `list(bg = "transparent", pointsize = 12)`).

## Value

A named list (returned invisibly) with elements:

- `file`: Full file path.

- `width`: Forest plot width.

- `height`: Forest plot height.

- `units`: Units of `width` and `height`.

- `dpi`: Forest plot resolution.

## Details

The auto-calculated dimensions are exposed to the `width` and `height`
arguments as the special variables `.width` and `.height`. You can
reference these in mathematical expressions to easily adjust the forest
plot's size relative to its optimal dimensions:

    # Auto dimensions (default)
    forest_save(m, filename = "plot.png")

    # 20% taller and 2 units wider than the auto-calculated dimensions
    forest_save(m, filename = "plot.png", width = .width + 2, height = .height * 1.2)

    # Explicit override
    forest_save(m, filename = "plot.png", width = 10, height = 8)

## Examples

``` r
m <- meta::metagen(
  TE = c(0.5, 0.8, 0.3),
  seTE = c(0.2, 0.3, 0.15),
  studlab = c("Study A", "Study B", "Study C")
)

# Save with auto-calculated dimensions
tmp <- tempfile(fileext = ".png")
forest_save(m, filename = tmp)

# 20% taller than auto-calculated
tmp2 <- tempfile(fileext = ".pdf")
forest_save(m, filename = tmp2, height = .height * 1.2)

# Explicit dimensions in centimetres
tmp3 <- tempfile(fileext = ".png")
forest_save(m, filename = tmp3, width = 25, height = 15, units = "cm")
```
