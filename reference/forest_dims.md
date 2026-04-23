# Calculate forest plot dimensions

Extracts the exact width and height of a `meta` forest plot from its
internal grid layout. This is primarily used to determine the optimal
dimensions for saving the forest plot to disk.

## Usage

``` r
forest_dims(x, ..., units = c("in", "cm", "mm"))
```

## Arguments

- x:

  An object of class `meta` (e.g., from
  [`meta::metacont()`](https://rdrr.io/pkg/meta/man/metacont.html),
  [`meta::metabin()`](https://rdrr.io/pkg/meta/man/metabin.html), or
  [`meta::metagen()`](https://rdrr.io/pkg/meta/man/metagen.html)).

- ...:

  Additional arguments passed on to the underlying `forest` method
  (e.g.,
  [`meta::forest.meta()`](https://rdrr.io/pkg/meta/man/forest.meta.html),
  [`meta::forest.metabind()`](https://rdrr.io/pkg/meta/man/forest.metabind.html),
  [`meta::forest.metacum()`](https://rdrr.io/pkg/meta/man/forest.metacum.html),
  or
  [`meta::forest.metainf()`](https://rdrr.io/pkg/meta/man/forest.metainf.html)).

- units:

  Units of the returned `width` and `height`. One of `"in"`, `"cm"`, or
  `"mm"`. Defaults to `"in"`.

## Value

A named list with elements:

- `width`: The width of the forest plot in the specified units.

- `height`: The height of the forest plot in the specified units.

## Details

Rather than using guesswork or manual row counting, `forest_dims()`
captures the forest plot as a true graphics object and extracts the
precise width and height directly from its underlying structure.

Because it mathematically measures the actual rendered components, it is
highly robust. It works seamlessly with any forest plot configuration.

## Examples

``` r
# Create a simple meta-analysis object
m <- meta::metagen(
  TE = c(0.5, 0.8, 0.3),
  seTE = c(0.2, 0.3, 0.15),
  studlab = c("Study A", "Study B", "Study C")
)

# Get dimensions in inches (default)
forest_dims(m)
#> $width
#>       in 
#> 8.768552 
#> 
#> $height
#>  in 
#> 3.2 
#> 

# Get dimensions in centimetres
forest_dims(m, units = "cm")
#> $width
#>       cm 
#> 22.27212 
#> 
#> $height
#>    cm 
#> 8.128 
#> 
```
